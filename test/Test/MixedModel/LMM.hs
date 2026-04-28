{-# LANGUAGE BangPatterns #-}
-- | Step 1 of the tier-3 mixed-model extension.
--
-- Same spring-EM algorithm as 'springREML': iterate the law-of-total-variance
-- update
--
--     τ²_new = (1/q) Σ_k  [ û_k²  +  Var(û_k | y, θ) ]
--     σ²_new = (1/(n-p)) [ rᵀr  +  σ² · tr(Z C_uu Zᵀ) ]
--
-- where r = y − Xβ̂ − Zû, C_uu = (ZᵀR⁻¹Z + G⁻¹)⁻¹ (the inner-problem inverse
-- bottom-right block).
--
-- Inner step: solve the spring network (= the mixed-model equations) at the
-- current (τ², σ²_ε). Same as the existing solver — every observation is an
-- obs-spring (hardness 1/σ²_ε) pulling  y_i  toward  X_iβ + Z_iu, every random
-- effect is a shrinkage spring (hardness 1/τ²) pulling u_k toward 0. Total
-- energy  U = ½(y − Xβ − Zu)ᵀR⁻¹(y − Xβ − Zu) + ½uᵀG⁻¹u  is minimized at the
-- MME solution.
--
-- What this generalizes over the existing NMA spring solver:
--   * X is an arbitrary fixed-effects design matrix (not only treatment dummies)
--   * σ²_ε is estimated (not known per arm from reported variances)
--   * Single grouping, scalar τ² (tier 1 G). Tiers 2/3 are the next steps.
module Test.MixedModel.LMM
  ( LMMData(..)
  , LMMFit(..)
  , Spring(..), SpringKind(..)
  , mkLMMFromRows
  , fitLMM
  , extractSprings
  , summarizeSprings
  , emStepDiag
  -- tier 3: vector random effects with r×r covariance Σ per group
  , LMMFitRE(..)
  , mkLMMVecRE
  , fitLMMRE
  -- logistic GLMM (tier 1 structure: scalar τ² per grouping)
  , GLMMFit(..)
  , fitLogisticGLMM
  ) where

import Prelude hiding ((<>))
import Data.List (nub, elemIndex, foldl')
import Data.Maybe (fromJust)
import Test.MixedModel.SpringNet
  ( buildSpringNetwork, buildSpringNetworkVecRE, buildSpringNetworkIRLS
  , solveSpringNetworkWithVar )
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra
  ( Vector, Matrix
  , tr, inv, ident
  , size, cols
  , fromList, fromLists, toList
  , (><), (!), (<>), (#>), (<.>)
  )

data LMMData = LMMData
  { lmmY           :: Vector Double   -- ^ n responses
  , lmmX           :: Matrix Double   -- ^ n × p fixed-effects design
  , lmmZ           :: Matrix Double   -- ^ n × q random-effects design
  , lmmGroupLabels :: [Int]           -- ^ unique grouping labels, length J
  , lmmGroupSize   :: Int             -- ^ r = # r.e. per group (1 = tier 1)
  } deriving Show

data LMMFit = LMMFit
  { fitBeta    :: Vector Double   -- ^ p fixed effects
  , fitU       :: Vector Double   -- ^ q BLUPs
  , fitVarU    :: Vector Double   -- ^ q posterior variances of BLUPs
  , fitTau2    :: Double          -- ^ random-effect variance
  , fitSigma2E :: Double          -- ^ residual variance
  , fitPath    :: [(Double, Double)]  -- ^ EM trajectory (τ², σ²_ε)
  , fitIters   :: Int
  } deriving Show

data SpringKind = Observation | Shrinkage
  deriving (Eq, Show)

data Spring = Spring
  { spKind     :: SpringKind
  , spLabel    :: String
  , spHardness :: Double
  , spLength   :: Double   -- signed length at equilibrium
  , spEnergy   :: Double   -- ½ k ℓ²
  } deriving Show

-- | Build LMMData from row-level data.  Same constructor handles aggregate
-- NMA (one row per arm) and IPD (one row per patient).
mkLMMFromRows
  :: [Double]    -- ^ responses
  -> [[Double]]  -- ^ covariate rows (may be empty)
  -> Bool        -- ^ prepend intercept column?
  -> [Int]       -- ^ grouping factor (one int per row)
  -> LMMData
mkLMMFromRows ys xsRows addIntcpt grps =
  let n = length ys
      xRow r = (if addIntcpt then [1.0] else []) ++ r
      xRows' = if null xsRows
                 then replicate n (if addIntcpt then [1.0] else [])
                 else map xRow xsRows
      p = length (head xRows')
      x = (n >< p) (concat xRows')
      uniqGs = nub grps
      z = fromLists [[if g == grp then 1.0 else 0.0 | g <- uniqGs] | grp <- grps]
  in LMMData (fromList ys) x z uniqGs 1

-- | Inner solve: builds an explicit SpringNetwork from (y, X, Z) at the
-- current (σ²_ε, τ²) and solves it.  Equivalent to the dense MME solve
-- but uses the spring-network path — every entry of A and b comes from
-- a spring's rank-1 contribution.
--
-- Returns (β̂, û, Var(û | y)) where Var(û | y) is the full q × q posterior
-- covariance (REML-adjusted via the Schur block structure of A⁻¹).
innerSolve :: LMMData -> Double -> Double -> (Vector Double, Vector Double, Matrix Double)
innerSolve lmm tau2 sigma2E =
  let y = lmmY lmm
      x = lmmX lmm
      z = lmmZ lmm
      p = cols x
      q = cols z
      net = buildSpringNetwork y x z sigma2E tau2
      (xSol, varU) = solveSpringNetworkWithVar net
      beta = LA.subVector 0 p xSol
      u    = LA.subVector p q xSol
  in (beta, u, varU)

-- | One spring-EM iteration.  Law-of-total-variance updates for (τ², σ²_ε):
--
--   τ²_new  = (1/q)     [ ûᵀû  +  tr(Var(û)) ]
--   σ²_new  = (1/(n−p)) [ rᵀr  +  tr(P_X · Z Var(û) Zᵀ) ]
--
-- where Var(û) is the full q×q posterior covariance and P_X = I − X(XᵀX)⁻¹Xᵀ
-- is the projector onto the orthogonal complement of X.  The P_X correction
-- is what makes this REML rather than ML — it removes the contribution of
-- the Z-columns that already lie in the span of X.
emStep :: LMMData -> (Double, Double) -> (Double, Double, Vector Double, Vector Double, Matrix Double)
emStep lmm (tau2, sigma2E) =
  let y = lmmY lmm
      x = lmmX lmm
      z = lmmZ lmm
      n = size y
      p = cols x
      q = cols z
      (beta, u, varU) = innerSolve lmm tau2 sigma2E
      uSq        = u <.> u
      trVarU     = LA.sumElements (LA.takeDiag varU)
      tau2New    = (uSq + trVarU) / fromIntegral q
      resid      = y - x #> beta - z #> u
      rss        = resid <.> resid
      ztz        = tr z <> z
      trZVZ      = LA.sumElements (LA.takeDiag (ztz <> varU))
      -- REML projection correction:
      xtx        = tr x <> x
      xtz        = tr x <> z
      xtzVzx     = xtz <> varU <> tr xtz
      trProj     = LA.sumElements (LA.takeDiag (inv xtx <> xtzVzx))
      trPxZVZ    = trZVZ - trProj
      sigma2ENew = (rss + trPxZVZ) / fromIntegral (n - p)
  in (tau2New, sigma2ENew, beta, u, varU)

-- | Diagnostic: run one emStep, return (τ²_new, σ²_new, β̂, û, varU-diag).
emStepDiag :: LMMData -> (Double, Double) -> (Double, Double, Vector Double, Vector Double, Vector Double)
emStepDiag lmm st =
  let (t2', s2', b, u, vu) = emStep lmm st
  in (t2', s2', b, u, LA.takeDiag vu)

-- | REML EM fit.  Iterates the spring-EM update until (τ², σ²_ε) converge.
fitLMM :: LMMData -> LMMFit
fitLMM lmm =
  let y = lmmY lmm
      x = lmmX lmm
      n = size y
      p = cols x
      -- Initial guesses: split total variance equally between τ² and σ²_ε.
      yBar     = LA.sumElements y / fromIntegral n
      centered = y - LA.scalar yBar
      totalVar = (centered <.> centered) / fromIntegral (n - 1)
      tau0     = 0.5 * totalVar
      sig0     = 0.5 * totalVar
      maxIter  = 10000
      tol      = 1e-12
      go !t2 !s2 !path !it
        | it > maxIter = finalize t2 s2 path it
        | converged    = finalize t2New s2New ((t2New, s2New) : path) it
        | otherwise    = go t2New s2New ((t2New, s2New) : path) (it + 1)
        where
          (t2New, s2New, _, _, _) = emStep lmm (t2, s2)
          converged = abs (t2New - t2) < tol * (1 + abs t2)
                   && abs (s2New - s2) < tol * (1 + abs s2)
      finalize t2 s2 path it =
        let (_, _, beta, u, varU) = emStep lmm (t2, s2)
        in LMMFit
             { fitBeta    = beta
             , fitU       = u
             , fitVarU    = LA.takeDiag varU
             , fitTau2    = t2
             , fitSigma2E = s2
             , fitPath    = reverse path
             , fitIters   = it
             }
  in go tau0 sig0 [(tau0, sig0)] 0

-- | Extract the spring representation at the fitted solution.
extractSprings :: LMMData -> LMMFit -> [Spring]
extractSprings lmm fit =
  let y  = lmmY lmm
      x  = lmmX lmm
      z  = lmmZ lmm
      b  = fitBeta fit
      u  = fitU fit
      s2 = fitSigma2E fit
      t2 = fitTau2 fit
      kObs = 1.0 / s2
      kShr = 1.0 / t2
      eta   = x #> b + z #> u
      resid = toList (y - eta)
      us    = toList u
      grpLabels = lmmGroupLabels lmm
      obsSprings =
        [ Spring Observation ("obs " ++ show i) kObs r (0.5 * kObs * r * r)
        | (i, r) <- zip [(0 :: Int)..] resid
        ]
      shrSprings =
        [ Spring Shrinkage ("group " ++ show g) kShr uj (0.5 * kShr * uj * uj)
        | (g, uj) <- zip grpLabels us
        ]
  in obsSprings ++ shrSprings

-- ======================================================================
-- Tier 3: vector random effects with r×r covariance Σ per group.
-- ======================================================================
-- Model:
--   y = X β + Z u + ε,   u_j ~ N(0, Σ)  for j = 1..J,   ε ~ N(0, σ²_ε I)
--
-- G = I_J ⊗ Σ (block-diagonal).  Shrinkage energy per group:
--   ½ u_jᵀ Σ⁻¹ u_j   — two self-springs + one cross-spring for r=2.
--
-- EM update for Σ (Laird-Ware, natural generalization of the tier-1
-- law-of-total-variance formula):
--   Σ_new = (1/J) Σ_j [ û_j û_jᵀ  +  Var(û_j | y) ]
-- σ²_ε update: same REML projection formula as tier 1.

data LMMFitRE = LMMFitRE
  { fitBetaRE    :: Vector Double   -- ^ p fixed effects
  , fitURE       :: Vector Double   -- ^ q = r·J BLUPs, stacked per group
  , fitVarURE    :: Matrix Double   -- ^ q × q posterior covariance of u
  , fitSigmaRE   :: Matrix Double   -- ^ r × r random-effects covariance
  , fitSigma2ERE :: Double          -- ^ residual variance
  , fitItersRE   :: Int             -- ^ outer EM iterations
  } deriving Show

-- | Build LMMData for a model with r random effects per group.
--
-- X comes from xsRows + optional intercept.
-- Z has r columns PER GROUP, containing (optional intercept) + zsRows covariates,
-- zero outside the row's group slab.
mkLMMVecRE
  :: [Double]    -- ^ responses
  -> [[Double]]  -- ^ X covariate rows (non-intercept)
  -> Bool        -- ^ add intercept column to X?
  -> [[Double]]  -- ^ Z r.e. covariate rows (non-intercept); empty list = intercept-only
  -> Bool        -- ^ add intercept column to Z?
  -> [Int]       -- ^ group label per row
  -> LMMData
mkLMMVecRE ys xsRows addIX zsRows addIZ grps =
  let n = length ys
      -- X
      xRow rs  = (if addIX then [1.0] else []) ++ rs
      xRowList = if null xsRows
                   then replicate n (if addIX then [1.0] else [])
                   else map xRow xsRows
      p = length (head xRowList)
      x = (n >< p) (concat xRowList)
      -- Z r.e. slab (one r-vector per row)
      zSlab rs  = (if addIZ then [1.0] else []) ++ rs
      zSlabs    = if null zsRows
                    then replicate n (if addIZ then [1.0] else [])
                    else map zSlab zsRows
      rSize     = length (head zSlabs)
      uniqGs    = nub grps
      jGroups   = length uniqGs
      gIdx g    = fromJust (elemIndex g uniqGs)
      zeros     = replicate rSize 0.0
      zRow (g, slab) = concat
        [ if k == gIdx g then slab else zeros
        | k <- [0 .. jGroups - 1]
        ]
      zMat      = fromLists $ map zRow (zip grps zSlabs)
  in LMMData (fromList ys) x zMat uniqGs rSize

-- | Tier-3 inner solve via explicit spring network (self- and cross-springs
-- per group encode the r×r Σ⁻¹ shrinkage quadratic form).  Same equilibrium
-- as the dense MME, just assembled one spring at a time.
innerSolveRE :: LMMData -> Matrix Double -> Double
             -> (Vector Double, Vector Double, Matrix Double)
innerSolveRE lmm sigma sigma2E =
  let y = lmmY lmm
      x = lmmX lmm
      z = lmmZ lmm
      p = cols x
      q = cols z
      j = length (lmmGroupLabels lmm)
      r = lmmGroupSize lmm
      net = buildSpringNetworkVecRE y x z r j sigma sigma2E
      (xSol, varU) = solveSpringNetworkWithVar net
      beta = LA.subVector 0 p xSol
      u    = LA.subVector p q xSol
  in (beta, u, varU)

-- | One EM step for tier 3: updates (Σ, σ²_ε).
emStepRE :: LMMData -> (Matrix Double, Double)
         -> (Matrix Double, Double, Vector Double, Vector Double, Matrix Double)
emStepRE lmm (sigma, sigma2E) =
  let y  = lmmY lmm
      x  = lmmX lmm
      z  = lmmZ lmm
      n  = size y
      p  = cols x
      j  = length (lmmGroupLabels lmm)
      r  = lmmGroupSize lmm
      (beta, u, varU) = innerSolveRE lmm sigma sigma2E
      -- Σ_new = (1/J) · Σ_j [ û_j û_jᵀ + Var(û_j|y) ]
      perGroup k =
        let uk    = LA.subVector (k * r) r u
            vk    = LA.subMatrix (k * r, k * r) (r, r) varU
            outer = LA.asColumn uk <> LA.asRow uk
        in outer + vk
      summed = foldl' (+) (LA.konst 0 (r, r))
                 [ perGroup k | k <- [0 .. j - 1] ]
      sigmaNew = LA.scale (1.0 / fromIntegral j) summed
      -- σ²_ε update (REML projection — same as tier 1)
      resid   = y - x #> beta - z #> u
      rss     = resid <.> resid
      ztzMat  = tr z <> z
      trZVZ   = LA.sumElements (LA.takeDiag (ztzMat <> varU))
      xtxMat  = tr x <> x
      xtzMat  = tr x <> z
      xtzVzx  = xtzMat <> varU <> tr xtzMat
      trProj  = LA.sumElements (LA.takeDiag (inv xtxMat <> xtzVzx))
      trPxZVZ = trZVZ - trProj
      sigma2ENew = (rss + trPxZVZ) / fromIntegral (n - p)
  in (sigmaNew, sigma2ENew, beta, u, varU)

-- | REML EM fit for tier 3.  Initial Σ is diagonal, σ²_ε = ½ · total var(y).
fitLMMRE :: LMMData -> LMMFitRE
fitLMMRE lmm =
  let y  = lmmY lmm
      n  = size y
      r  = lmmGroupSize lmm
      yBar     = LA.sumElements y / fromIntegral n
      centered = y - LA.scalar yBar
      totalVar = (centered <.> centered) / fromIntegral (n - 1)
      -- start with diagonal Σ, equal across components, half of var(y).
      initSigma   = LA.scale (0.5 * totalVar) (ident r)
      initSigma2E = 0.5 * totalVar
      maxIter = 5000
      tol     = 1e-10
      go !sigma !s2 !it
        | it > maxIter = finalize sigma s2 it
        | converged    = finalize sigmaNew s2New (it + 1)
        | otherwise    = go sigmaNew s2New (it + 1)
        where
          (sigmaNew, s2New, _, _, _) = emStepRE lmm (sigma, s2)
          dSigma = LA.norm_2 (sigmaNew - sigma)
          dS2    = abs (s2New - s2)
          sSize  = LA.norm_2 sigma + 1e-12
          converged = dSigma < tol * (1 + sSize)
                   && dS2    < tol * (1 + abs s2)
      finalize sigma s2 it =
        let (_, _, beta, u, varU) = emStepRE lmm (sigma, s2)
        in LMMFitRE beta u varU sigma s2 it
  in go initSigma initSigma2E 0

-- ======================================================================
-- Logistic GLMM — IRLS inner + spring-EM outer (single grouping, scalar τ²).
-- ======================================================================
-- Model:
--   y_i ∈ {0,1},  p_i = σ(η_i),  η_i = X_i β + Z_i u
--   u_j ~ N(0, τ²)    (one scalar r.e. per group, as in LMMData with rSize=1)
--
-- Inner (IRLS) — at each step, linearize the log-likelihood around current (β̂, û):
--   w_i  = p_i (1 − p_i)                         (Fisher weight)
--   z̃_i = η_i + (y_i − p_i) / w_i                (working response)
--   then weighted MME with R⁻¹ = diag(w):
--     [ XᵀWX      XᵀWZ          ]   [β̂]     [XᵀW z̃]
--     [ ZᵀWX    ZᵀWZ + (1/τ²) I ] · [û]  =  [ZᵀW z̃]
-- Iterate until (β, u) converge.
--
-- Laplace posterior variance at the mode:
--   Var(û | y) ≈ (Zᵀ W Z + G⁻¹ − ZᵀW X (XᵀWX)⁻¹ XᵀW Z)⁻¹
-- (bottom-right block of the weighted-MME inverse, no σ²_ε scaling).
--
-- Outer EM:
--   τ²_new = (ûᵀû + tr(Var(û|y))) / q      (same moment-match as Gaussian)
--   No σ²_ε update — Bernoulli has no residual variance to estimate.

data GLMMFit = GLMMFit
  { glmmBeta   :: Vector Double
  , glmmU      :: Vector Double
  , glmmVarU   :: Matrix Double   -- ^ q × q Laplace posterior covariance
  , glmmTau2   :: Double
  , glmmEta    :: Vector Double   -- ^ fitted linear predictor
  , glmmP      :: Vector Double   -- ^ fitted probabilities
  , glmmW      :: Vector Double   -- ^ IRLS weights at convergence (= p(1-p))
  , glmmIters  :: Int             -- ^ outer EM iterations
  } deriving Show

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

-- | One IRLS step via the spring network.  At current (β, u), compute the
-- Fisher weights w and working response z̃, build a SpringNetwork with
-- per-obs hardness w_i and obs-target z̃_i, solve → new (β, u).
irlsStepNet
  :: LMMData
  -> Double
  -> (Vector Double, Vector Double)
  -> (Vector Double, Vector Double, Vector Double, Vector Double, Vector Double)
irlsStepNet lmm tau2 (beta, u) =
  let y = lmmY lmm
      x = lmmX lmm
      z = lmmZ lmm
      p_ = cols x
      q = cols z
      eta    = x #> beta + z #> u
      pVec   = LA.cmap sigmoid eta
      w0     = LA.cmap (\pp -> pp * (1 - pp)) pVec
      w      = LA.cmap (max 1e-6) w0      -- floor for numerical stability
      zw     = eta + (y - pVec) / w        -- working response
      net    = buildSpringNetworkIRLS zw x z w tau2
      (xSol, _) = solveSpringNetworkWithVar net
      betaNew = LA.subVector 0  p_ xSol
      uNew    = LA.subVector p_ q  xSol
  in (betaNew, uNew, eta, pVec, w)

-- | IRLS to convergence via the spring network.  Returns (β, u, Var(û|y), η, p, w).
irlsConverge
  :: LMMData
  -> Double
  -> (Vector Double, Vector Double)
  -> Int
  -> Double
  -> (Vector Double, Vector Double, Matrix Double, Vector Double, Vector Double, Vector Double)
irlsConverge lmm tau2 (b0, u0) maxIt tol = go b0 u0 0
  where
    go !b !u !it
      | it >= maxIt || converged = finalize bNew uNew
      | otherwise                = go bNew uNew (it + 1)
      where
        (bNew, uNew, _, _, _) = irlsStepNet lmm tau2 (b, u)
        db = LA.norm_2 (bNew - b)
        du = LA.norm_2 (uNew - u)
        converged = db < tol * (1 + LA.norm_2 b)
                 && du < tol * (1 + LA.norm_2 u)
    finalize b u =
      let x     = lmmX lmm
          z     = lmmZ lmm
          y     = lmmY lmm
          eta   = x #> b + z #> u
          pVec  = LA.cmap sigmoid eta
          w0    = LA.cmap (\pp -> pp * (1 - pp)) pVec
          w     = LA.cmap (max 1e-6) w0
          zw    = eta + (y - pVec) / w
          net   = buildSpringNetworkIRLS zw x z w tau2
          (_, cuu) = solveSpringNetworkWithVar net
      in (b, u, cuu, eta, pVec, w)

-- | Fit a logistic GLMM with single grouping, scalar τ².  IRLS inner,
-- spring-EM outer for τ².
fitLogisticGLMM :: LMMData -> GLMMFit
fitLogisticGLMM lmm =
  let p_  = cols (lmmX lmm)
      q   = cols (lmmZ lmm)
      -- Initial: β, u = 0; τ² = 0.5.
      b0  = LA.konst 0 p_
      u0  = LA.konst 0 q
      initTau2 = 0.5
      maxOuter = 500
      outerTol = 1e-6
      irlsMax  = 50
      irlsTol  = 1e-8
      go !tau2 !b !u !it
        | it > maxOuter = finalize tau2 b u (it - 1)
        | converged     = finalize tau2New bNew uNew it
        | otherwise     = go tau2New bNew uNew (it + 1)
        where
          (bNew, uNew, varU, _, _, _) =
            irlsConverge lmm tau2 (b, u) irlsMax irlsTol
          trVarU = LA.sumElements (LA.takeDiag varU)
          tau2New = (uNew <.> uNew + trVarU) / fromIntegral q
          converged = abs (tau2New - tau2) < outerTol * (1 + abs tau2)
      finalize tau2 b u it =
        let (bF, uF, varU, eta, p, w) =
              irlsConverge lmm tau2 (b, u) irlsMax irlsTol
        in GLMMFit
             { glmmBeta  = bF
             , glmmU     = uF
             , glmmVarU  = varU
             , glmmTau2  = tau2
             , glmmEta   = eta
             , glmmP     = p
             , glmmW     = w
             , glmmIters = it
             }
  in go initTau2 b0 u0 0

-- ======================================================================

-- | Compact summary of the spring network.
summarizeSprings :: [Spring] -> String
summarizeSprings sprs =
  let obs  = filter ((== Observation) . spKind) sprs
      shr  = filter ((== Shrinkage)   . spKind) sprs
      eObs = sum (map spEnergy obs)
      eShr = sum (map spEnergy shr)
      kObs = if null obs then 0 else spHardness (head obs)
      kShr = if null shr then 0 else spHardness (head shr)
      fmt k = show k
  in   "  observation springs: n="   ++ show (length obs)
    ++ " hardness=1/σ²_ε="           ++ fmt kObs
    ++ " energy="                    ++ fmt eObs
    ++ "\n  shrinkage   springs: q=" ++ show (length shr)
    ++ " hardness=1/τ²="             ++ fmt kShr
    ++ " energy="                    ++ fmt eShr
    ++ "\n  total energy="           ++ fmt (eObs + eShr)
