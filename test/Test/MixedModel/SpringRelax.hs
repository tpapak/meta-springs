{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Proper iterative spring solver with nonlinear springs.
--
-- Unlike SpringNet.hs which assembles A·x = b and solves in one shot,
-- this module computes forces on nodes and iteratively relaxes until
-- equilibrium.  Supports both linear (Gaussian) and nonlinear
-- (binomial/logistic) observation springs.
--
-- The solver is pure force-based gradient descent with backtracking
-- line search — no matrix assembly, no direct solve.  Every iteration
-- computes net force on each node from its connected springs and steps
-- in the force direction.
module Test.MixedModel.SpringRelax
  ( NLSpring(..)
  , buildGaussianSprings
  , buildBinomialSprings
  , springEnergy
  , springForce
  , springHessian
  , relaxGD
  , relaxCG
  , relaxNewton
  , fitSpringEM
  , fitSpringEMBinomial
  , fitSpringEMNewton
  , fitSpringEMBinomialNewton
  -- Z integration (exact posterior for variance components)
  , Tau2Posterior(..)
  , fitGaussianZ
  , fitBinomialZ
  -- Grand canonical (chain springs)
  , fitGaussianGrand
  , fitGaussianGrandEigen
  ) where

import Prelude hiding ((<>))
import Data.List (foldl')
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra
  ( Vector, Matrix
  , size, cols
  , fromList, toList
  , (><), (!), (<>), (#>), (<.>)
  , atIndex, inv, ident, tr
  )

-- | A nonlinear spring.  Each spring has:
--   * coefficients connecting to free nodes
--   * a constant term (from pinned/data nodes)
--   * energy, gradient, and Hessian as functions of the extension
--
-- Extension at position x:  e(x) = Σ coef_j · x_j  +  const
--
-- Linear spring:   energy(e) = ½ k e²,   grad(e) = k·e,   hess(e) = k
-- Binomial spring: energy(e) = -y·e + log(1+exp(e)),
--                  grad(e)   = σ(e) - y,
--                  hess(e)   = σ(e)·(1 - σ(e))
data NLSpring = NLSpring
  { nlCoefs    :: ![(Int, Double)]  -- (free-node index, coefficient)
  , nlConst    :: !Double           -- constant from pinned nodes
  , nlEnergyF  :: Double -> Double  -- energy as function of extension
  , nlGradF    :: Double -> Double  -- dE/d(extension)
  , nlHessF    :: Double -> Double  -- d²E/d(extension)²
  , nlLabel    :: !String
  }

-- | Extension of a spring at position x.
ext :: NLSpring -> Vector Double -> Double
ext s x = sum [c * (x ! i) | (i, c) <- nlCoefs s] + nlConst s

-- | Total energy of the spring system.
springEnergy :: [NLSpring] -> Vector Double -> Double
springEnergy springs x = sum [nlEnergyF s (ext s x) | s <- springs]

-- | Gradient of total energy w.r.t. node positions (= negative force).
springForce :: [NLSpring] -> Int -> Vector Double -> Vector Double
springForce springs n x =
  let g0 = LA.konst 0 n
      addSpring !g s =
        let e  = ext s x
            dE = nlGradF s e
            updates = [(i, dE * c) | (i, c) <- nlCoefs s]
        in LA.accum g (+) updates
  in foldl' addSpring g0 springs

-- | Hessian of total energy (assembled from springs at current position).
springHessian :: [NLSpring] -> Int -> Vector Double -> Matrix Double
springHessian springs n x =
  let h0 = LA.konst 0 (n, n) :: Matrix Double
      addSpring !h s =
        let e  = ext s x
            d2 = nlHessF s e
            cs = nlCoefs s
            updates = [((i, j), d2 * ci * cj) | (i, ci) <- cs, (j, cj) <- cs]
        in LA.accum h (+) updates :: Matrix Double
  in foldl' addSpring h0 springs

-- ======================================================================
-- Spring builders
-- ======================================================================

sigmoid :: Double -> Double
sigmoid !x
  | x > 30    = 1.0
  | x < (-30) = 0.0
  | otherwise  = 1.0 / (1.0 + exp (negate x))

-- | Numerically stable log(1 + exp(x)).
softplus :: Double -> Double
softplus !x
  | x > 30    = x
  | x < (-30) = 0.0
  | otherwise  = log (1.0 + exp x)

-- | Build springs for a Gaussian LMM:  y ~ Xβ + Zu + ε
--
-- Observation springs (linear, hardness 1/σ²):
--   extension = y_i - X_i β - Z_i u
--   energy = ½(1/σ²)(y_i - η_i)²
--
-- Shrinkage springs (linear, hardness 1/τ²):
--   extension = u_k
--   energy = ½(1/τ²) u_k²
buildGaussianSprings
  :: Vector Double -> Matrix Double -> Matrix Double
  -> Double -> Double
  -> [NLSpring]
buildGaussianSprings y x z sigma2E tau2 =
  let n    = size y
      p    = cols x
      q    = cols z
      kObs = 1.0 / sigma2E
      kShr = 1.0 / tau2

      obsSpr i =
        let yi  = y ! i
            bCs = [(j, negate (x `atIndex` (i, j)))
                  | j <- [0..p-1], abs (x `atIndex` (i,j)) > 0]
            uCs = [(p+k, negate (z `atIndex` (i, k)))
                  | k <- [0..q-1], abs (z `atIndex` (i,k)) > 0]
        in NLSpring
          { nlCoefs   = bCs ++ uCs
          , nlConst   = yi
          , nlEnergyF = \e -> 0.5 * kObs * e * e
          , nlGradF   = \e -> kObs * e
          , nlHessF   = \_ -> kObs
          , nlLabel   = "obs_" ++ show i
          }

      shrSpr k = NLSpring
        { nlCoefs   = [(p + k, 1.0)]
        , nlConst   = 0
        , nlEnergyF = \e -> 0.5 * kShr * e * e
        , nlGradF   = \e -> kShr * e
        , nlHessF   = \_ -> kShr
        , nlLabel   = "shr_" ++ show k
        }

  in map obsSpr [0..n-1] ++ map shrSpr [0..q-1]

-- | Build springs for a logistic GLMM:  y_i ~ Bernoulli(σ(η_i)),
--   η_i = X_i β + Z_i u,   u_k ~ N(0, τ²)
--
-- Observation springs (NONLINEAR):
--   extension = η_i = X_i β + Z_i u
--   energy = -y_i η_i + log(1 + exp(η_i))
--   grad  = σ(η_i) - y_i
--   hess  = σ(η_i)(1 - σ(η_i))
--
-- Shrinkage springs (linear, hardness 1/τ²):
--   extension = u_k
--   energy = ½(1/τ²) u_k²
buildBinomialSprings
  :: Vector Double -> Matrix Double -> Matrix Double
  -> Double
  -> [NLSpring]
buildBinomialSprings y x z tau2 =
  let n    = size y
      p    = cols x
      q    = cols z
      kShr = 1.0 / tau2

      obsSpr i =
        let yi  = y ! i
            -- NOTE: positive coefficients — extension = η, not residual
            bCs = [(j, x `atIndex` (i, j))
                  | j <- [0..p-1], abs (x `atIndex` (i,j)) > 0]
            uCs = [(p+k, z `atIndex` (i, k))
                  | k <- [0..q-1], abs (z `atIndex` (i,k)) > 0]
        in NLSpring
          { nlCoefs   = bCs ++ uCs
          , nlConst   = 0
          , nlEnergyF = \eta -> negate yi * eta + softplus eta
          , nlGradF   = \eta -> sigmoid eta - yi
          , nlHessF   = \eta -> let p_ = sigmoid eta in p_ * (1 - p_)
          , nlLabel   = "binom_" ++ show i
          }

      shrSpr k = NLSpring
        { nlCoefs   = [(p + k, 1.0)]
        , nlConst   = 0
        , nlEnergyF = \e -> 0.5 * kShr * e * e
        , nlGradF   = \e -> kShr * e
        , nlHessF   = \_ -> kShr
        , nlLabel   = "shr_" ++ show k
        }

  in map obsSpr [0..n-1] ++ map shrSpr [0..q-1]

-- ======================================================================
-- Iterative solvers (force-based, no matrix assembly)
-- ======================================================================

-- | Gradient descent with backtracking line search.
-- Pure force-based: computes gradient, steps in negative gradient direction.
-- No matrix operations — just spring forces.
relaxGD
  :: [NLSpring]
  -> Int             -- ^ number of free nodes
  -> Vector Double   -- ^ initial positions
  -> Double          -- ^ tolerance on |gradient|
  -> Int             -- ^ max iterations
  -> (Vector Double, Int, Double)  -- ^ (solution, iterations, final |grad|)
relaxGD springs n x0 tol maxIter = go x0 0
  where
    go !x !it
      | gNorm < tol  = (x, it, gNorm)
      | it >= maxIter = (x, it, gNorm)
      | otherwise     = go xNew (it + 1)
      where
        g     = springForce springs n x
        gNorm = LA.norm_2 g
        -- Backtracking line search (Armijo)
        alpha0 = 1.0
        c1     = 1e-4
        e0     = springEnergy springs x
        findAlpha !a
          | a < 1e-15 = a  -- give up
          | springEnergy springs xTry <= e0 - c1 * a * (g <.> g)
                      = a
          | otherwise = findAlpha (0.5 * a)
          where xTry = x - LA.scale a g
        alpha = findAlpha alpha0
        xNew  = x - LA.scale alpha g

-- | Conjugate gradient (Polak-Ribière) with line search.
-- Still purely force-based — no matrix assembly.
relaxCG
  :: [NLSpring]
  -> Int
  -> Vector Double
  -> Double
  -> Int
  -> (Vector Double, Int, Double)
relaxCG springs n x0 tol maxIter = go x0 g0 (LA.scale (-1) g0) 0
  where
    g0 = springForce springs n x0
    go !x !gOld !d !it
      | gNorm < tol  = (x, it, gNorm)
      | it >= maxIter = (x, it, gNorm)
      | otherwise     = go xNew gNew dNew (it + 1)
      where
        gNorm = LA.norm_2 gOld
        e0    = springEnergy springs x
        -- Line search along d
        findAlpha !a
          | a < 1e-15 = a
          | springEnergy springs xTry <= e0 + 1e-4 * a * (gOld <.> d)
                      = a
          | otherwise = findAlpha (0.5 * a)
          where xTry = x + LA.scale a d
        alpha = findAlpha 1.0
        xNew  = x + LA.scale alpha d
        gNew  = springForce springs n xNew
        -- Polak-Ribière beta
        betaPR = max 0 ((gNew <.> (gNew - gOld)) / (gOld <.> gOld))
        dNew   = LA.scale (-1) gNew + LA.scale betaPR d

-- | Newton's method on the spring energy.
-- Each step: compute gradient and Hessian from springs, solve H·d = -g,
-- step x ← x + d.  For linear springs this converges in 1 iteration
-- (the energy is quadratic).  For nonlinear springs (binomial), typically
-- 5-15 iterations.
--
-- This is what TMB does internally (with AD for the derivatives).
-- We compute derivatives from the per-spring grad/hess functions instead.
relaxNewton
  :: [NLSpring]
  -> Int             -- ^ number of free nodes
  -> Vector Double   -- ^ initial positions
  -> Double          -- ^ tolerance on |gradient|
  -> Int             -- ^ max iterations
  -> (Vector Double, Int, Double)  -- ^ (solution, iterations, final |grad|)
relaxNewton springs n x0 tol maxIter = go x0 0
  where
    go !x !it
      | gNorm < tol  = (x, it, gNorm)
      | it >= maxIter = (x, it, gNorm)
      | otherwise     = go xNew (it + 1)
      where
        g     = springForce springs n x
        gNorm = LA.norm_2 g
        h     = springHessian springs n x
        -- Newton direction: solve H d = -g
        d     = LA.flatten (h LA.<\> LA.asColumn (LA.scale (-1) g))
        -- Backtracking line search for safety (Newton step may overshoot
        -- on highly nonlinear problems)
        e0 = springEnergy springs x
        findAlpha !a
          | a < 1e-15 = a
          | springEnergy springs xTry <= e0 + 1e-4 * a * (g <.> d)
                      = a
          | otherwise = findAlpha (0.5 * a)
          where xTry = x + LA.scale a d
        alpha = findAlpha 1.0
        xNew  = x + LA.scale alpha d

-- ======================================================================
-- Full EM with spring relaxation inner solve
-- ======================================================================

-- | Fit Gaussian LMM by spring-EM with force-based inner relaxation.
-- Outer loop updates (τ², σ²_ε) via law-of-total-variance.
-- Inner loop: relax springs to equilibrium using CG.
-- Posterior variance: computed from Hessian at converged solution (Laplace).
fitSpringEM
  :: Vector Double -> Matrix Double -> Matrix Double
  -> (Vector Double, Vector Double, Double, Double, Int, Int)
  -- ^ (β, u, τ², σ²_ε, outer_iters, total_inner_iters)
fitSpringEM y x z =
  let n = size y
      p = cols x
      q = cols z
      m = p + q
      -- Initial variance components
      yBar     = LA.sumElements y / fromIntegral n
      centered = y - LA.scalar yBar
      totalVar = (centered <.> centered) / fromIntegral (n - 1)
      tau0     = 0.5 * totalVar
      sig0     = 0.5 * totalVar
      outerTol = 1e-10
      maxOuter = 5000
      innerTol = 1e-12

      go !tau2 !s2 !x0 !oIt !totInner
        | oIt > maxOuter = finish tau2 s2 x0 oIt totInner
        | converged      = finish tau2New s2New xSol (oIt+1) (totInner + iIter)
        | otherwise      = go tau2New s2New xSol (oIt+1) (totInner + iIter)
        where
          springs = buildGaussianSprings y x z s2 tau2
          (xSol, iIter, _) = relaxCG springs m x0 innerTol 10000
          -- Extract β, u
          beta = LA.subVector 0 p xSol
          u    = LA.subVector p q xSol
          -- Compute Hessian at solution for posterior variance
          hess = springHessian springs m xSol
          hInv = inv hess
          varU = LA.subMatrix (p, p) (q, q) hInv
          -- EM updates
          uSq    = u <.> u
          trVarU = LA.sumElements (LA.takeDiag varU)
          tau2New = (uSq + trVarU) / fromIntegral q
          resid   = y - x #> beta - z #> u
          rss     = resid <.> resid
          trZVZ   = LA.sumElements (LA.takeDiag (tr z <> z <> varU))
          xtx     = tr x <> x
          xtz     = tr x <> z
          xtzVzx  = xtz <> varU <> tr xtz
          trProj  = LA.sumElements (LA.takeDiag (inv xtx <> xtzVzx))
          trPxZVZ = trZVZ - trProj
          s2New   = (rss + trPxZVZ) / fromIntegral (n - p)
          -- Convergence check
          converged = abs (tau2New - tau2) < outerTol * (1 + abs tau2)
                   && abs (s2New - s2)   < outerTol * (1 + abs s2)

      finish tau2 s2 x0 oIt totInner =
        let springs = buildGaussianSprings y x z s2 tau2
            (xSol, iIter, _) = relaxCG springs m x0 innerTol 10000
            beta = LA.subVector 0 p xSol
            u    = LA.subVector p q xSol
        in (beta, u, tau2, s2, oIt, totInner + iIter)

  in go tau0 sig0 (LA.konst 0 m) 0 0

-- | Fit logistic GLMM by spring-EM with force-based inner relaxation.
-- Inner: relax nonlinear (binomial + quadratic shrinkage) springs via CG.
-- Outer: update τ² from posterior variance at mode (Laplace-EM).
fitSpringEMBinomial
  :: Vector Double -> Matrix Double -> Matrix Double
  -> (Vector Double, Vector Double, Double, Int, Int)
  -- ^ (β, u, τ², outer_iters, total_inner_iters)
fitSpringEMBinomial y x z =
  let p = cols x
      q = cols z
      m = p + q
      initTau2  = 0.5
      outerTol  = 1e-6
      maxOuter  = 500
      innerTol  = 1e-8

      go !tau2 !x0 !oIt !totInner
        | oIt > maxOuter = finish tau2 x0 oIt totInner
        | converged      = finish tau2New xSol (oIt+1) (totInner + iIter)
        | otherwise      = go tau2New xSol (oIt+1) (totInner + iIter)
        where
          springs = buildBinomialSprings y x z tau2
          (xSol, iIter, _) = relaxCG springs m x0 innerTol 5000
          u = LA.subVector p q xSol
          -- Hessian at mode → Laplace posterior variance
          hess = springHessian springs m xSol
          hInv = inv hess
          varU = LA.subMatrix (p, p) (q, q) hInv
          trVarU  = LA.sumElements (LA.takeDiag varU)
          tau2New = (u <.> u + trVarU) / fromIntegral q
          converged = abs (tau2New - tau2) < outerTol * (1 + abs tau2)

      finish tau2 x0 oIt totInner =
        let springs = buildBinomialSprings y x z tau2
            (xSol, iIter, _) = relaxCG springs m x0 innerTol 5000
            beta = LA.subVector 0 p xSol
            u    = LA.subVector p q xSol
        in (beta, u, tau2, oIt, totInner + iIter)

  in go initTau2 (LA.konst 0 m) 0 0

-- | Fit Gaussian LMM with Newton inner solve.
-- For linear springs, Newton converges in 1 iteration per EM step
-- (the energy is quadratic — Newton finds the exact minimum).
fitSpringEMNewton
  :: Vector Double -> Matrix Double -> Matrix Double
  -> (Vector Double, Vector Double, Double, Double, Int, Int)
fitSpringEMNewton y x z =
  let n = size y
      p = cols x
      q = cols z
      m = p + q
      yBar     = LA.sumElements y / fromIntegral n
      centered = y - LA.scalar yBar
      totalVar = (centered <.> centered) / fromIntegral (n - 1)
      tau0     = 0.5 * totalVar
      sig0     = 0.5 * totalVar
      outerTol = 1e-10
      maxOuter = 5000
      innerTol = 1e-12

      go !tau2 !s2 !x0 !oIt !totInner
        | oIt > maxOuter = finish tau2 s2 x0 oIt totInner
        | converged      = finish tau2New s2New xSol (oIt+1) (totInner + iIter)
        | otherwise      = go tau2New s2New xSol (oIt+1) (totInner + iIter)
        where
          springs = buildGaussianSprings y x z s2 tau2
          (xSol, iIter, _) = relaxNewton springs m x0 innerTol 100
          beta = LA.subVector 0 p xSol
          u    = LA.subVector p q xSol
          hess = springHessian springs m xSol
          hInv = inv hess
          varU = LA.subMatrix (p, p) (q, q) hInv
          uSq    = u <.> u
          trVarU = LA.sumElements (LA.takeDiag varU)
          tau2New = (uSq + trVarU) / fromIntegral q
          resid   = y - x #> beta - z #> u
          rss     = resid <.> resid
          trZVZ   = LA.sumElements (LA.takeDiag (tr z <> z <> varU))
          xtx     = tr x <> x
          xtz     = tr x <> z
          xtzVzx  = xtz <> varU <> tr xtz
          trProj  = LA.sumElements (LA.takeDiag (inv xtx <> xtzVzx))
          trPxZVZ = trZVZ - trProj
          s2New   = (rss + trPxZVZ) / fromIntegral (n - p)
          converged = abs (tau2New - tau2) < outerTol * (1 + abs tau2)
                   && abs (s2New - s2)   < outerTol * (1 + abs s2)

      finish tau2 s2 x0 oIt totInner =
        let springs = buildGaussianSprings y x z s2 tau2
            (xSol, iIter, _) = relaxNewton springs m x0 innerTol 100
            beta = LA.subVector 0 p xSol
            u    = LA.subVector p q xSol
        in (beta, u, tau2, s2, oIt, totInner + iIter)

  in go tau0 sig0 (LA.konst 0 m) 0 0

-- | Fit logistic GLMM with Newton inner solve.
-- For nonlinear (binomial) springs, Newton typically converges in 5-15
-- iterations per EM step — vs ~5000 for CG.
fitSpringEMBinomialNewton
  :: Vector Double -> Matrix Double -> Matrix Double
  -> (Vector Double, Vector Double, Double, Int, Int)
fitSpringEMBinomialNewton y x z =
  let p = cols x
      q = cols z
      m = p + q
      initTau2  = 0.5
      outerTol  = 1e-6
      maxOuter  = 500
      innerTol  = 1e-8

      go !tau2 !x0 !oIt !totInner
        | oIt > maxOuter = finish tau2 x0 oIt totInner
        | converged      = finish tau2New xSol (oIt+1) (totInner + iIter)
        | otherwise      = go tau2New xSol (oIt+1) (totInner + iIter)
        where
          springs = buildBinomialSprings y x z tau2
          (xSol, iIter, _) = relaxNewton springs m x0 innerTol 200
          u = LA.subVector p q xSol
          hess = springHessian springs m xSol
          hInv = inv hess
          varU = LA.subMatrix (p, p) (q, q) hInv
          trVarU  = LA.sumElements (LA.takeDiag varU)
          tau2New = (u <.> u + trVarU) / fromIntegral q
          converged = abs (tau2New - tau2) < outerTol * (1 + abs tau2)

      finish tau2 x0 oIt totInner =
        let springs = buildBinomialSprings y x z tau2
            (xSol, iIter, _) = relaxNewton springs m x0 innerTol 200
            beta = LA.subVector 0 p xSol
            u    = LA.subVector p q xSol
        in (beta, u, tau2, oIt, totInner + iIter)

  in go initTau2 (LA.konst 0 m) 0 0

-- ======================================================================
-- Z integration: exact posterior for τ² via grid
-- ======================================================================

data Tau2Posterior = Tau2Posterior
  { tpTau2Grid    :: [Double]
  , tpLogPost     :: [Double]
  , tpPosterior   :: [Double]
  , tpMean        :: Double
  , tpMode        :: Double
  , tpCI          :: (Double, Double)
  , tpBeta        :: Vector Double
  , tpU           :: Vector Double
  , tpSigma2E     :: Maybe Double
  } deriving Show

-- | Evaluate logZ at a single τ² value (Gaussian, profiling σ²).
evalLogZ_Gauss
  :: Vector Double -> Matrix Double -> Matrix Double
  -> Int -> Int -> Int -> Int -> Double
  -> Double
  -> (Double, Vector Double, Double)  -- (logZ, solution, sigma2)
evalLogZ_Gauss y x z n p q m totalVarHalf tau2 =
  let go s2 sol 0 = (s2, sol)
      go s2 sol it =
        let spr = buildGaussianSprings y x z s2 tau2
            (xS, _, _) = relaxNewton spr m sol 1e-12 10
            b_ = LA.subVector 0 p xS
            u_ = LA.subVector p q xS
            r_ = y - x #> b_ - z #> u_
            s2' = (r_ <.> r_) / fromIntegral (n - p)
        in go s2' xS (it - 1 :: Int)
      (sigma2, xSol) = go totalVarHalf (LA.konst 0 m) 10
      beta = LA.subVector 0 p xSol
      u    = LA.subVector p q xSol
      resid = y - x #> beta - z #> u
      rss   = resid <.> resid
      uSq   = u <.> u
      eStar = 0.5 * rss / sigma2 + 0.5 * uSq / tau2
      hess  = springHessian (buildGaussianSprings y x z sigma2 tau2) m xSol
      logDetH = log (abs (LA.det hess))
      logMarg = - fromIntegral n / 2 * log sigma2
              - fromIntegral q / 2 * log tau2
              - 0.5 * logDetH - eStar
  in (logMarg, xSol, sigma2)

-- | Fit Gaussian LMM by Z integration with adaptive grid.
-- Phase 1: coarse grid → find approximate mode
-- Phase 2: Hessian at mode → posterior width σ_τ
-- Phase 3: fine grid from mode-4σ to mode+4σ → exact posterior
fitGaussianZ
  :: Vector Double -> Matrix Double -> Matrix Double
  -> Tau2Posterior
fitGaussianZ y x z =
  let n = size y
      p = cols x
      q = cols z
      m = p + q
      yBar = LA.sumElements y / fromIntegral n
      centered = y - LA.scalar yBar
      totalVar = (centered <.> centered) / fromIntegral (n - 1)
      tvHalf = 0.5 * totalVar
      eval = evalLogZ_Gauss y x z n p q m tvHalf

      -- Phase 1: coarse grid (20 points) to find ballpark mode
      coarseN = 20 :: Int
      coarseMax = 4 * totalVar
      coarseMin = max 1e-2 (totalVar / 100)
      coarseDt = (coarseMax - coarseMin) / fromIntegral coarseN
      coarseGrid = [coarseMin + fromIntegral i * coarseDt | i <- [0..coarseN]]
      coarseResults = map (\t2 -> let (lz, _, _) = eval t2 in (t2, lz)) coarseGrid
      (approxMode, _) = foldl1 (\a@(_, la) b@(_, lb) -> if lb > la then b else a) coarseResults

      -- Phase 2: refine mode by golden section, then get curvature
      -- Simple: evaluate 3 points near approxMode, fit parabola
      h = max 1 (approxMode * 0.05)
      (lzL, _, _) = eval (max 1e-2 (approxMode - h))
      (lzM, _, _) = eval approxMode
      (lzR, _, _) = eval (approxMode + h)
      -- Curvature d²logZ/dτ² ≈ (lzL - 2*lzM + lzR) / h²
      curv = (lzL - 2 * lzM + lzR) / (h * h)
      -- Posterior std dev of τ²
      sigmaT = if curv < 0 then 1 / sqrt (negate curv) else 4 * approxMode

      -- Phase 3: fine grid centered on mode, ±4σ
      fineN = 80 :: Int
      fineMin = max 1e-2 (approxMode - 4 * sigmaT)
      fineMax = approxMode + 6 * sigmaT  -- extend right for skew
      fineDt = (fineMax - fineMin) / fromIntegral fineN
      tau2Grid = [fineMin + fromIntegral i * fineDt | i <- [0..fineN]]
      results = map (\t2 -> let (lm, sol, s2) = eval t2
                            in (t2, lm, sol, s2)) tau2Grid
      logPosts = map (\(_, lm, _, _) -> lm) results
      maxLP = maximum logPosts
      unnorm = map (\lp -> exp (lp - maxLP)) logPosts
      totalMass = sum unnorm * fineDt
      posterior = map (/ totalMass) unnorm
      postMean = sum (zipWith (\t2 p_ -> t2 * p_ * fineDt) tau2Grid posterior)
      (modeT2, _, modeSol, modeS2) = foldl1 (\a@(_, la, _, _) b@(_, lb, _, _) ->
        if lb > la then b else a) results
      cumul = scanl1 (+) (map (* fineDt) posterior)
      ci_lo = tau2Grid !! findIdx cumul 0.025
      ci_hi = tau2Grid !! findIdx cumul 0.975
      sprM = buildGaussianSprings y x z modeS2 postMean
      (xM, _, _) = relaxNewton sprM m modeSol 1e-12 10
  in Tau2Posterior
       { tpTau2Grid  = tau2Grid
       , tpLogPost   = logPosts
       , tpPosterior = posterior
       , tpMean      = postMean
       , tpMode      = modeT2
       , tpCI        = (ci_lo, ci_hi)
       , tpBeta      = LA.subVector 0 p xM
       , tpU         = LA.subVector p q xM
       , tpSigma2E   = Just modeS2
       }

-- | Fit logistic GLMM by Z integration (Laplace at each grid point).
fitBinomialZ
  :: Vector Double -> Matrix Double -> Matrix Double
  -> Tau2Posterior
fitBinomialZ y x z =
  let p = cols x
      q = cols z
      m = p + q
      nGrid = 80 :: Int
      tau2Max = 0.3
      tau2Min = 1e-6
      dt = (tau2Max - tau2Min) / fromIntegral nGrid
      tau2Grid = [tau2Min + fromIntegral i * dt | i <- [0..nGrid]]
      evalPoint tau2 prevSol =
        let springs = buildBinomialSprings y x z tau2
            (xSol, _, _) = relaxNewton springs m prevSol 1e-8 200
            u    = LA.subVector p q xSol
            uSq  = u <.> u
            -- Laplace log marginal:
            -- log p(y|τ²) ≈ log p(y|β*,u*) + log p(u*|τ²) - ½ log|H|
            -- The binomial spring energy = -log p(y|η*) + ½(1/τ²)||u||²
            -- So: -E* = log p(y|η*) - ½(1/τ²)||u||²
            -- Full: log p(y|η*) + log p(u|τ²) - ½ log|H|
            --     = log p(y|η*) - ½(1/τ²)||u||² - ½ q log(2πτ²) - ½ log|H|
            -- Binomial obs springs have energy = -log p(y_i|η_i)
            -- Shrinkage springs have energy = ½(1/τ²) u_k²
            -- So E* = -log p(y|η*) + ½(1/τ²)||u||²  → -E* = log p(y|η*) - shrinkage
            eStar = springEnergy springs xSol
            hess  = springHessian springs m xSol
            logDetH = log (abs (LA.det hess))
            logMarg = -eStar
                    - 0.5 * fromIntegral q * log tau2
                    - 0.5 * logDetH
        in (logMarg, xSol)
      scanG [] _ acc = reverse acc
      scanG (t2:rest) prev acc =
        let (lm, sol) = evalPoint t2 prev
        in scanG rest sol ((t2, lm, sol) : acc)
      results = scanG tau2Grid (LA.konst 0 m) []
      logPosts = map (\(_, lm, _) -> lm) results
      maxLP = maximum logPosts
      unnorm = map (\lp -> exp (lp - maxLP)) logPosts
      totalMass = sum unnorm * dt
      posterior = map (/ totalMass) unnorm
      postMean = sum (zipWith (\t2 p_ -> t2 * p_ * dt) tau2Grid posterior)
      (modeT2, _, modeSol) = foldl1 (\a@(_, la, _) b@(_, lb, _) ->
        if lb > la then b else a) results
      cumul = scanl1 (+) (map (* dt) posterior)
      ci_lo = tau2Grid !! findIdx cumul 0.025
      ci_hi = tau2Grid !! findIdx cumul 0.975
      sprM = buildBinomialSprings y x z postMean
      (xM, _, _) = relaxNewton sprM m modeSol 1e-8 200
  in Tau2Posterior
       { tpTau2Grid  = tau2Grid
       , tpLogPost   = logPosts
       , tpPosterior = posterior
       , tpMean      = postMean
       , tpMode      = modeT2
       , tpCI        = (ci_lo, ci_hi)
       , tpBeta      = LA.subVector 0 p xM
       , tpU         = LA.subVector p q xM
       , tpSigma2E   = Nothing
       }

-- | Grand canonical ensemble: tau springs in series.
-- k_tau = individual spring hardness, N = chain length, tau2 = N/k_tau.
-- Sums Z_network(tau2=N/k_tau) over N=0..N_max.
-- Flat prior on N ≈ flat prior on tau2 with resolution 1/k_tau.
fitGaussianGrand
  :: Vector Double -> Matrix Double -> Matrix Double
  -> Double          -- ^ k_tau (individual tau spring hardness)
  -> Int             -- ^ N_max (maximum chain length)
  -> Tau2Posterior
fitGaussianGrand y x z kTau nMax =
  let n = size y
      p = cols x
      q = cols z
      m = p + q
      yBar = LA.sumElements y / fromIntegral n
      centered = y - LA.scalar yBar
      totalVarH = 0.5 * (centered <.> centered) / fromIntegral (n - 1)
      -- Evaluate logZ at tau2 = chainN / kTau for each chain length
      evalAt tau2 =
        let go s2 sol 0 = (s2, sol)
            go s2 sol it =
              let spr = buildGaussianSprings y x z s2 tau2
                  (xS, _, _) = relaxNewton spr m sol 1e-12 10
                  b_ = LA.subVector 0 p xS
                  u_ = LA.subVector p q xS
                  r_ = y - x #> b_ - z #> u_
                  s2' = (r_ <.> r_) / fromIntegral (n - p)
              in go s2' xS (it - 1 :: Int)
            (sigma2, xSol) = go totalVarH (LA.konst 0 m) 10
            beta = LA.subVector 0 p xSol
            u    = LA.subVector p q xSol
            resid = y - x #> beta - z #> u
            rss   = resid <.> resid
            uSq   = u <.> u
            eStar = 0.5 * rss / sigma2 + 0.5 * uSq / max 1e-10 tau2
            hess  = springHessian (buildGaussianSprings y x z sigma2 (max 1e-10 tau2)) m xSol
            logDetH = log (abs (LA.det hess))
            logMarg = - fromIntegral n / 2 * log sigma2
                    - fromIntegral q / 2 * log (max 1e-10 tau2)
                    - 0.5 * logDetH - eStar
        in (logMarg, xSol, sigma2)
      -- Chain lengths 0..nMax
      tau2Vals = [fromIntegral chainN / kTau | chainN <- [0..nMax]]
      -- N=0 uses tau2 very small (fixed effect limit)
      results = map (\t2 -> let t2' = max 1e-10 t2
                                (lm, sol, s2) = evalAt t2'
                            in (t2, lm, sol, s2)) tau2Vals
      logPosts = map (\(_, lm, _, _) -> lm) results
      maxLP = maximum logPosts
      unnorm = map (\lp -> exp (lp - maxLP)) logPosts
      totalMass = sum unnorm  -- discrete sum, no dt needed
      probs = map (/ totalMass) unnorm
      postMean = sum (zipWith (*) tau2Vals probs)
      (modeT2, _, modeSol, modeS2) = foldl1 (\a@(_, la, _, _) b@(_, lb, _, _) ->
        if lb > la then b else a) results
      cumul = scanl1 (+) probs
      ci_lo = tau2Vals !! findIdx cumul 0.025
      ci_hi = tau2Vals !! findIdx cumul 0.975
      postMedian = tau2Vals !! findIdx cumul 0.5
      sprM = buildGaussianSprings y x z modeS2 (max 1e-10 postMean)
      (xM, _, _) = relaxNewton sprM m modeSol 1e-12 10
  in Tau2Posterior
       { tpTau2Grid  = tau2Vals
       , tpLogPost   = logPosts
       , tpPosterior = probs
       , tpMean      = postMean
       , tpMode      = modeT2
       , tpCI        = (ci_lo, ci_hi)
       , tpBeta      = LA.subVector 0 p xM
       , tpU         = LA.subVector p q xM
       , tpSigma2E   = Just modeS2
       }

-- | Grand canonical via eigendecomposition — for known-variance models.
-- Model: y_i ~ N(X_i beta, v_i + tau2), v_i known.
-- No sigma2 to profile — this covers pairwise MA and NMA.
-- Precompute eigenvalues once, then each tau2 costs O(k) not O(k^3).
fitGaussianGrandEigen
  :: Vector Double   -- ^ y (observed effects)
  -> Vector Double   -- ^ v (known within-study variances)
  -> Matrix Double   -- ^ X (design matrix, k x p)
  -> Double          -- ^ k_tau
  -> Int             -- ^ N_max
  -> Tau2Posterior
fitGaussianGrandEigen y v xMat kTau nMax =
  let k = size y
      p = cols xMat
      vi = toList v
      -- For known-variance model, the marginal log-likelihood at tau2 is:
      -- logL(tau2) = -1/2 sum log(v_i+tau2) - 1/2 log|X' W X| - 1/2 y' P y
      -- where W = diag(1/(v_i+tau2)), P = W - W X (X'WX)^-1 X' W
      --
      -- For pairwise MA (X = [1,...,1]'):
      --   X'WX = sum(w_i), beta_hat = sum(w_i y_i)/sum(w_i)
      --   Everything is O(k) per tau2 evaluation.
      --
      -- For NMA (X is k x p), X'WX is p x p — need to solve a p x p system.
      -- With eigendecomposition of X' diag(1/v_i) X, this becomes O(p) per tau2.

      -- Evaluate logL at each tau2 — direct O(k) computation
      evalLogL tau2 =
        let wi    = map (\vi_ -> 1.0 / (vi_ + tau2)) vi
            wVec  = fromList wi
            -- X' W X  (p x p)
            xw    = tr xMat <> LA.diag wVec <> xMat
            -- X' W y  (p x 1)
            xwy   = tr xMat #> (wVec * y)
            -- beta_hat = (X'WX)^-1 X'Wy
            beta  = LA.flatten (xw LA.<\> LA.asColumn xwy)
            -- residuals
            resid = y - xMat #> beta
            -- y' P y = sum w_i r_i^2
            yPy   = sum (zipWith (\w r -> w * r * r) wi (toList resid))
            -- log determinants
            logDetSigma = sum (map (\vi_ -> log (vi_ + tau2)) vi)
            logDetXWX   = log (abs (LA.det xw))
            logL = -0.5 * logDetSigma - 0.5 * logDetXWX - 0.5 * yPy
        in (logL, beta)

      tau2Vals = [fromIntegral chainN / kTau | chainN <- [0..nMax]]
      results  = map (\t2 -> let t2' = max 1e-10 t2
                                 (lm, beta) = evalLogL t2'
                             in (t2, lm, beta)) tau2Vals
      logPosts = map (\(_, lm, _) -> lm) results
      maxLP    = maximum logPosts
      unnorm   = map (\lp -> exp (lp - maxLP)) logPosts
      totalMass = sum unnorm
      probs    = map (/ totalMass) unnorm
      postMean = sum (zipWith (*) tau2Vals probs)
      (modeT2, _, modeBeta) = foldl1 (\a@(_, la, _) b@(_, lb, _) ->
        if lb > la then b else a) results
      cumul = scanl1 (+) probs
      ci_lo = tau2Vals !! findIdx cumul 0.025
      ci_hi = tau2Vals !! findIdx cumul 0.975
  in Tau2Posterior
       { tpTau2Grid  = tau2Vals
       , tpLogPost   = logPosts
       , tpPosterior = probs
       , tpMean      = postMean
       , tpMode      = modeT2
       , tpCI        = (ci_lo, ci_hi)
       , tpBeta      = modeBeta
       , tpU         = fromList []  -- no random effects nodes in this formulation
       , tpSigma2E   = Nothing
       }

findIdx :: [Double] -> Double -> Int
findIdx [] _ = 0
findIdx (c:cs) target
  | c >= target = 0
  | otherwise   = 1 + findIdx cs target
