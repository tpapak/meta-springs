{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Meta.RandomEffects
-- Description : Find taus of a network
-- Copyright   : (c) Thodoris Papakonstantinou, 2022
-- License     : GPL-3
-- Maintainer  : dev@tpapak.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Perform root finding solution for finding taus of the random effects model
-- Multiple taus can be calculated for defined groups of studies
module Data.Meta.RandomEffects
  ( tauEnergy
  , totalEnergy
  , logLikelihood
  , tauCostFunction
  , thetaiEnergy
  , thetaiEnergy'
  , springREML
  , irlsSolve
  , newtonSolveReducedBin
  , springREMLBin
  , springREMLBinOpt
  , studyWithinVariance
  , studyKeff
  , studyPredictionVariance
  , predictionIntervalVariance
  , contrastPredictionVariances
  , i2BLUP
  , classicalI2
  , springGrandCanonical
  , springGrandCanonicalAdaptive
  , evalXiGaussianAt
  , Tau2Prior (..)
  , logTau2Prior
  , PosteriorSummary (..)
  , posteriorUnder
  , springGrandCanonicalBin
  , springGrandCanonicalBinFull
  , springGrandCanonicalBinAdaptive
  , evalXiBinAt
  )
where

import Control.Applicative
import Control.Parallel.Strategies (using, parList, rseq)
import Data.Either
import qualified Data.Graph.AdjacencyList as G
import qualified Data.Graph.AdjacencyList.BFS as BFS
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Tuple
import qualified Data.Set as Set
import qualified Data.Vector as V

import Data.Numerics
import Data.Meta.Effects
import Data.Meta.Matrices
import Data.Meta.Studies
import Data.Meta.NMA
import qualified Data.Meta.Matrix as Mat
import qualified Numeric.LinearAlgebra as LA

totalEnergy :: SpringNetwork -> Double
totalEnergy springNet =
  sum $ Map.elems $ Map.map springEnergy (springs springNet)

-- Energy of just the Tau springs
tauEnergy :: SpringNetwork -> Double
tauEnergy springNet =
  sum ( Map.elems 
        ( Map.map springEnergy 
        $ Map.restrictKeys (springs springNet) (tauEdges springNet)
        ) 
      )

-- | REML LogLikelihood of network without the constant term 
-- (ONLY PAIRWISE multiarms not correctly calculated)
-- NOT to be used for NMA!!
logLikelihood :: SpringNetwork -> Double
logLikelihood spnet = 
  let sprs = springs spnet
      numberOfSprings = fromIntegral $ Map.size sprs
      vss = vs2ts spnet
      !studyVars = Map.foldrWithKey(\e sp acc ->
        let esid = case (IM.lookup (G.from e) vss) of
                     Just (StudyEnd sid') -> Right sid'
                     Just (TauEnd (TauId sid' _)) -> Right sid'
                     _ -> Left "no studyID found in Edge"
            vi = 1 / (hardness sp)
         in case esid of
              Right sid -> Map.insertWith (+) sid vi acc
              Left err -> Map.empty
          ) Map.empty sprs
      !ksum = Map.foldr (\vi acc -> acc + log vi) 0 studyVars
      !ksum2 = log $ Map.foldr (\vi acc -> acc + (1 / vi)) 0 studyVars
      numberOfStudies = fromIntegral $ Map.size studyVars
      uu = totalEnergy spnet
   in  - (ksum/2) - (ksum2/2) - uu

-- | Var(δi)
vardis :: SpringNetwork -> [Double]
vardis spnet = 
  let studyVs = netStudies spnet
      sprs = springs spnet
      thetais sv = 
        let thetasprs = Map.elems $ 
              Map.filterWithKey 
              (\ (G.Edge u v) _ -> u == sv || v == sv) sprs
         in map (springLength) thetasprs
      veff sv = sum (map abs $ thetais sv) 
      vis = map (veff) studyVs 
   in vis
   
-- | Energy of tausprings to open to their final position
thetaiEnergy :: SpringNetwork -> [Double]
thetaiEnergy spnet = 
  let studyVs = netStudies spnet
      sprs = springs spnet
      thetais sv = 
        let thetasprs = Map.elems $ 
              Map.filterWithKey 
              (\ (G.Edge u v) _ -> u == sv || v == sv) sprs
         in map springLength thetasprs
      keff sv =   0.5 / sum (map abs $ thetais sv) 
      --studyQ sv = (keff sv) * (sum $ map (^2) (thetais sv))
      studyQ sv = 2 * (sum $ map abs (thetais sv))
   --in sum $ map studyQ studyVs
   --in (sum $ map ((1 /) . keff) studyVs)/ (fromIntegral $ length studyVs)
      kis = map ( (1/) . keff) studyVs 
      tauspls = map springLength $ Map.elems $ Map.filterWithKey (\e _ -> Set.member e (tauEdges spnet)) sprs 
   in [average kis, sampleVariance kis] ++ kis ++ tauspls

average :: [Double] -> Double
average la =
  (sum la) / fromIntegral (length la)

-- | Energy of tausprings to open to their final position
thetaiEnergy' :: SpringNetwork -> Double
thetaiEnergy' spnet = 
  let totalQ = totalEnergy spnet
      studyVs = netStudies spnet
      studyQs = sum $ map (\sv -> 
        (totalEnergy (springNetwork $ nma (removeStudy spnet sv))) - totalQ
                          ) studyVs
   in 2 * studyQs

-- | Cost function for finding tau, meaning: Qτ - dof
-- At the solution τ², Qτ = dof
tauCostFunction :: SpringNetwork -> Double
tauCostFunction spnet =
  let dofs = fromIntegral $ dof spnet
   in qqTau spnet - dofs

     

-- | Spring-REML estimator (deltais).
--
-- Iterates the EM variance decomposition (law of total variance):
--   τ²_new = (1/k) Σ δ_i² + (1/k) Σ (1/κ_i)
--
-- where:
--   δ_i  = tau spring length (BLUP residual = θ̂ - θ̃_i)
--   κ_i  = leave-one-out prediction precision:
--          1/τ² + 1/(σ_i² + Var(μ̂_{-i}))
--   k    = number of studies
--
-- δ_i comes directly from the spring network: the tau spring connects
-- the study vertex (θ̃_i) to the tau vertex (near θ̂). Its natural length
-- is 0, so springLength = θ̂ - θ̃_i = δ_i.
-- For 2-arm studies, each arm has a tau spring with opposite sign;
-- δ_i² = average of the squared tau spring lengths.
--
-- References:
--   - Karl (2026) arXiv:2602.09247 (REML via prediction-error covariances)
--   - Viechtbauer (2023) r-sig-meta-analysis (law of total variance)
--   - Henderson (1975) (BLUP and mixed model equations)
springREML ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] -> -- list of studies
  (Arm -> Either String a) -> -- study effect calculator
  Maybe TauSquare -> -- first guess
  Maybe Int -> -- iterations
  Maybe Double -> -- epsilon
  Either String (NetworkEffects, (Double, [Double]))
springREML studies armEffect mtau0 miters mε =
  let iters = fromMaybe 1000 miters
      ε = fromMaybe 10E-4 mε
      guess0 = fromMaybe 4.0 mtau0
      espringnet = makeSprings studies Nothing (Just guess0) armEffect
   in case espringnet of
        Left err -> Left err
        Right springsnet0 ->
          let studyVs0 = netStudies springsnet0
              k = fromIntegral $ length studyVs0
              -- One EM step: τ² → τ²_new
              emStep t2 =
                let neteffs = nma $ updateSprings springsnet0 t2
                    spnet = springNetwork neteffs
                    sprs = springs spnet
                    tauEs = tauEdges spnet
                    studyVs = netStudies spnet
                    allTauLengthsSq = concatMap (\sv ->
                      map (\(_, s) -> springLength s ^ 2)
                        $ Map.toList $ Map.filterWithKey
                        (\(G.Edge u v) _ -> (u == sv || v == sv)
                          && Set.member (G.Edge u v) tauEs)
                        sprs
                      ) studyVs
                    deltaVarMap = studyDeltaVariances neteffs
                    allVarDeltas = concatMap (\sv ->
                      IM.findWithDefault [] sv deltaVarMap
                      ) studyVs
                    nTotalArms = fromIntegral $ length allTauLengthsSq
                 in max 0 $ 2 * (sum allTauLengthsSq / nTotalArms
                                + sum allVarDeltas / nTotalArms)
              -- Newton-EM iteration.
              -- First emWarmup steps use plain EM. Then try Newton on
              -- g(τ²) = τ² - emStep(τ²) = 0 for up to newtonBudget
              -- steps. If Newton hasn't converged by then, finish with
              -- plain EM (cheap, one emStep per iteration).
              emWarmup = 5
              newtonBudget = 20
              step t2 path iter
                | abs (t2_em - t2) <= ε || iter > iters =
                    let neteffs' = nma $ updateSprings springsnet0 t2_em
                     in (neteffs', (t2_em, reverse (t2_em:path)))
                | iter < emWarmup || iter > emWarmup + newtonBudget =
                    -- Plain EM step
                    step t2_em (t2_em:path) (iter + 1)
                | otherwise =
                    -- Newton on g(τ²) = τ² - emStep(τ²)
                    let !g = t2 - t2_em
                        !h = max 1e-10 (abs t2 * 1e-6)
                        !g_plus  = (t2 + h) - emStep (t2 + h)
                        !g_minus = (t2 - h) - emStep (max 0 (t2 - h))
                        !gPrime = (g_plus - g_minus) / (2 * h)
                        !t2_newton = if abs gPrime > 1e-20
                                       then t2 - g / gPrime
                                       else t2_em
                        emDir = t2_em - t2
                        nwDir = t2_newton - t2
                        !t2_next = if t2_newton > 0
                                     && emDir * nwDir > 0
                                     then t2_newton
                                     else t2_em
                     in if abs (t2_next - t2) <= ε
                         then
                           let neteffs' = nma $ updateSprings springsnet0 t2_next
                            in (neteffs', (t2_next, reverse (t2_next:path)))
                         else step t2_next (t2_next:path) (iter + 1)
                where !t2_em = emStep t2
           in Right (step guess0 [] 1)

-- | Within-study variance σ_i² for each study vertex.
-- Computed by tracing from study vertex → tau vertices → treatment vertices
-- and summing the arm variances (1/hardness of direct springs).
--
-- In the spring network:
--   study_i → τ_{ij} via tau spring (hardness 2/τ²)
--   τ_{ij}  → t_j   via direct spring (hardness 1/v_j)
-- So σ_i² = Σ_j v_j = Σ_j (1/k_j)
studyWithinVariance :: SpringNetwork -> Map.Map Int Double
studyWithinVariance spnet =
  let sprs = springs spnet
      tauEs = tauEdges spnet
      studyVs = netStudies spnet
   in Map.fromList $ map (\sv ->
        let -- tau edges from this study vertex
            tauVerts = map (\(G.Edge u v) -> if u == sv then v else u)
                     $ filter (\(G.Edge u v) -> u == sv || v == sv)
                     $ Set.toList tauEs
            -- direct springs from tau vertices to treatment vertices
            armVariances = map (\tv ->
              let directSpr = head $ Map.elems $
                    Map.filterWithKey
                      (\e@(G.Edge u v) _ -> (u == tv || v == tv) &&
                        not (Set.member e tauEs) &&
                        not (Set.member (G.reverseEdge e) tauEs))
                      sprs
               in 1 / hardness directSpr
              ) tauVerts
         in (sv, sum armVariances)
      ) studyVs

-- | Effective hardness κ_i for each study.
--
-- == Algebraic derivation (pairwise case)
--
-- Given the random effects model:
--
--   y_i | θ_i ~ N(θ_i, σ_i²)     -- likelihood
--   θ_i | μ   ~ N(μ, τ²)          -- random effects prior
--
-- The summary estimate μ̂ has variance:
--   Var(μ̂) = 1 / Σ_j w_j*   where  w_j* = 1/(σ_j² + τ²)
--
-- Since μ is estimated (not known), the effective prior on θ_i becomes:
--   θ_i ~ N(μ̂, τ² + Var(μ̂))
--
-- Combining likelihood and effective prior (product of Gaussians):
--   κ_i = 1/σ_i² + 1/(τ² + Var(μ̂))
--
-- == Spring network interpretation
--
-- Study vertex s_i connects through:
--   1. Tau springs (hardness 2/τ²) to tau vertices
--   2. Direct springs (hardness 1/v_j) to treatment vertices
--   3. Treatment vertices to the rest of the network (variance Var(μ̂))
--
-- Integrating out all intermediate vertices:
--   Series spring: var = σ_i² (direct) + τ² (tau) → but μ is free
--   The network resolves μ with precision Σ w_j*, giving Var(μ̂)
--   Effective prior variance = τ² + Var(μ̂)
--   κ_i = 1/σ_i² + 1/(τ² + Var(μ̂))
studyKeff :: SpringNetwork -> Map.Map Int Double
studyKeff spnet =
  let τ2 = tau2 spnet
      σ2s = studyWithinVariance spnet
      -- Random effects weights w_i* = 1/(σ_i² + τ²)
      ws = Map.map (\σ2 -> 1 / (σ2 + τ2)) σ2s
      -- Variance of summary estimate
      varMuHat = 1 / sum (Map.elems ws)
   in Map.map (\σ2 ->
        1 / σ2 + 1 / (τ2 + varMuHat)
      ) σ2s

-- | Study-specific prediction interval variance = 1/κ_i.
-- This is Var(θ_i | data), the posterior variance of the true study effect.
studyPredictionVariance :: SpringNetwork -> Map.Map Int Double
studyPredictionVariance spnet =
  Map.map (1 /) (studyKeff spnet)

-- | Prediction interval variance for a NEW study's true effect.
-- This is τ² + Var(μ̂), i.e., without the 1/σ_i² observation term.
predictionIntervalVariance :: SpringNetwork -> Double
predictionIntervalVariance spnet =
  let τ2 = tau2 spnet
      σ2s = studyWithinVariance spnet
      ws = Map.map (\σ2 -> 1 / (σ2 + τ2)) σ2s
      varMuHat = 1 / sum (Map.elems ws)
   in τ2 + varMuHat

-- | Contrast-level prediction interval variances for a NEW study.
-- For each treatment pair (A, B):
--   Var_predict(A,B) = Var_network(A,B) + τ²
-- This matches netmeta's formula: seTE.predict = sqrt(seTE.random² + τ²).
contrastPredictionVariances ::
  NetworkEffects ->
  Map.Map TreatmentId (Map.Map TreatmentId Double)
contrastPredictionVariances neteffs =
  let τ2 = tau2 $ springNetwork neteffs
   in Map.map (Map.map (+ τ2)) (networkVariances neteffs)

-- | I²_BLUP: fraction of τ² visible in the BLUP spread.
--
-- From the law of total variance decomposition:
--   τ² = (2/N) Σ δ_ij² + (2/N) Σ Var(δ_ij)
--        ^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^
--        among BLUPs       within (shrinkage)
--
-- I²_BLUP = among / τ² = (2/N) Σ δ_ij² / τ²
--
-- Ranges 0–1. High values mean BLUPs are barely shrunk and capture
-- most of the real heterogeneity; low values mean heavy shrinkage.
i2BLUP :: NetworkEffects -> Double
i2BLUP neteffs =
  let spnet = springNetwork neteffs
      sprs = springs spnet
      tauEs = tauEdges spnet
      studyVs = netStudies spnet
      τ2 = tau2 spnet
      allTauLengthsSq = concatMap (\sv ->
        map (\(_, s) -> springLength s ^ 2)
          $ Map.toList $ Map.filterWithKey
          (\(G.Edge u v) _ -> (u == sv || v == sv)
            && Set.member (G.Edge u v) tauEs)
          sprs
        ) studyVs
      nTotalArms = fromIntegral $ length allTauLengthsSq
      blupSpread = 2 * sum allTauLengthsSq / nTotalArms
   in if τ2 > 0 then blupSpread / τ2 else 0

-- | Classical I² = max(0, (Q - df) / Q) from Cochran's Q at fixed effects.
-- This is the same formula used by netmeta (via meta:::isquared).
classicalI2 ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] ->
  (Arm -> Either String a) ->
  Either String Double
classicalI2 studies armEffect =
  let efeNet = makeSprings studies Nothing Nothing armEffect
   in case efeNet of
        Left err -> Left err
        Right feNet ->
          let feNMA = nma feNet
              feSpnet = springNetwork feNMA
              q = 2 * totalEnergy feSpnet
              df' = fromIntegral $ dof feSpnet
           in Right $ max 0 ((q - df') / q)

-- | Newton solver for spring networks with non-linear (e.g. binomial) springs.
--
-- Each iteration is exactly one Newton step: linearise the non-linear springs
-- at the current positions (hardness = 2nd derivative of E_obs at current ℓ,
-- natural length = working response), solve the resulting Gaussian network via
-- `nma` (= solve H·δ = −g), then take step `x ← x + α·δ` with Armijo backtracking.
--
-- For canonical-link GLM springs (e.g. binomial-logit) one linearise-solve
-- cycle is algebraically identical to one Newton step with Fisher-scoring
-- information matrix — so this is IRLS viewed as Newton, with two upgrades
-- over the older irlsSolve:
--   * Armijo line search with the proper sufficient-decrease condition
--     `E(new) ≤ E(old) + c_armijo · α · g·δ`
--   * Convergence on max|gradient| (force balance) rather than step-length.
--
-- Returns `NetworkEffects` via one final `nma` on the linearised network at
-- the converged mode, identical to the old interface.
newtonSolve ::
  SpringNetwork ->
  Int ->     -- ^ max Newton iterations
  Double ->  -- ^ gradient-magnitude convergence tolerance (max|∇E|)
  NetworkEffects
newtonSolve net0 maxIter eps = go net0 0
  where
    netEnergy net = sum $ map springEnergy $ Map.elems $ springs net
    cArmijo       = 1e-4 :: Double
    maxHalvings   = 20   :: Int

    -- Build a per-node gradient and a signed node<-spring incidence to
    -- contract g·δ. Each spring on edge (u,v) contributes springForce(spr)
    -- to the force on v (= −dE/dx_v) and −springForce(spr) to u.
    -- Gradient on free node x is dE/dx = −(force on x).
    gradAtEdge spr = springForce spr   -- magnitude; applied as +/- per endpoint
    -- Max |grad| across free vertices (skipping pinned vertex).
    maxGradFree net =
      let !pinV    = fromMaybe 1 (pinned net)
          !sprsM   = springs net
          accForce !m (G.Edge u v, spr) =
            let f = gradAtEdge spr  -- dE/dx_v  =  -f  ;  dE/dx_u  = +f
                m' = IM.insertWith (+) u   f  m
            in       IM.insertWith (+) v (-f) m'
          forceMap = foldl' accForce IM.empty (Map.toList sprsM)
      in  maximum $ 0 : [ abs g | (v, g) <- IM.toList forceMap, v /= pinV ]

    -- Inner product g·δ over free nodes, where δ is the position change
    -- implied by the trial linearised-network solution.
    gDotDelta net netCand =
      let !pinV     = fromMaybe 1 (pinned net)
          !sprsOld  = springs net
          !sprsNew  = springs netCand
          accForce !m (G.Edge u v, spr) =
            let f  = gradAtEdge spr
                m' = IM.insertWith (+) u   f  m
            in       IM.insertWith (+) v (-f) m'
          forceMap = foldl' accForce IM.empty (Map.toList sprsOld)
          -- Position change for each vertex inferred from spring endpoint
          -- updates. For each edge (u,v), ℓ_new - ℓ_old = Δx_v - Δx_u.
          -- We accumulate a best-estimate per-vertex displacement by one
          -- fixed-vertex anchor (pinned vertex held at 0).
          vs = IM.keys (vs2ts net)
          dispFromBFS = dfsDisp sprsOld sprsNew pinV vs
      in  sum [ -f * IM.findWithDefault 0 v dispFromBFS
              | (v, f) <- IM.toList forceMap, v /= pinV ]

    -- Infer per-vertex displacements by a BFS from the pinned vertex along
    -- undirected spring edges, using Δx_v − Δx_u = Δℓ_edge for each edge.
    dfsDisp :: Map.Map G.Edge Spring -> Map.Map G.Edge Spring
            -> G.Vertex -> [G.Vertex] -> IM.IntMap Double
    dfsDisp sprsOld sprsNew pinV allVs =
      let edgesBoth = Map.toList sprsOld
          adj       = foldl' (\m (G.Edge u v, _) ->
                         IM.insertWith (++) u [(v, G.Edge u v)]
                       $ IM.insertWith (++) v [(u, G.Edge u v)] m
                       ) IM.empty edgesBoth
          step visited [] = visited
          step visited ((x, d):rest) =
            let nbrs = IM.findWithDefault [] x adj
                newOnes =
                  [ (y, d + dl)
                  | (y, e) <- nbrs
                  , not (IM.member y visited)
                  , let oldL = springLength (sprsOld Map.! e)
                        newL = springLength (sprsNew Map.! e)
                        -- l is length "u→v" from stored edge orientation.
                        -- If x is the u-end, Δx_v = Δx_u + Δl, so d_y = d_x + Δl.
                        -- If x is the v-end, d_y = d_x - Δl.
                        dl   = case e of
                                 G.Edge u v ->
                                   if u == x then (newL - oldL)
                                             else -(newL - oldL)
                  ]
                visited' = foldl' (\m (y, dy) -> IM.insert y dy m) visited newOnes
            in  step visited' (rest ++ newOnes)
      in  step (IM.singleton pinV 0) [(pinV, 0)]
            `IM.intersection` IM.fromList [(v, 0) | v <- allVs]

    go net !iter =
      let linNet   = linearizeNetwork net
          solved   = nma linNet
          solNet   = springNetwork solved
          sprsSol  = springs solNet
          -- Trial net at full step (α=1): adopt linear spring positions
          -- directly; for binomial springs keep (e, n) and just update ℓ.
          applyStep alpha =
            let oldSprs = springs net
                mkNew e spr =
                  let newL = springLength (sprsSol Map.! e)
                      oldL = springLength spr
                      ℓ    = oldL + alpha * (newL - oldL)
                  in case spr of
                       BinomialSpring ev ns _ -> BinomialSpring ev ns ℓ
                       Spring k ll _          -> Spring k ll ℓ
            in  net { springs = Map.mapWithKey mkNew oldSprs }
          e0        = netEnergy net
          gdelta    = gDotDelta net (applyStep 1.0)   -- at full step
          -- Armijo with backtracking
          findAlpha alpha halv =
            let cand  = applyStep alpha
                ec    = netEnergy cand
                target = e0 + cArmijo * alpha * gdelta
            in if halv >= maxHalvings || ec <= target
                 then (cand, ec)
                 else findAlpha (alpha * 0.5) (halv + 1)
          (newNet, _) = findAlpha 1.0 0
          gNorm       = maxGradFree newNet
      in  if gNorm < eps || iter >= maxIter
            then nma (linearizeNetwork newNet)
            else go newNet (iter + 1)

-- | Back-compat alias: earlier code still uses `irlsSolve`.
irlsSolve :: SpringNetwork -> Int -> Double -> NetworkEffects
irlsSolve = newtonSolve

-- | Reduced (Schur-complement) Newton for binomial spring networks.
--
-- Equivalent to 'newtonSolve' / 'irlsSolve' on the full (study + treat +
-- τ-aux) network at convergence (matches it to machine precision; see
-- @test/proto_reduced_newton.hs@ and @test/proto_gc_smoke.hs@), but
--
--   * each Newton iteration eliminates the diagonal τ-aux block
--     analytically and solves only the (k + T − 1)-dim subsystem, and
--
--   * the final 'NetworkEffects' is built directly from the converged
--     reduced Hessian, bypassing the full-network 'nma' call. Schur
--     identity guarantees that the inverse of the reduced Hessian on the
--     (S, T) block matches the (S, T) block of the full inverse, so
--     @networkVariances@ are exact.
--
-- Per-iter cost: @O((k + T)^3)@ instead of @O((k + T + A)^3)@; final
-- variance extraction also @O((k + T)^3)@ instead of @O((k + T + A)^3)@.
-- For the typical NMA with @A ≈ 2.5 k@ this is roughly an order of
-- magnitude per iter and per fit.
--
-- @studyDeltaVariances@ is left empty by this solver (only the GC log Z
-- and the contrast posterior summaries are needed downstream); REML still
-- routes through 'newtonSolve' which fills it.
--
-- Plain (un-damped) Gauss-Newton; if convergence problems show up on
-- pathological data, copy the Armijo backtracking pattern from 'newtonSolve'.
newtonSolveReducedBin :: SpringNetwork -> Int -> Double -> NetworkEffects
newtonSolveReducedBin net0 maxIter eps =
  let tau2c   = max 1e-10 (tau2 net0)
      tau2arm = tau2c / 2.0
      vmap    = vs2ts net0
      sprMap0 = springs net0

      -- vertex classification
      treatVs = [v | (v, TreatmentEnd _) <- IM.toList vmap]
      studyVs = [v | (v, StudyEnd _)     <- IM.toList vmap]
      pinV    = fromMaybe (head treatVs) (pinned net0)

      studyVByS  = Map.fromList
                     [(sid, v) | (v, StudyEnd sid) <- IM.toList vmap]

      -- Free vertices (study + free-treat) and indexing for the reduced system
      freeStudyVs = studyVs
      freeTreatVs = filter (/= pinV) treatVs
      freeVs      = freeStudyVs ++ freeTreatVs
      nFree       = length freeVs
      vIdxM       = Map.fromList (zip freeVs [0 .. nFree - 1])

      -- Per-arm topology, extracted from binomial springs.
      -- Each binomial spring lives on a tau↔treat edge; identify which
      -- endpoint is the τ-aux vertex by inspecting the vertex map.
      armRecs = [ let (tauV, tVtx) = case IM.lookup a vmap of
                        Just (TauEnd _) -> (a, b)
                        _               -> (b, a)
                      sid = case IM.lookup tauV vmap of
                        Just (TauEnd (TauId s _)) -> s
                        _ -> error "newtonSolveReducedBin: expected TauEnd"
                      sVtx = studyVByS Map.! sid
                  in (sVtx, tauV, tVtx, ev, n)
                | (G.Edge a b, BinomialSpring ev n _) <- Map.toList sprMap0 ]

      -- Initial positions: zero everywhere (matches the spring-network
      -- template, where every spring length starts at 0).
      initPos :: IM.IntMap Double
      initPos = IM.fromList [(v, 0.0) | v <- IM.keys vmap]

      logistic x = 1 / (1 + exp (-x))
      clamp lo hi x = max lo (min hi x)

      -- Exact total energy at a candidate position.  Used by Armijo damping
      -- to detect overshoots at large τ² with rare events.
      -- E = Σ_arms [ n·log(1+e^l) − ev·l ]   (binomial obs)
      --   + Σ_arms u² / (2·τ²_arm)            (τ-spring)
      energyAt p =
        sum [ let xS   = p IM.! sV
                  xT   = if tV == pinV then 0 else p IM.! tV
                  xTau = p IM.! tauV
                  l    = xT - xTau
                  u    = xTau - xS
                  n_   = fromIntegral n
                  e_   = fromIntegral ev
                  lp1  = if l >= 0
                           then l + log (1 + exp (-l))
                           else log (1 + exp l)
              in n_ * lp1 - e_ * l + u*u / (2.0 * tau2arm)
            | (sV, tauV, tV, ev, n) <- armRecs ]

      -- Mix two positions: (1−α)·pos + α·newPos vertex-wise.
      mix2 α pos newPos =
        IM.intersectionWith (\old new -> (1 - α) * old + α * new) pos newPos

      -- One reduced Newton step.
      stepNewton pos =
        let perArm = [ let xS   = pos IM.! sV
                           xT   = if tV == pinV then 0 else pos IM.! tV
                           xTau = pos IM.! tauV
                           l    = xT - xTau                -- spring length convention
                           p    = logistic l
                           n_   = fromIntegral n
                           kMin = max 1e-6 (n_ * 1e-10)
                           w    = max kMin (n_ * p * (1 - p))
                           resid = fromIntegral ev - n_ * p
                           yLin = l + clamp (-5.0) 5.0 (resid / w)
                           kEff = 1.0 / (tau2arm + 1.0 / w)
                       in (sV, tauV, tV, w, yLin, kEff)
                     | (sV, tauV, tV, ev, n) <- armRecs ]

            -- Reduced (study + free-treat) system.
            addArm (am, bm) (sV, _, tV, _, yL, kE) =
              let s = vIdxM Map.! sV
              in if tV == pinV
                   then ([((s, s), kE)] ++ am, [(s, -kE * yL)] ++ bm)
                   else
                     let t = vIdxM Map.! tV
                     in ([((s, s), kE), ((t, t), kE),
                          ((s, t), -kE), ((t, s), -kE)] ++ am
                        , [(s, -kE * yL), (t, kE * yL)] ++ bm)
            (entries, bEntries) = foldl' addArm ([], []) perArm
            aMap = Map.fromListWith (+) entries
            bMap = Map.fromListWith (+) bEntries
            aMat = Mat.fromLists
                     [ [ Map.findWithDefault 0 (i, j) aMap
                       | j <- [0 .. nFree - 1] ]
                     | i <- [0 .. nFree - 1] ]
            bMat = Mat.reshape nFree 1
                     [ Map.findWithDefault 0 i bMap
                     | i <- [0 .. nFree - 1] ]
            sol  = Mat.toList (Mat.flatten (Mat.inv aMat `matMul` bMat))

            posST = IM.fromList
                      [ (v, sol !! (vIdxM Map.! v)) | v <- freeVs ]
            -- Recover τ-aux positions in closed form from the linearised
            -- per-arm equilibrium (diagonal τ-block makes this exact).
            posTau = IM.fromList
                       [ (tauV,
                          let xS = posST IM.! sV
                              xT = if tV == pinV then 0 else posST IM.! tV
                          in (xS / tau2arm + w * (xT - yL))
                             / (1 / tau2arm + w))
                       | (sV, tauV, tV, w, yL, _) <- perArm ]
            newPos = IM.union (IM.union posTau posST)
                              (IM.singleton pinV 0.0)

            -- Convergence δ measured externally by `stepDelta` so the same
            -- function works for both the full and damped steps.
        in (newPos, stepDelta pos newPos)

      -- Armijo-damped Newton step: try full Newton, halve until energy does
      -- not increase.  Bounded backtracking iterations.  Critical at large τ²
      -- with rare events, where the full Newton step can overshoot the true
      -- mode badly enough that the next iteration's linearisation diverges.
      stepDelta pos newPos =
        maximum $ 0.0 :
          [ let oldL = (if tV == pinV then 0 else pos    IM.! tV)
                          - (pos    IM.! tauV)
                newL = (if tV == pinV then 0 else newPos IM.! tV)
                          - (newPos IM.! tauV)
            in abs (newL - oldL)
          | (_, tauV, tV, _, _) <- armRecs ]

      stepDamped pos =
        let (newPos, _) = stepNewton pos
            e0 = energyAt pos
            tryStep α
              | α < 1.0 / 64 = (newPos, stepDelta pos newPos)
              | otherwise =
                  let p = if α >= 1.0 then newPos else mix2 α pos newPos
                      e = energyAt p
                  in if e <= e0 + 1e-12
                       then (p, stepDelta pos p)
                       else tryStep (α * 0.5)
        in tryStep 1.0

      go !iter !pos
        | iter >= maxIter = pos
        | otherwise =
            let (pos', !δ) = stepDamped pos
            in if δ < eps then pos' else go (iter + 1) pos'

      finalPos = go 0 initPos

      -- Update binomial spring lengths to the converged @x_T - x_τ@; leave
      -- Gaussian τ-springs untouched (their length doesn't matter once
      -- the network is linearised, as @nma@ uses hardness + natural length).
      -- Spring length convention: for an edge G.Edge u v the spring length
      -- is pos(v) - pos(u). At convergence, set every spring's stored length
      -- to that exact difference, so springEnergy and lookups produce the
      -- same values that 'nma' would have written.
      finalSprings =
        Map.mapWithKey
          (\(G.Edge u v) spr ->
            let posOf vtx
                  | vtx == pinV = 0
                  | otherwise   = IM.findWithDefault 0 vtx finalPos
                newLen = posOf v - posOf u
            in case spr of
                 BinomialSpring ev n _ -> BinomialSpring ev n newLen
                 Spring k ll _         -> Spring k ll newLen)
          sprMap0

      converged = net0 { springs = finalSprings }
      linearised = linearizeNetwork converged

      ----------------------------------------------------------------
      -- Build NetworkEffects directly from the reduced Hessian.
      -- Avoids the O((k+T+A)^3) full-network @nma@ variance pass.
      ----------------------------------------------------------------

      -- Re-evaluate the linearisation + reduced Hessian at finalPos.
      -- (We run one extra build to get aMat materialised; the iteration
      -- itself didn't keep it.)
      finalPerArm =
        [ let xS   = finalPos IM.! sV
              xT   = if tV == pinV then 0 else finalPos IM.! tV
              xTau = finalPos IM.! tauV
              l    = xT - xTau
              p    = logistic l
              n_   = fromIntegral n
              kMin = max 1e-6 (n_ * 1e-10)
              w    = max kMin (n_ * p * (1 - p))
              kEff = 1.0 / (tau2arm + 1.0 / w)
          in (sV, tauV, tV, w, kEff)
        | (sV, tauV, tV, ev, n) <- armRecs ]

      finalEntries =
        concatMap (\(sV, _, tV, _, kE) ->
          let s = vIdxM Map.! sV
          in if tV == pinV
               then [((s, s), kE)]
               else
                 let t = vIdxM Map.! tV
                 in [((s, s), kE), ((t, t), kE),
                     ((s, t), -kE), ((t, s), -kE)]
        ) finalPerArm
      finalAMap = Map.fromListWith (+) finalEntries
      finalAMat = Mat.fromLists
                    [ [ Map.findWithDefault 0 (i, j) finalAMap
                      | j <- [0 .. nFree - 1] ]
                    | i <- [0 .. nFree - 1] ]
      aInv = Mat.inv finalAMat

      -- Treatment-vertex → reduced-system column index.
      treatVtxToIdx = Map.fromList [(v, vIdxM Map.! v) | v <- freeTreatVs]

      -- Treatment positions: pinned treatment at 0, others read from finalPos.
      treatPos v = if v == pinV then 0 else finalPos IM.! v

      -- All treatments (pinned + free) with their TreatmentId.
      treatList =
        [ (v, tid) | (v, TreatmentEnd tid) <- IM.toList vmap ]

      -- Effect a → b = treatPos[b] − treatPos[a].
      -- Variance of (treatPos[b] − treatPos[a]) from aInv.
      effEst aV bV = treatPos bV - treatPos aV
      effVar aV bV
        | aV == bV          = 0
        | aV == pinV =
            let bi = treatVtxToIdx Map.! bV
            in Mat.atIndex aInv (bi, bi)
        | bV == pinV =
            let ai = treatVtxToIdx Map.! aV
            in Mat.atIndex aInv (ai, ai)
        | otherwise =
            let ai = treatVtxToIdx Map.! aV
                bi = treatVtxToIdx Map.! bV
            in Mat.atIndex aInv (ai, ai)
             + Mat.atIndex aInv (bi, bi)
             - 2 * Mat.atIndex aInv (ai, bi)

      netests = Map.fromList
        [ (tidA,
           Map.fromList [(tidB, effEst vA vB) | (vB, tidB) <- treatList])
        | (vA, tidA) <- treatList ]
      netvars = Map.fromList
        [ (tidA,
           Map.fromList [(tidB, effVar vA vB) | (vB, tidB) <- treatList])
        | (vA, tidA) <- treatList ]

      -- Per-arm posterior variance of the random effect u_ij = x_τ − x_S.
      -- After the analytic τ-aux elimination at the linearised mode:
      --   x_τ = (1 − β) x_S + β x_T − β ℓ₀,  β = w · τ²_arm / (1 + w · τ²_arm)
      -- Conditional on (x_S, x_T):  E[x_τ | …] = (1 − β) x_S + β x_T − β ℓ₀
      --                              Var(x_τ | …) = 1 / (1/τ²_arm + w) = β · τ²_arm
      -- Law of total variance →
      --   Var(u_ij | y) = β² · Var(x_T_j − x_S_i | y)  +  β · τ²_arm
      --                  (marginal-of-mode part)         (within-conditional part)
      -- The Var(x_T − x_S | y) part comes from aInv via the Schur identity.
      armDeltaVar (sV, _, tV, w, _) =
        let beta = w * tau2arm / (1 + w * tau2arm)
            sIdx = vIdxM Map.! sV
            varST
              | tV == pinV = Mat.atIndex aInv (sIdx, sIdx)
              | otherwise  =
                  let tIdx = vIdxM Map.! tV
                  in Mat.atIndex aInv (tIdx, tIdx)
                   + Mat.atIndex aInv (sIdx, sIdx)
                   - 2 * Mat.atIndex aInv (sIdx, tIdx)
        in (sV, beta * beta * varST + beta * tau2arm)

      studyDV =
        IM.fromListWith (++)
          [ (sV, [v]) | row <- map armDeltaVar finalPerArm
                      , let (sV, v) = row ]

      -- Empty StudyGraph for the directs field — none of the GC pipeline
      -- consumers read it.
      emptySG = StudyGraph
        { studyGraph  = G.graphFromEdges []
        , directGraph = G.graphFromEdges []
        , vsts        = IM.empty
        , tsvs        = Map.empty
        }
  in NetworkEffects
       { directs              = emptySG
       , networkEstimates     = netests
       , networkVariances     = netvars
       , studyDeltaVariances  = studyDV
       , springNetwork        = linearised
       , debugmsg             = "newtonSolveReducedBin"
       }

-- | Spring REML for binary data using IRLS + EM.
-- Outer loop: EM for τ² estimation.
-- Inner loop: IRLS to solve the non-linear spring network at fixed τ².
--
-- When 'applyCorrection' is True, applies a second-order Laplace correction
-- to the EM M-step.  The Gaussian (Laplace) approximation to E[u²|y] misses
-- the posterior skewness/kurtosis induced by the binomial likelihood.
-- Correction per arm (Laplace expansion of E[u²|y] beyond Gaussian):
--   Δ = û·A₃·v² + A₄·v³/2 + 5·A₃²·v⁴/4
-- where û = tau spring length (posterior mode), v = 1/(w + 2/τ²),
-- A₃ = ℓ'''(û) = −w(1−2p), A₄ = ℓ''''(û) = −w(1−6p(1−p)), w = n·p·(1−p).
springREMLBin ::
  [Study] ->
  Maybe TauSquare ->  -- ^ initial τ² guess
  Maybe Int ->        -- ^ max outer iterations
  Maybe Double ->     -- ^ convergence tolerance
  Either String (NetworkEffects, (Double, [Double]))
springREMLBin studies mtau0 miters mε =
  springREMLBinOpt False studies mtau0 miters mε

-- | Spring REML for binary data with an optional second-order Laplace
-- correction.  See 'springREMLBin' for details.
springREMLBinOpt ::
  Bool ->             -- ^ apply second-order correction
  [Study] ->
  Maybe TauSquare ->
  Maybe Int ->
  Maybe Double ->
  Either String (NetworkEffects, (Double, [Double]))
springREMLBinOpt applyCorrection studies mtau0 miters mε =
  let iters = fromMaybe 1000 miters
      ε = fromMaybe 1e-4 mε
      irlsIters = 50
      irlsEps = 1e-8
      -- Build fixed-effect network (τ²=0) to get initial positions
      espringnetFE = makeBinomialSprings studies Nothing Nothing
      -- Build network with τ²
      mkNet t2 = makeBinomialSprings studies Nothing (Just t2)
   in case (espringnetFE, mkNet 0.5) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right fenet, Right springsnet0) ->
          let -- Capture original binomial springs by edge — these are lost
              -- after IRLS linearizes them, but we need (events, n) to
              -- compute the Laplace correction at each EM iteration.
              binSprMap :: Map.Map G.Edge (Int, Int)
              binSprMap = Map.mapMaybe
                (\s -> case s of
                         BinomialSpring ev ns _ -> Just (ev, ns)
                         _ -> Nothing)
                (springs springsnet0)
              -- Compute initial τ² guess: solve at fixed effect, then DL-style
              feEffs = irlsSolve fenet irlsIters irlsEps
              feSpnet = springNetwork feEffs
              feSprs = springs feSpnet
              -- Approximate study variances from linearized FE springs
              studyVarsApprox = map (\sv ->
                let svSprs = Map.elems $ Map.filterWithKey
                      (\(G.Edge u v) _ -> u == sv || v == sv) feSprs
                    ks = map hardness svSprs
                 in if length ks == 0 then 1.0 else sum (map (1/) ks) / fromIntegral (length ks)
                ) (netStudies feSpnet)
              -- τ²_max: upper bound for clamping
              tau2Max = max 10.0 (10.0 * maximum (1.0 : studyVarsApprox))
              -- DL-style initial guess from FE residuals
              guess0DL =
                let q = 2 * totalEnergy feSpnet
                    df' = fromIntegral $ dof feSpnet
                    c = df'  -- simplified: sum of weights ≈ df for balanced designs
                 in if q > df' && c > 0 then min tau2Max (max 0 ((q - df') / c)) else 0.5
              guess0 = case mtau0 of
                         Just t -> t
                         Nothing -> max 0.1 (min tau2Max guess0DL)
              -- EM step: given current τ² and network, solve IRLS and compute new τ²
              emStep t2 prevNet =
                let netWithTau = updateSprings prevNet t2
                    neteffs = irlsSolve netWithTau irlsIters irlsEps
                    spnet = springNetwork neteffs
                    sprs = springs spnet
                    tauEs = tauEdges spnet
                    studyVs = netStudies spnet
                    allTauLengthsSq = concatMap (\sv ->
                      map (\(_, s) -> springLength s ^ 2)
                        $ Map.toList $ Map.filterWithKey
                        (\(G.Edge u v) _ -> (u == sv || v == sv)
                          && Set.member (G.Edge u v) tauEs)
                        sprs
                      ) studyVs
                    deltaVarMap = studyDeltaVariances neteffs
                    allVarDeltas = concatMap (\sv ->
                      IM.findWithDefault [] sv deltaVarMap
                      ) studyVs
                    -- Optional second-order Laplace correction for binomial posterior.
                    -- Uses binSprMap (captured from the original, pre-IRLS network)
                    -- to recover (events, n) per binomial edge, since IRLS linearizes
                    -- BinomialSpring into Spring and loses that info.
                    laplaceCorr =
                      if not applyCorrection then 0
                      else sum $ concatMap (\sv ->
                        let tauSprsHere = Map.toList $ Map.filterWithKey
                              (\(G.Edge u v) _ -> (u == sv || v == sv)
                                && Set.member (G.Edge u v) tauEs)
                              sprs
                         in map (\(G.Edge u v, tauSpr) ->
                              let tauV = if u == sv then v else u
                                  uHat = springLength tauSpr
                                  mBin = listToMaybe
                                    [(e, ens) | (e@(G.Edge a _), ens) <- Map.toList binSprMap
                                              , a == tauV ]
                               in case mBin of
                                    Just (binE, (_, ns)) ->
                                      let binLen = springLength (sprs Map.! binE)
                                          p = sigmoid binLen
                                          w = fromIntegral ns * p * (1 - p)
                                          hPost = w + 2 / t2
                                          pv = 1 / hPost
                                          -- A3 = ℓ'''(û) = -w(1-2p), A4 = -w(1-6p(1-p))
                                          -- Δ E[u²|y] = û·A3·v² + A4·v³/2 + 5·A3²·v⁴/4
                                          bigA3 = negate $ w * (1 - 2*p)
                                          bigA4 = negate $ w * (1 - 6*p*(1-p))
                                       in uHat * bigA3 * pv*pv
                                          + bigA4 * pv*pv*pv / 2
                                          + 5 * bigA3*bigA3 * pv*pv*pv*pv / 4
                                    _ -> 0
                           ) tauSprsHere
                        ) studyVs
                    nTotalArms = fromIntegral $ length allTauLengthsSq
                    rawTau2 = max 0 $ 2 * (sum allTauLengthsSq / nTotalArms
                                           + sum allVarDeltas / nTotalArms
                                           + laplaceCorr / nTotalArms)
                    -- Clamp to [0, tau2Max]
                    newTau2 = min tau2Max rawTau2
                 in (newTau2, spnet)
              -- Dampen large jumps: if τ² changes by more than 2x, use geometric mean
              dampen t2Old t2New
                | t2Old <= 0 = t2New
                | t2New / t2Old > 2.0 = sqrt (t2Old * t2New)
                | t2Old / t2New > 2.0 = sqrt (t2Old * t2New)
                | otherwise = t2New
              -- Detect oscillation: count direction changes in recent path
              isOscillating path =
                case path of
                  (a:b:c:_) -> (a - b) * (b - c) < 0
                  _ -> False
              -- Iteration loop
              step t2 net path iter
                | abs (t2_damped - t2) <= ε || iter > iters =
                    let finalNet = updateSprings net' t2_damped
                        neteffs' = irlsSolve finalNet irlsIters irlsEps
                     in (neteffs', (t2_damped, reverse (t2_damped:path)))
                | otherwise = step t2_damped net' (t2_damped:path) (iter + 1)
                where
                  (t2_em, net') = emStep t2 net
                  -- Apply damping and oscillation averaging
                  t2_dampedRaw = dampen t2 t2_em
                  t2_damped
                    | isOscillating (t2_dampedRaw : path) =
                        -- Average the last two values to stabilize
                        case path of
                          (prev:_) -> (t2_dampedRaw + prev) / 2.0
                          _ -> t2_dampedRaw
                    | otherwise = t2_dampedRaw
           in Right (step guess0 springsnet0 [] 1)


-- | Grand canonical spring ensemble: exact posterior of τ² by summing
-- the partition function Z over chain lengths N = 0..nMax.
--
-- Each chain length N corresponds to N identical tau-springs in series
-- between arm nodes and treatment nodes, giving effective τ² = N/kTau.
--
-- At each τ², the arm/tau nodes are integrated out analytically:
--   * Two springs in series (obs: 1/v, tau: 1/τ²) give effective
--     hardness 1/(v+τ²) between study and treatment nodes.
--   * Integrating out each arm node produces a prefactor -½ log(v+τ²).
--
-- The partition function of the effective network is:
--   log Z(τ²) = -½ Σ_arms log(v_ij + τ²) - E*(τ²) - ½ log|A_eff(τ²)|
--
-- where E* = spring energy at equilibrium of the effective network,
-- A_eff = stiffness matrix with study + treatment nodes only,
-- effective spring hardness 1/(v_ij + τ²) per arm.
--
-- Returns: (mode_tau2, [(tau2, logZ)], NetworkEffects_at_mode)
springGrandCanonical ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] ->
  (Arm -> Either String a) ->
  Double ->   -- kTau: individual tau spring hardness (resolution = 1/kTau)
  Int ->      -- nMax: maximum chain length
  Either String (Double, [(Double, Double)], NetworkEffects)
springGrandCanonical studies armEffect kTau nMax =
  let espringnet = makeSprings studies Nothing (Just 1.0) armEffect
  in case espringnet of
    Left err -> Left err
    Right springsnet0 ->
      let vmap = vs2ts springsnet0
          -- Identify node types
          treatVs = [v | (v, TreatmentEnd _) <- IM.toList vmap]
          studyVs = [v | (v, StudyEnd _)     <- IM.toList vmap]
          -- Pin first treatment (reference)
          pinnedV = head treatVs
          -- Free study + treatment nodes (not pinned)
          freeVs = filter (/= pinnedV) (studyVs ++ treatVs)
          nFree  = length freeVs
          vIdx   = Map.fromList $ zip freeVs [0 .. nFree - 1]

          -- Extract arm info directly from the studies and the vertex map
          -- For each arm: (study vertex, treatment vertex, v_ij, y_ij)
          -- where v_ij = arm variance, y_ij = arm mean (natural length)
          studyVtxMap = Map.fromList [(sid, v) | (v, StudyEnd sid) <- IM.toList vmap]
          treatVtxMap = Map.fromList [(tid, v) | (v, TreatmentEnd tid) <- IM.toList vmap]

          armInfo = concatMap (\study ->
            let sid  = getStudyId study
                arms = getStudyArms study
                sVtx = studyVtxMap Map.! sid
            in map (\arm ->
                let tid  = tidOfArm arm
                    tVtx = treatVtxMap Map.! tid
                    -- Compute arm-level effect
                    armEff = armEffect arm
                in case armEff of
                  Right eff ->
                    let vi = variance eff
                        yi = expectation eff
                    in (sVtx, tVtx, vi, yi)
                  Left _ -> (sVtx, tVtx, 1.0, 0.0)  -- fallback
              ) arms
            ) studies

          -- Evaluate logZ at a given τ²_contrast (same convention as springREML).
          -- updateSprings expects contrast-space τ² (divides by 2 internally).
          -- Effective arm variance = v_ij + τ²_arm = v_ij + τ²_contrast/2.
          evalLogZ t2contrast =
            let tau2c = max 1e-10 t2contrast
                tau2arm = tau2c / 2.0
                -- Build effective stiffness matrix: study + treatment nodes
                -- Each arm contributes a spring with k_eff = 1/(v+τ²_arm)
                aInit = Mat.fromLists
                          [ [ 0 | _ <- [0..nFree-1] ] | _ <- [0..nFree-1] ]
                bInit = replicate nFree 0.0

                (aMat, bVec) = foldl' (\(a, b) (sNode, tNode, vi, yi) ->
                  let kEff = 1.0 / (vi + tau2arm)
                      sIdx = Map.lookup sNode vIdx
                      tIdx = Map.lookup tNode vIdx
                      -- tNode might be pinned (reference treatment)
                  in case (sIdx, tIdx) of
                    (Just si, Just ti) ->
                      let a' = accum2 a [((si,si), kEff), ((ti,ti), kEff),
                                         ((si,ti), -kEff), ((ti,si), -kEff)]
                          b' = accumV b [(si, -kEff * yi), (ti, kEff * yi)]
                      in (a', b')
                    (Just si, Nothing) ->  -- treatment pinned at 0
                      let a' = accum2 a [((si,si), kEff)]
                          b' = accumV b [(si, -kEff * yi)]
                      in (a', b')
                    _ -> (a, b)
                  ) (aInit, bInit) armInfo

                -- Solve for equilibrium: A x = b
                bMat = Mat.reshape nFree 1 bVec
                sol  = Mat.flatten (Mat.inv aMat `matMul` bMat)
                solList = Mat.toList sol

                -- Energy at equilibrium
                eStar = sum $ map (\(sNode, tNode, vi, yi) ->
                  let kEff = 1.0 / (vi + tau2arm)
                      sPos = case Map.lookup sNode vIdx of
                               Just si -> solList !! si
                               Nothing -> 0
                      tPos = case Map.lookup tNode vIdx of
                               Just ti -> solList !! ti
                               Nothing -> 0  -- pinned
                  in 0.5 * kEff * (tPos - sPos - yi) ^ 2
                  ) armInfo

                -- Log determinant
                logDetA = log (abs (Mat.det aMat))

                -- Arm prefactor from integrating out arm/tau nodes
                armPrefactor = -0.5 * sum (map (\(_, _, vi, _) -> log (vi + tau2arm)) armInfo)

                logZ = armPrefactor - eStar - 0.5 * logDetA

                -- Also solve the full spring network for treatment effects
                net = updateSprings springsnet0 tau2c
                neteffs = nma net
            in (logZ, neteffs)

          -- Sweep chain lengths (τ²_contrast = N/kTau)
          tau2Vals = map (\n -> fromIntegral n / kTau) [0..nMax]
          results = map (\t2 -> let (lz, ne) = evalLogZ t2  -- t2 is contrast-space
                                in (t2, lz, ne)) tau2Vals
          logZs = map (\(_, lz, _) -> lz) results
          maxLZ = maximum logZs
          unnorm = map (\lz -> exp (lz - maxLZ)) logZs
          totalMass = sum unnorm
          probs = map (/ totalMass) unnorm
          -- Mode
          modeIdx = snd $ maximum $ zip probs [0..]
          (modeTau2, _, modeNE) = results !! modeIdx
          grid = zip tau2Vals logZs
      in Right (modeTau2, grid, modeNE)

-- | Same as 'springGrandCanonical' but evaluates Z at an arbitrary list of
-- τ²_contrast values (any order/spacing) instead of a uniform grid. Used
-- by 'springGrandCanonicalAdaptive'.
--
-- Returns @(modeTau2, [(τ², logZ, NetworkEffects)])@ where modeTau2 is the
-- argmax of logZ over the supplied grid.
evalXiGaussianAt ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] ->
  (Arm -> Either String a) ->
  [Double] ->                 -- ^ τ²_contrast values to evaluate at
  Either String (Double, [(Double, Double, NetworkEffects)])
evalXiGaussianAt studies armEffect tau2Vals =
  let espringnet = makeSprings studies Nothing (Just 1.0) armEffect
  in case espringnet of
    Left err -> Left err
    Right springsnet0 ->
      let vmap = vs2ts springsnet0
          treatVs = [v | (v, TreatmentEnd _) <- IM.toList vmap]
          studyVs = [v | (v, StudyEnd _)     <- IM.toList vmap]
          pinnedV = head treatVs
          freeVs  = filter (/= pinnedV) (studyVs ++ treatVs)
          nFree   = length freeVs
          vIdx    = Map.fromList $ zip freeVs [0 .. nFree - 1]
          studyVtxMap = Map.fromList [(sid, v) | (v, StudyEnd sid)     <- IM.toList vmap]
          treatVtxMap = Map.fromList [(tid, v) | (v, TreatmentEnd tid) <- IM.toList vmap]
          armInfo = concatMap (\study ->
            let sid  = getStudyId study
                arms = getStudyArms study
                sVtx = studyVtxMap Map.! sid
            in map (\arm ->
                let tid  = tidOfArm arm
                    tVtx = treatVtxMap Map.! tid
                    armEff = armEffect arm
                in case armEff of
                  Right eff -> (sVtx, tVtx, variance eff, expectation eff)
                  Left _    -> (sVtx, tVtx, 1.0, 0.0)
              ) arms
            ) studies

          evalLogZ t2contrast =
            let tau2c   = max 1e-10 t2contrast
                tau2arm = tau2c / 2.0
                aInit = Mat.fromLists
                          [ [ 0 | _ <- [0..nFree-1] ] | _ <- [0..nFree-1] ]
                bInit = replicate nFree 0.0
                (aMat, bVec) = foldl' (\(a, b) (sNode, tNode, vi, yi) ->
                  let kEff = 1.0 / (vi + tau2arm)
                      sIdx = Map.lookup sNode vIdx
                      tIdx = Map.lookup tNode vIdx
                  in case (sIdx, tIdx) of
                    (Just si, Just ti) ->
                      let a' = accum2 a [((si,si), kEff), ((ti,ti), kEff),
                                         ((si,ti), -kEff), ((ti,si), -kEff)]
                          b' = accumV b [(si, -kEff * yi), (ti, kEff * yi)]
                      in (a', b')
                    (Just si, Nothing) ->
                      let a' = accum2 a [((si,si), kEff)]
                          b' = accumV b [(si, -kEff * yi)]
                      in (a', b')
                    _ -> (a, b)
                  ) (aInit, bInit) armInfo
                bMat = Mat.reshape nFree 1 bVec
                sol  = Mat.flatten (Mat.inv aMat `matMul` bMat)
                solList = Mat.toList sol
                eStar = sum $ map (\(sNode, tNode, vi, yi) ->
                  let kEff = 1.0 / (vi + tau2arm)
                      sPos = case Map.lookup sNode vIdx of
                               Just si -> solList !! si; Nothing -> 0
                      tPos = case Map.lookup tNode vIdx of
                               Just ti -> solList !! ti; Nothing -> 0
                  in 0.5 * kEff * (tPos - sPos - yi) ^ (2::Int)
                  ) armInfo
                logDetA      = log (abs (Mat.det aMat))
                armPrefactor = -0.5 * sum
                  (map (\(_, _, vi, _) -> log (vi + tau2arm)) armInfo)
                logZ = armPrefactor - eStar - 0.5 * logDetA
                neteffs = nma (updateSprings springsnet0 tau2c)
            in (logZ, neteffs)

          results = map (\t2 -> let (lz, ne) = evalLogZ t2 in (t2, lz, ne)) tau2Vals
          logZs   = map (\(_, lz, _) -> lz) results
          maxLZ   = maximum logZs
          unnorm  = map (\lz -> exp (lz - maxLZ)) logZs
          totalM  = sum unnorm
          probs   = map (/ totalM) unnorm
          modeIdx = snd $ maximum $ zip probs [0..]
          (modeTau2, _, _) = results !! modeIdx
      in Right (modeTau2, results)

-- ============================================================
-- τ² prior + posterior reweight from a fitted Ξ grid
-- ============================================================

-- | Common τ-priors. All densities are written in τ²-space (via Jacobian
-- |dτ/dτ²| = 1/(2τ)) so they can be applied directly to (τ², logZ) grids.
-- Constants are dropped (cancel under normalisation).
data Tau2Prior
  = HalfNormalTau Double      -- ^ τ ~ HalfNormal(0, σ);   σ here is the τ-scale.
  | HalfCauchyTau Double      -- ^ τ ~ HalfCauchy(0, σ).
  | UniformTau2 Double Double -- ^ τ² ~ Uniform(a, b).
  | FlatTau2                  -- ^ improper flat in τ² (default for raw GC argmax).
  deriving Show

-- | log π(τ²) for any 'Tau2Prior'. Drops constants.
logTau2Prior :: Tau2Prior -> Double -> Double
logTau2Prior _ t2 | t2 <= 0 = -1e30
logTau2Prior (HalfNormalTau s) t2 =
  -0.5 * log t2 - 0.5 * t2 / (s * s)
logTau2Prior (HalfCauchyTau s) t2 =
  -0.5 * log t2 - log (1 + t2 / (s * s))
logTau2Prior (UniformTau2 a b) t2
  | t2 < a || t2 > b = -1e30
  | otherwise        = 0
logTau2Prior FlatTau2 _ = 0

-- | τ² posterior summary.
data PosteriorSummary = PosteriorSummary
  { psMode   :: Double        -- ^ argmax over the supplied grid (after prior)
  , psMedian :: Double        -- ^ weighted-median τ²
  , psMean   :: Double        -- ^ posterior mean τ²
  , psCi95   :: (Double, Double)  -- ^ 95 % credible interval (q_0.025, q_0.975)
  } deriving Show

-- | Reweight a fitted Ξ grid under any τ² prior, producing posterior summaries.
-- The first argument indicates whether the grid is log-spaced in τ² (true for
-- 'springGrandCanonicalAdaptive', false for the uniform 'springGrandCanonical').
-- Log-spaced grids include a Jacobian factor τ² = exp(log τ²) so the
-- trapezoidal sum approximates ∫ p(τ²) dτ² correctly.
posteriorUnder ::
  Bool ->                              -- ^ True if grid is log-spaced in τ²
  [(Double, Double)] ->                -- ^ (τ², logZ)
  Tau2Prior ->
  PosteriorSummary
posteriorUnder logSpaced grid prior =
  let pts = [(t, lz) | (t, lz) <- grid, t > 0]
      ts  = [ t | (t, _) <- pts ]
      -- log p(τ² | y) ∝ logZ + log_prior — density on natural Lebesgue
      -- scale of τ². Mode = argmax of THIS, no Jacobian shift.
      logDensity = [ lz + logTau2Prior prior t | (t, lz) <- pts ]
      -- Trapezoid weights for log-spaced grids include the Jacobian
      -- dτ²/dlog τ² = τ², used for CDF-based summaries. Adding +log τ² to
      -- the mode argmax would otherwise bias it toward larger τ².
      lps = [ ld + (if logSpaced then log t else 0)
            | (ld, t) <- zip logDensity ts ]
      m   = maximum lps
      ws  = [ exp (lp - m) | lp <- lps ]
      tot = sum ws
      ws' = [ w / tot | w <- ws ]
      modeIdx = snd $ maximum (zip logDensity [0 :: Int ..])
      mode_   = ts !! modeIdx
      mean_   = sum (zipWith (*) ts ws')
      sorted  = let pairs = zip ts ws'
                in [(t, w) | (t, w) <- pairs] -- already in grid order; for safety, sort:
      sortedPairs = mergeSort sorted
      cums    = scanl1 (+) (map snd sortedPairs)
      tsSorted = map fst sortedPairs
      wQuantile q =
        let ix = length (takeWhile (< q) cums)
        in tsSorted !! min ix (length tsSorted - 1)
  in PosteriorSummary
       { psMode   = mode_
       , psMedian = wQuantile 0.5
       , psMean   = mean_
       , psCi95   = (wQuantile 0.025, wQuantile 0.975)
       }
  where
    -- naive sort; grid sizes here are < 1000 points
    mergeSort = sortOn fst
    sortOn f xs = map snd $ sortPairs [(f x, x) | x <- xs]
      where sortPairs = sortByKey
            sortByKey = foldr ins []
            ins p [] = [p]
            ins p (q:qs) | fst p <= fst q = p : q : qs
                         | otherwise      = q : ins p qs

-- | Adaptive-grid Gaussian Ξ. Finds REML mode via 1-D bisection on
-- d log Z / dτ² (computed by central finite difference of two logZ
-- evaluations), then places nPts log-spaced grid points covering
-- [REML/1000, 4·REML]. Reduces evaluation count from ~3000 (typical
-- dense grid) to ~20 + nPts. Posterior summaries match the dense
-- version to ~1% on median/mean for nPts ≥ 50.
--
-- The user reweights by their chosen τ² prior at consumption time
-- (HalfNormal/Jeffreys/etc.) using the returned grid.
springGrandCanonicalAdaptive ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] ->
  (Arm -> Either String a) ->
  Int ->                  -- ^ number of adaptive grid points (~50 recommended)
  Either String (Double, [(Double, Double, NetworkEffects)])
springGrandCanonicalAdaptive studies armEffect nPts =
  let espringnet = makeSprings studies Nothing (Just 1.0) armEffect
  in case espringnet of
    Left err -> Left err
    Right springsnet0 ->
      let -- ============== shared setup (built ONCE) ==============
          vmap        = vs2ts springsnet0
          treatVs     = [v | (v, TreatmentEnd _) <- IM.toList vmap]
          studyVs     = [v | (v, StudyEnd _)     <- IM.toList vmap]
          pinnedV     = head treatVs
          freeVs      = filter (/= pinnedV) (studyVs ++ treatVs)
          nFree       = length freeVs
          vIdx        = Map.fromList $ zip freeVs [0 .. nFree - 1]
          studyVtxMap = Map.fromList [(sid, v) | (v, StudyEnd sid)     <- IM.toList vmap]
          treatVtxMap = Map.fromList [(tid, v) | (v, TreatmentEnd tid) <- IM.toList vmap]
          armInfo = concatMap (\study ->
            let sid  = getStudyId study
                arms = getStudyArms study
                sVtx = studyVtxMap Map.! sid
            in map (\arm ->
                let tid  = tidOfArm arm
                    tVtx = treatVtxMap Map.! tid
                    armEff = armEffect arm
                in case armEff of
                  Right eff -> (sVtx, tVtx, variance eff, expectation eff)
                  Left _    -> (sVtx, tVtx, 1.0, 0.0)
              ) arms
            ) studies

          -- evalLogZ closes over the cached setup. Call once per τ².
          evalLogZ t2contrast =
            let tau2c   = max 1e-10 t2contrast
                tau2arm = tau2c / 2.0
                aInit = Mat.fromLists
                          [ [ 0 | _ <- [0..nFree-1] ] | _ <- [0..nFree-1] ]
                bInit = replicate nFree 0.0
                (aMat, bVec) = foldl' (\(a, b) (sNode, tNode, vi, yi) ->
                  let kEff = 1.0 / (vi + tau2arm)
                      sIdx = Map.lookup sNode vIdx
                      tIdx = Map.lookup tNode vIdx
                  in case (sIdx, tIdx) of
                    (Just si, Just ti) ->
                      let a' = accum2 a [((si,si), kEff), ((ti,ti), kEff),
                                         ((si,ti), -kEff), ((ti,si), -kEff)]
                          b' = accumV b [(si, -kEff * yi), (ti, kEff * yi)]
                      in (a', b')
                    (Just si, Nothing) ->
                      let a' = accum2 a [((si,si), kEff)]
                          b' = accumV b [(si, -kEff * yi)]
                      in (a', b')
                    _ -> (a, b)
                  ) (aInit, bInit) armInfo
                bMat = Mat.reshape nFree 1 bVec
                sol  = Mat.flatten (Mat.inv aMat `matMul` bMat)
                solList = Mat.toList sol
                eStar = sum $ map (\(sNode, tNode, vi, yi) ->
                  let kEff = 1.0 / (vi + tau2arm)
                      sPos = case Map.lookup sNode vIdx of
                               Just si -> solList !! si; Nothing -> 0
                      tPos = case Map.lookup tNode vIdx of
                               Just ti -> solList !! ti; Nothing -> 0
                  in 0.5 * kEff * (tPos - sPos - yi) ^ (2::Int)
                  ) armInfo
                logDetA      = log (abs (Mat.det aMat))
                armPrefactor = -0.5 * sum
                  (map (\(_, _, vi, _) -> log (vi + tau2arm)) armInfo)
                logZ = armPrefactor - eStar - 0.5 * logDetA
            in logZ

          evalLogZNE t2contrast =
            let lz = evalLogZ t2contrast
                ne = nma (updateSprings springsnet0 (max 1e-10 t2contrast))
            in (lz, ne)

          -- ============== mode-finder via bisection on dlogZ/dτ² ==============
          fSign t =
            let h      = max 1e-4 (t * 1e-3)
                tplus  = t + h
                tminus = max 1e-8 (t - h)
                lp = evalLogZ tplus
                lm = evalLogZ tminus
            in lp - lm  -- proportional to derivative

          bracketLo lo
            | lo < 1e-8        = lo
            | fSign lo > 0     = lo
            | otherwise        = bracketLo (lo * 0.5)
          bracketHi hi
            | hi > 1e8         = hi
            | fSign hi < 0     = hi
            | otherwise        = bracketHi (hi * 2)
          bisect lo hi eps maxIt
            | hi - lo < eps || maxIt <= 0 = (lo + hi) / 2
            | otherwise =
                let x = (lo + hi) / 2
                in if fSign x > 0
                     then bisect x hi eps (maxIt - 1)
                     else bisect lo x eps (maxIt - 1)
          lo0  = bracketLo 1.0
          hi0  = bracketHi 1.0
          reml = bisect lo0 hi0 1e-5 30

          -- ============== adaptive grid + final evaluation ==============
          loG  = max 1e-4 (reml / 1000)
          hiG  = max 5    (reml * 4)
          step = (log hiG - log loG) / fromIntegral (max 1 (nPts - 1))
          gridT = [exp (log loG + step * fromIntegral i) | i <- [0 .. nPts - 1]]
          results = map (\t -> let (lz, ne) = evalLogZNE t in (t, lz, ne)) gridT
          logZs   = map (\(_, lz, _) -> lz) results
          maxLZ   = maximum logZs
          unnorm  = map (\lz -> exp (lz - maxLZ)) logZs
          tot     = sum unnorm
          probs   = map (/ tot) unnorm
          modeIdx = snd $ maximum $ zip probs [0..]
          (modeTau2, _, _) = results !! modeIdx
      in Right (modeTau2, results)
-- At each τ²: IRLS to find mode, linearize at mode, then use the
-- Gaussian arm-space Z formula with Fisher weights as effective variances.
--
-- logZ(τ²) ≈ -E*(τ²) - ½ Σ_arms log(1/w_ij + τ²) - ½ log|A_eff(τ²)|
--
-- where w_ij = n*p*(1-p) is the Fisher weight at the mode, and
-- E* includes both binomial obs energy and tau spring energy.
springGrandCanonicalBin ::
  [Study] ->
  Double ->   -- kTau
  Int ->      -- nMax
  Either String (Double, [(Double, Double)], NetworkEffects)
springGrandCanonicalBin studies kTau nMax =
  let espringnet = makeBinomialSprings studies Nothing (Just 1.0)
  in case espringnet of
    Left err -> Left err
    Right springsnet0 ->
      let vmap = vs2ts springsnet0
          treatVs = [v | (v, TreatmentEnd _) <- IM.toList vmap]
          studyVs' = [v | (v, StudyEnd _)    <- IM.toList vmap]
          pinnedV = head treatVs
          freeVs = filter (/= pinnedV) (studyVs' ++ treatVs)
          nFree  = length freeVs
          vIdx   = Map.fromList $ zip freeVs [0 .. nFree - 1]

          studyVtxMap = Map.fromList [(sid, v) | (v, StudyEnd sid) <- IM.toList vmap]
          treatVtxMap = Map.fromList [(tid, v) | (v, TreatmentEnd tid) <- IM.toList vmap]
          tauVtxMap   = Map.fromList [((sid, tid), v)
                                     | (v, TauEnd (TauId sid tid)) <- IM.toList vmap]

          -- Topology + raw (events, n) for every arm. The obs spring lives on
          -- edge tau↔treat; the τ-spring on edge study↔tau.
          armTopo =
            concatMap (\study ->
              let sid  = getStudyId study
                  sVtx = studyVtxMap Map.! sid
              in map (\arm ->
                    let tid   = tidOfArm arm
                        tVtx  = treatVtxMap Map.! tid
                        tauV  = tauVtxMap   Map.! (sid, tid)
                        (ev, ns) = case arm of
                          BinaryArm _ e n -> (e, n)
                          _ -> error "springGrandCanonicalBin: non-binary arm"
                    in (sVtx, tauV, tVtx, ev, ns)
                 ) (getStudyArms study)
              ) studies

          irlsIters = 50
          irlsEps   = 1e-8

          -- At each τ²_contrast: reduced (Schur-complement) Newton for the
          -- mode, then Laplace Ξ at the mode. The reduced solver is
          -- numerically equivalent to irlsSolve (validated to machine
          -- precision in test/proto_reduced_newton.hs) but each iteration
          -- works on the (k+T-1)-dim reduced system instead of the full
          -- (k+T+A-1)-dim spring network.
          evalLogZ t2contrast =
            let tau2c  = max 1e-10 t2contrast
                tau2arm = tau2c / 2.0
                net     = updateSprings springsnet0 tau2c
                neteffs = newtonSolveReducedBin net irlsIters irlsEps
                spnet   = springNetwork neteffs
                sprs    = springs spnet

                lookupSpr u v =
                  Map.lookup (G.Edge u v) sprs
                    <|> Map.lookup (G.Edge v u) sprs

                -- For each arm, extract (Fisher weight, converged obs-spring
                -- length l, τ-spring length u) at the IRLS mode. The linearised
                -- spring's hardness still equals n·σ(l)(1-σ(l)).
                armFisher = map (\(sVtx, tauV, tVtx, ev, ns) ->
                    let obs = case lookupSpr tauV tVtx of
                          Just s  -> s
                          Nothing -> error $ "springGrandCanonicalBin: missing \
                                              \obs spring tau↔treat for "
                                              ++ show (tauV, tVtx)
                        tauSp = case lookupSpr sVtx tauV of
                          Just s  -> s
                          Nothing -> error $ "springGrandCanonicalBin: missing \
                                              \tau spring study↔tau for "
                                              ++ show (sVtx, tauV)
                        wij  = max 1e-10 (hardness obs)
                        vij  = 1.0 / wij
                        kEff = 1.0 / (vij + tau2arm)
                        lObs = springLength obs            -- l = x_T − x_τ
                        uTau = springLength tauSp          -- u = x_τ − x_S
                    in (sVtx, tVtx, vij, kEff, ev, ns, lObs, uTau)
                  ) armTopo

                -- Exact post-linearisation Laplace energy at the mode:
                --   E* = Σ_arms [n·log(1+e^l) − e·l]   (binomial obs likelihood)
                --      + Σ_arms (1/(2τ²_arm)) · u²    (τ-spring energy)
                -- The Gaussian τ-springs are unchanged by linearizeNetwork, so
                -- their stored springEnergy is correct.  But the obs-springs are
                -- now Spring(k, ll, l) and their linearised energy
                -- (1/2)·k·(l−ll)² ≠ exact binomial loss at the mode.
                eStar =
                  sum [ let n_  = fromIntegral ns
                            e_  = fromIntegral ev
                            -- log(1+e^l) computed in the stable branch.
                            lp1 = if lObs >= 0
                                    then lObs + log (1 + exp (-lObs))
                                    else log (1 + exp lObs)
                        in n_ * lp1 - e_ * lObs
                      | (_,_,_,_,ev,ns,lObs,_) <- armFisher ]
                  + sum [ uTau*uTau / (2.0 * tau2arm)
                        | (_,_,_,_,_,_,_,uTau) <- armFisher ]

                -- Effective stiffness on (study+treat) free nodes after
                -- eliminating tau nodes: each arm is a series spring with
                -- k_eff = 1/(1/w_ij + τ²_arm).
                -- Collect all contributions into an (i,j)->Double map, then
                -- materialise the matrix ONCE. Avoids O(n²) rebuilds per arm.
                armEntries = concatMap (\(sNode, tNode, _, kEff, _, _, _, _) ->
                    let sIdx = Map.lookup sNode vIdx
                        tIdx = Map.lookup tNode vIdx
                    in case (sIdx, tIdx) of
                      (Just si, Just ti) -> [((si,si),kEff),((ti,ti),kEff),
                                             ((si,ti),-kEff),((ti,si),-kEff)]
                      (Just si, Nothing) -> [((si,si),kEff)]
                      _ -> []
                  ) armFisher
                entryMap = Map.fromListWith (+) armEntries
                aMat = Mat.fromLists
                  [ [ Map.findWithDefault 0 (i, j) entryMap
                    | j <- [0 .. nFree - 1] ]
                  | i <- [0 .. nFree - 1] ]

                logDetA      = log (abs (Mat.det aMat))
                armPrefactor = -0.5 * sum
                  (map (\(_,_,vij,_,_,_,_,_) -> log (1 + tau2arm / vij)) armFisher)

                logZ = armPrefactor - eStar - 0.5 * logDetA
            in (logZ, neteffs)

          tau2Vals = map (\n -> fromIntegral n / kTau) [0..nMax]
          results = map (\t2 -> let (lz, ne) = evalLogZ t2 in (t2, lz, ne)) tau2Vals
          logZs = map (\(_, lz, _) -> lz) results
          maxLZ = maximum logZs
          unnorm = map (\lz -> exp (lz - maxLZ)) logZs
          totalMass = sum unnorm
          probs = map (/ totalMass) unnorm
          modeIdx = snd $ maximum $ zip probs [0..]
          (modeTau2, _, modeNE) = results !! modeIdx
          grid = zip tau2Vals logZs
      in Right (modeTau2, grid, modeNE)

-- | Same as 'springGrandCanonicalBin' but returns the per-τ² grid with
-- NetworkEffects at each grid point. This lets callers reconstruct the FULL
-- marginal posterior of any treatment effect as the mixture
--   p(d_j | y) = Σ_k p(τ²_k | y) · N(d_j; μ_j(τ²_k), σ²_j(τ²_k))
-- where p(τ²_k | y) ∝ exp(logZ_k) · π(τ²_k) for any chosen prior π.
springGrandCanonicalBinFull ::
  [Study] ->
  Double ->   -- kTau
  Int ->      -- nMax
  Either String (Double, [(Double, Double, NetworkEffects)])
springGrandCanonicalBinFull studies kTau nMax =
  let tau2Vals = map (\n -> fromIntegral n / kTau) [0..nMax]
  in evalXiBinAt studies tau2Vals

-- | Adaptive-grid GC: find the posterior mode via REML (cheap EM-style solver),
-- then evaluate Ξ on ~nPts log-spaced τ² values concentrated around the mode
-- covering [mode/30, mode*30] (clamped to a sensible absolute range when the
-- mode is near zero).  Delivers the same mode / median / CI as the uniform
-- grid at a fraction of the cost.
springGrandCanonicalBinAdaptive ::
  [Study] ->
  Int ->       -- ^ number of grid points (~100 is usually plenty)
  Either String (Double, [(Double, Double, NetworkEffects)])
springGrandCanonicalBinAdaptive studies nPts =
  case springREMLBin studies Nothing (Just 500) (Just 1e-6) of
    Left err             -> Left err
    Right (_, (tauHat, _)) ->
      let -- Cover τ² ∈ [lo, hi] on a log-spaced grid. If the REML mode is
          -- tiny (near the boundary), widen the low end to an absolute floor
          -- and the high end to cover typical prior support.
          --
          -- The upper bound is held tighter (×10 instead of ×30) because the
          -- reduced-Schur Hessian becomes near-singular at very large τ²
          -- (eigenvalues like 1/τ² → 0), causing pure-Haskell LU to produce
          -- garbage solves and Newton to spin on rare-events replicates.
          -- HN(τ; σ ≤ 1) places negligible posterior mass past 10·REML, so
          -- this trims the grid where the Laplace posterior weight is ~0
          -- without affecting reportable τ² statistics.
          tHat   = max tauHat 1e-5
          lo     = max 1e-6 (tHat / 30)
          hi     = max (tHat * 10) 2.0
          ls     = [lo * ((hi/lo) ** (fromIntegral i / fromIntegral (nPts-1)))
                   | i <- [0 .. nPts - 1]]
          tau2Vs = 0.0 : ls       -- include τ² = 0 for the fixed-effect point
      in  evalXiBinAt studies tau2Vs

-- | Evaluate log Ξ(τ²) and the network effects at an ARBITRARY list of τ²
-- values. Grid points are independent → can be parallelised.
evalXiBinAt ::
  [Study] ->
  [Double] ->    -- ^ τ²_contrast values to evaluate at (any order/spacing)
  Either String (Double, [(Double, Double, NetworkEffects)])
evalXiBinAt studies tau2Vals =
  let espringnet = makeBinomialSprings studies Nothing (Just 1.0)
  in case espringnet of
    Left err -> Left err
    Right springsnet0 ->
      let vmap = vs2ts springsnet0
          treatVs = [v | (v, TreatmentEnd _) <- IM.toList vmap]
          studyVs' = [v | (v, StudyEnd _)    <- IM.toList vmap]
          pinnedV = head treatVs
          freeVs = filter (/= pinnedV) (studyVs' ++ treatVs)
          nFree  = length freeVs
          vIdx   = Map.fromList $ zip freeVs [0 .. nFree - 1]

          studyVtxMap = Map.fromList [(sid, v) | (v, StudyEnd sid) <- IM.toList vmap]
          treatVtxMap = Map.fromList [(tid, v) | (v, TreatmentEnd tid) <- IM.toList vmap]
          tauVtxMap   = Map.fromList [((sid, tid), v)
                                     | (v, TauEnd (TauId sid tid)) <- IM.toList vmap]

          armTopo =
            concatMap (\study ->
              let sid  = getStudyId study
                  sVtx = studyVtxMap Map.! sid
              in map (\arm ->
                    let tid   = tidOfArm arm
                        tVtx  = treatVtxMap Map.! tid
                        tauV  = tauVtxMap   Map.! (sid, tid)
                        (ev, ns) = case arm of
                          BinaryArm _ e n -> (e, n)
                          _ -> error "evalXiBinAt: non-binary arm"
                    in (sVtx, tauV, tVtx, ev, ns)
                 ) (getStudyArms study)
              ) studies

          irlsIters = 50
          irlsEps   = 1e-8

          -- Warm-started variant: use the passed-in network (from the
          -- previous grid point) as the IRLS starting point. updateSprings
          -- changes only the tau-spring hardness; node positions are kept,
          -- so IRLS converges in 1-3 iterations when adjacent τ² values
          -- are close.  Falls back to the fresh springsnet0 when no warm
          -- start is available.
          evalLogZFrom warmNet t2contrast =
            let tau2c  = max 1e-10 t2contrast
                tau2arm = tau2c / 2.0
                net     = updateSprings warmNet tau2c
                neteffs = newtonSolveReducedBin net irlsIters irlsEps
                spnet   = springNetwork neteffs
                sprs    = springs spnet

                lookupSpr u v =
                  Map.lookup (G.Edge u v) sprs
                    <|> Map.lookup (G.Edge v u) sprs

                armFisher = map (\(sVtx, tauV, tVtx, ev, ns) ->
                    let obs = case lookupSpr tauV tVtx of
                          Just s  -> s
                          Nothing -> error "evalXiBinAt: obs spring missing"
                        tauSp = case lookupSpr sVtx tauV of
                          Just s  -> s
                          Nothing -> error "evalXiBinAt: tau spring missing"
                        wij  = max 1e-10 (hardness obs)
                        vij  = 1.0 / wij
                        kEff = 1.0 / (vij + tau2arm)
                        lObs = springLength obs
                        uTau = springLength tauSp
                    in (sVtx, tVtx, vij, kEff, ev, ns, lObs, uTau)
                  ) armTopo

                -- Exact Laplace energy at the mode (binomial obs + Gaussian τ).
                eStar =
                  sum [ let n_  = fromIntegral ns
                            e_  = fromIntegral ev
                            lp1 = if lObs >= 0
                                    then lObs + log (1 + exp (-lObs))
                                    else log (1 + exp lObs)
                        in n_ * lp1 - e_ * lObs
                      | (_,_,_,_,ev,ns,lObs,_) <- armFisher ]
                  + sum [ uTau*uTau / (2.0 * tau2arm)
                        | (_,_,_,_,_,_,_,uTau) <- armFisher ]

                armEntries = concatMap (\(sNode, tNode, _, kEff, _, _, _, _) ->
                    let sIdx = Map.lookup sNode vIdx
                        tIdx = Map.lookup tNode vIdx
                    in case (sIdx, tIdx) of
                      (Just si, Just ti) -> [((si,si),kEff),((ti,ti),kEff),
                                             ((si,ti),-kEff),((ti,si),-kEff)]
                      (Just si, Nothing) -> [((si,si),kEff)]
                      _ -> []
                  ) armFisher
                entryMap = Map.fromListWith (+) armEntries
                -- Build an hmatrix matrix (LAPACK-backed) and use its fast
                -- determinant via LU factorisation. Much cheaper than the
                -- pure-Haskell Mat.det for medium-to-large networks.
                flatRowMajor =
                  [ Map.findWithDefault 0 (i,j) entryMap
                  | i <- [0 .. nFree - 1], j <- [0 .. nFree - 1] ]
                aMatLA       = (nFree LA.>< nFree) flatRowMajor
                logDetA      = log (abs (LA.det aMatLA))
                armPrefactor = -0.5 * sum
                  (map (\(_,_,vij,_,_,_,_,_) -> log (1 + tau2arm / vij)) armFisher)
                logZ = armPrefactor - eStar - 0.5 * logDetA
            in (logZ, neteffs)

          -- Sort τ² before evaluation (CDF downstream assumes monotone).
          t2Sorted = sort tau2Vals
          -- No warm-start: each grid point starts from the fresh springsnet0.
          -- Warm-start across nearby τ² was giving wrong posteriors on some
          -- heterogeneity regimes (the joint-mode drift is larger than Newton
          -- can follow back to the true mode in 1-3 iterations when the seed
          -- is far from the target).  Re-enable only once the accuracy is
          -- verified dataset-by-dataset.
          evalPoint t2 =
            let (!lz, !ne) = evalLogZFrom springsnet0 t2
            in (t2, lz, ne)
          results = map evalPoint t2Sorted
          logZs   = map (\(_, lz, _) -> lz) results
          maxLZ   = maximum logZs
          unnorm  = map (\lz -> exp (lz - maxLZ)) logZs
          totalM  = sum unnorm
          probs   = map (/ totalM) unnorm
          modeIdx = snd $ maximum $ zip probs [0..]
          (modeTau2, _, _) = results !! modeIdx
      in Right (modeTau2, results)
matMul :: Mat.Matrix -> Mat.Matrix -> Mat.Matrix
matMul = (Mat.<>)

-- Accumulate entries into matrix
accum2 :: Mat.Matrix -> [((Int,Int), Double)] -> Mat.Matrix
accum2 m entries =
  let rows = Mat.toLists m
      addEntry rs ((i,j), v) =
        [ if ri == i
          then [ if cj == j then old + v else old | (cj, old) <- zip [0..] row ]
          else row
        | (ri, row) <- zip [0..] rs ]
  in Mat.fromLists $ foldl' addEntry rows entries

-- Accumulate entries into vector
accumV :: [Double] -> [(Int, Double)] -> [Double]
accumV v entries = foldl' (\vs (i, val) ->
  [ if j == i then old + val else old | (j, old) <- zip [0..] vs ]
  ) v entries
