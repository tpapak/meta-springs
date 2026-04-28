{-# LANGUAGE BangPatterns #-}
-- Step 4: adaptive τ² grid for Gaussian Ξ.
-- 1. Find mode τ²* via bisection on d log Z / dτ².
-- 2. Estimate Hessian curvature c = −d²/dτ²² log Z |_{τ²*}.
-- 3. Place ~20 grid points spanning [max(0, τ²* − 4σ), τ²* + 4σ] with σ = 1/√c.
-- 4. Evaluate log Z + HalfN(0,1) prior weight at each → posterior summaries.
-- 5. Compare summaries against the existing dense-grid springGrandCanonical.
module Main where

import qualified Data.Map.Strict     as Map
import qualified Data.IntMap.Strict  as IM
import           Data.Foldable       (foldl')
import           Data.List           (sortBy)
import           Data.Maybe          (fromMaybe)
import           Data.Ord            (comparing)
import           Text.Printf         (printf)

import qualified Data.Meta.Matrix         as Mat
import           Data.Meta.Effects        (meanArm, Gaussian (..), tidOfArm)
import           Data.Meta.Studies        (readStudies, getStudyId, getStudyArms, Study)
import           Data.Meta.NMA            (SpringVertex (..), makeSprings)
import qualified Data.Meta.NMA            as NMA
import           Data.Meta.RandomEffects  (springGrandCanonical)

-- Returns (logZ, dlogZ/dτ²) at τ²_contrast for the Gaussian Ξ.
evalAt :: [Study] -> Double -> Maybe (Double, Double)
evalAt studies t2contrast =
  case makeSprings studies Nothing (Just 1.0) meanArm of
    Left _ -> Nothing
    Right springsnet0 ->
      let vmap     = NMA.vs2ts springsnet0
          tau2c    = max 1e-10 t2contrast
          tau2arm  = tau2c / 2.0
          treatVs  = [v | (v, TreatmentEnd _) <- IM.toList vmap]
          studyVs  = [v | (v, StudyEnd _)     <- IM.toList vmap]
          pinnedV  = head treatVs
          freeVs   = filter (/= pinnedV) (studyVs ++ treatVs)
          nFree    = length freeVs
          vIdx     = Map.fromList $ zip freeVs [0 .. nFree - 1]
          studyVtxMap = Map.fromList [(sid, v) | (v, StudyEnd sid)     <- IM.toList vmap]
          treatVtxMap = Map.fromList [(tid, v) | (v, TreatmentEnd tid) <- IM.toList vmap]
          armInfo =
            concatMap (\study ->
              let sid  = getStudyId study
                  sVtx = studyVtxMap Map.! sid
              in mapMaybe (\arm ->
                    case meanArm arm of
                      Right eff ->
                        let tid  = tidOfArm arm
                            tVtx = treatVtxMap Map.! tid
                        in Just (sVtx, tVtx, variance eff, expectation eff)
                      Left _ -> Nothing
                  ) (getStudyArms study)
              ) studies
          accum2 m0 ents =
            foldl' (\mm ((i, j), v) ->
                       [ if r == i
                            then [ if c == j then x + v else x | (c, x) <- zip [0..] row ]
                            else row
                       | (r, row) <- zip [0..] mm ]) m0 ents
          accumV v0 ents =
            foldl' (\v (i, x) ->
                       [ if k == i then y + x else y | (k, y) <- zip [0..] v ]) v0 ents
          aInit = replicate nFree (replicate nFree 0)
          bInit = replicate nFree 0
          (aMat0, bVec0) =
            foldl' (\(am, bm) (sV, tV, vi, yi) ->
                let kEff = 1.0 / (vi + tau2arm)
                    si = Map.lookup sV vIdx
                    ti = Map.lookup tV vIdx
                in case (si, ti) of
                     (Just s, Just t) ->
                       (accum2 am [((s,s), kEff), ((t,t), kEff),
                                   ((s,t), -kEff), ((t,s), -kEff)],
                        accumV bm [(s, -kEff * yi), (t, kEff * yi)])
                     (Just s, Nothing) ->
                       (accum2 am [((s,s), kEff)],
                        accumV bm [(s, -kEff * yi)])
                     _ -> (am, bm)
              ) (aInit, bInit) armInfo
          aMat = Mat.fromLists aMat0
          bMat = Mat.reshape nFree 1 bVec0
          aInv = Mat.inv aMat
          sol  = Mat.toList (Mat.flatten (aInv Mat.<> bMat))
          posOf v = case Map.lookup v vIdx of
                      Just i -> sol !! i
                      Nothing -> 0
          armPart (sV, tV, vi, yi) =
            let kEff = 1.0 / (vi + tau2arm)
                xS = posOf sV; xT = posOf tV
                r = xT - xS - yi
                hLev = case (Map.lookup sV vIdx, Map.lookup tV vIdx) of
                  (Just s, Just t) ->
                    Mat.atIndex aInv (s, s) + Mat.atIndex aInv (t, t)
                    - 2 * Mat.atIndex aInv (s, t)
                  (Just s, Nothing) -> Mat.atIndex aInv (s, s)
                  (Nothing, Just t) -> Mat.atIndex aInv (t, t)
                  _ -> 0
                eContrib = 0.5 * kEff * r * r
                logV     = log (vi + tau2arm)
                derivContrib = kEff * kEff * (r * r + hLev) - kEff
            in (eContrib, logV, derivContrib)
          contribs = map armPart armInfo
          eStar    = sum [e | (e, _, _) <- contribs]
          armPref  = -0.5 * sum [l | (_, l, _) <- contribs]
          logDetA  = log (abs (Mat.det aMat))
          logZ     = armPref - eStar - 0.5 * logDetA
          dlz      = 0.25 * sum [d | (_, _, d) <- contribs]
      in Just (logZ, dlz)
  where
    mapMaybe f = foldr (\x acc -> case f x of Just y -> y : acc; Nothing -> acc) []

-- Bisection mode-finder on dlogZ/dτ² (no-prior ⇒ REML mode of the marginal
-- likelihood). Used to anchor the adaptive grid; we then reweight by the
-- prior at evaluation time. (The posterior mode itself can be at the
-- boundary τ²→0 under strong priors and isn't a useful anchor in those
-- cases.)
findReml :: [Study] -> Double -> IO Double
findReml studies eps = do
  let f t = case evalAt studies t of
              Just (_, d) -> d
              Nothing     -> 0/0
      bracketLo lo
        | lo < 1e-8     = lo
        | f lo > 0      = lo
        | otherwise     = bracketLo (lo * 0.5)
      bracketHi hi
        | hi > 1e8      = hi
        | f hi < 0      = hi
        | otherwise     = bracketHi (hi * 2)
      lo0 = bracketLo 1.0
      hi0 = bracketHi 1.0
      bisect lo hi
        | hi - lo < eps = (lo + hi) / 2
        | otherwise =
            let x = (lo + hi) / 2
                fx = f x
            in if fx > 0 then bisect x hi else bisect lo x
  return (bisect lo0 hi0)

-- HalfNormal(0,1) on τ → log p(τ²) = -0.5 log τ² - 0.5 τ² + const (drop const)
hnLogPrior :: Double -> Double
hnLogPrior t2 | t2 <= 0   = -1e30
              | otherwise = -0.5 * log t2 - 0.5 * t2

-- Adaptive grid: log-spaced from a tiny lower bound to ~4× the REML mode.
-- We anchor on REML to span the data's peak region; the prior reshapes the
-- posterior at evaluation time. Lower edge is small enough to capture
-- prior-dominant boundary behaviour for HalfN-style priors.
adaptiveGrid :: [Study] -> Double -> Int -> [Double]
adaptiveGrid _ tauMode n =
  let lo   = max 1e-4 (tauMode / 1000)
      hi   = max 5    (tauMode * 4)
      step = (log hi - log lo) / fromIntegral (n - 1)
  in [exp (log lo + step * fromIntegral i) | i <- [0 .. n - 1]]

main :: IO ()
main = do
  Right studies <- readStudies "test/continuous.json"
  printf "Dataset: continuous.json (%d studies, Gaussian)\n\n" (length studies)

  -- Reference: dense GC sweep summaries.
  let refKTau = 100; refNMax = 3000  -- 3001 grid points up to τ²=30
  Right (gcMode, refGrid, _) <- pure $
    case springGrandCanonical studies meanArm refKTau refNMax of
      r@(Right _) -> r; Left e -> error e
  let refWeights =
        let lps = [lz + hnLogPrior t2 | (t2, lz) <- refGrid, t2 > 0]
            m   = maximum lps
            ws  = map (\lp -> exp (lp - m)) lps
            tot = sum ws
        in zipWith (\(t2, _) _ -> ()) refGrid (replicate (length refGrid) ())
        `seq` (map (/ tot) ws)
      refTaus = [t2 | (t2, _) <- refGrid, t2 > 0]
      refLogZs = [lz | (t2, lz) <- refGrid, t2 > 0]
      wQuant ts ws q =
        let s = zip ts ws
            cum = scanl1 (+) (map snd s)
            go [] = fst (last s)
            go ((c, t):rest) | c >= q = t | otherwise = go rest
        in go (zip cum ts)
  let refMed   = wQuant refTaus refWeights 0.5
      refLo    = wQuant refTaus refWeights 0.025
      refHi    = wQuant refTaus refWeights 0.975
      refMean  = sum (zipWith (*) refTaus refWeights)

  -- REML mode (no prior). Used to anchor the adaptive grid.
  modeNewton <- findReml studies 1e-5

  -- Adaptive grid evaluation. For log-spaced points, the trapezoidal
  -- rule in τ² space requires the Jacobian τ² (since dτ² = τ²·d log τ²).
  -- Without it, the high-τ² tail is under-weighted.
  let grid = adaptiveGrid studies modeNewton 50
      logZs = [case evalAt studies t of Just (lz, _) -> lz; Nothing -> -1e30 | t <- grid]
      lps   = zipWith (\t lz -> lz + hnLogPrior t + log t) grid logZs
      m     = maximum lps
      ws    = map (\lp -> exp (lp - m)) lps
      tot   = sum ws
      ws'   = map (/ tot) ws
      adMed = wQuant grid ws' 0.5
      adLo  = wQuant grid ws' 0.025
      adHi  = wQuant grid ws' 0.975
      adMean = sum (zipWith (*) grid ws')

  printf "%-20s %-12s %-12s\n" ("summary"::String) ("dense"::String) ("adaptive (20)"::String)
  printf "%s\n" (replicate 50 '-')
  printf "%-20s %-12.6f %-12.6f\n" ("argmax τ² (no prior)"::String) gcMode modeNewton
  printf "%-20s %-12.6f %-12.6f\n" ("post median τ²"::String) refMed adMed
  printf "%-20s %-12.6f %-12.6f\n" ("post mean τ²"::String) refMean adMean
  printf "%-20s %-12.6f %-12.6f\n" ("post 2.5%% τ²"::String) refLo adLo
  printf "%-20s %-12.6f %-12.6f\n" ("post 97.5%% τ²"::String) refHi adHi
  printf "\nadaptive grid range: [%.4f, %.4f] (%d pts)\n" (head grid) (last grid) (length grid)
  printf "Newton mode-finder iterations not separately reported.\n"
