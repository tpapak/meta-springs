{-# LANGUAGE BangPatterns #-}
-- Step 3: 1-D Newton on the analytic d log Z / dτ² to find the GC mode.
--   τ²_{n+1} = τ²_n − f(τ²_n) / f'(τ²_n)
-- f' computed by central finite difference of the analytic f.
-- Convergence target: GC argmax (= 9.606 for continuous.json) to ~1e-6
-- in ~5 iterations from a coarse start.
module Main where

import qualified Data.Map.Strict     as Map
import qualified Data.IntMap.Strict  as IM
import           Data.Foldable       (foldl')
import           Data.Maybe          (fromMaybe)
import           Text.Printf         (printf)

import qualified Data.Meta.Matrix         as Mat
import           Data.Meta.Effects        (meanArm, Gaussian (..), tidOfArm)
import           Data.Meta.Studies        (readStudies, getStudyId, getStudyArms, Study)
import           Data.Meta.NMA            (SpringVertex (..), makeSprings)
import qualified Data.Meta.NMA            as NMA
import           Data.Meta.RandomEffects  (springGrandCanonical)

-- analytic d/dτ² log Z at a single τ²_contrast.
analyticDeriv :: Double -> [Study] -> Maybe Double
analyticDeriv t2contrast studies =
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
                            then [ if c == j then x + v else x
                                 | (c, x) <- zip [0..] row ]
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
          armTerm (sV, tV, vi, yi) =
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
            in kEff * kEff * (r * r + hLev) - kEff
      in Just $ 0.25 * sum (map armTerm armInfo)
  where
    mapMaybe f = foldr (\x acc -> case f x of Just y -> y : acc; Nothing -> acc) []

-- Hybrid bracket+Newton mode-finder. First locate τ²_lo, τ²_hi with
-- f(τ²_lo) > 0 > f(τ²_hi) (an "interior maximum bracket"), then refine
-- with Newton when the step stays inside, bisection otherwise.
newtonMode ::
  [Study] ->
  Double ->            -- starting guess
  Int ->               -- max iterations
  Double ->            -- convergence eps on |τ²_{n+1} − τ²_n|
  IO (Double, Int)
newtonMode studies _ maxIt eps = do
  let f t = fromMaybe (0/0) (analyticDeriv t studies)
  -- Bracket: walk doubling-down for τ²_lo (need f>0, i.e. below mode)
  --           walk doubling-up for τ²_hi (need f<0, i.e. above mode).
  let bracketLo lo
        | lo < 1e-8                    = lo
        | f lo > 0                     = lo
        | otherwise                    = bracketLo (lo * 0.5)
      bracketHi hi
        | hi > 1e8                     = hi
        | f hi < 0                     = hi
        | otherwise                    = bracketHi (hi * 2)
      lo0 = bracketLo 1.0
      hi0 = bracketHi 1.0
  refine 0 lo0 hi0 (f lo0) (f hi0)
  where
    refine !it !lo !hi !flo !fhi
      | it >= maxIt           = return ((lo + hi) / 2, it)
      | (hi - lo) < eps       = return ((lo + hi) / 2, it)
      | otherwise = do
          -- Pure bisection: midpoint, halve bracket each iter.
          let f t = fromMaybe (0/0) (analyticDeriv t studies)
              x   = (lo + hi) / 2
              fx  = f x
          if fx > 0
             then refine (it + 1) x hi fx fhi
             else refine (it + 1) lo x flo fx

main :: IO ()
main = do
  Right studies <- readStudies "test/continuous.json"
  printf "Dataset: continuous.json (%d studies, Gaussian)\n\n" (length studies)

  -- Reference: GC argmax via dense grid sweep.
  case springGrandCanonical studies meanArm 1000 30000 of
    Left e -> putStrLn $ "GC err: " ++ e
    Right (gcMode, _, _) -> do
      printf "GC argmax τ²  (dense grid)  = %.6f\n\n" gcMode

      printf "%-10s %-12s %-7s %-12s\n"
        ("start"::String) ("Newton τ²"::String) ("iters"::String) ("|Δ vs grid|"::String)
      printf "%s\n" (replicate 50 '-')
      mapM_ (\x0 -> do
        (x, n) <- newtonMode studies x0 30 1e-5
        printf "%-10.3f %-12.6f %-7d %.3e\n" x0 x n (abs (x - gcMode))
        ) [0.1, 1.0, 5.0, 20.0, 100.0]
