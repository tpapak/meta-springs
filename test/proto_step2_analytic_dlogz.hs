{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Step 2: implement and verify an analytic d log Z / dτ² for the Gaussian
-- path, then compare against the finite-difference values from Step 1.
--
-- The function reproduces what springGrandCanonical does at a single τ²
-- (build A_eff, solve for the mode, compute residuals, leverages) and
-- returns the analytic derivative
--   d/dτ² log Z = (1/4) Σ_ij [ k_eff_ij² · (r_ij² + h_ij)  −  k_eff_ij ]
--
-- where:
--   k_eff_ij = 1 / (v_ij + τ²/2)
--   r_ij     = (x_T_j* − x_S_i*) − y_ij    (residual at mode)
--   h_ij     = (e_s − e_t)^T A_eff^{-1} (e_s − e_t)   (leverage)
--
-- Verification: this should equal the finite difference (lookupLZ(τ²+h) -
-- lookupLZ(τ²-h))/(2h) from a fine GC grid to ~1e-3 (limited by grid
-- resolution and IRLS tolerance).
module Main where

import qualified Data.Map.Strict     as Map
import qualified Data.IntMap.Strict  as IM
import           Data.Foldable       (foldl')
import           Data.List           (sortBy)
import           Data.Ord            (comparing)
import           Data.Maybe          (fromMaybe)
import           Text.Printf         (printf)

import qualified Data.Meta.Matrix         as Mat
import           Data.Meta.Effects        (TreatmentId (..), meanArm,
                                           ArmEffect (..), Gaussian (..),
                                           tidOfArm)
import           Data.Meta.Studies        (readStudies, getStudyId, getStudyArms, Study)
import           Data.Meta.NMA            (SpringVertex (..),
                                           makeSprings)
import qualified Data.Meta.NMA            as NMA
import           Data.Meta.RandomEffects  (springGrandCanonical)

-- Analytic d log Z / dτ²_contrast at a given τ²_contrast.
analyticDLogZ ::
  [Study] ->
  (a)
analyticDLogZ = error "stub"

-- A re-implementation of the inner body of springGrandCanonical at a
-- single τ², returning enough state to compute the analytic derivative.
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

          -- arm info: (sVtx, tVtx, v_ij, y_ij)
          armInfo =
            concatMap (\study ->
              let sid  = getStudyId study
                  sVtx = studyVtxMap Map.! sid
              in mapMaybe (\arm ->
                    case meanArm arm of
                      Right eff ->
                        let tid  = tidOfArm arm
                            tVtx = treatVtxMap Map.! tid
                            vi   = variance eff
                            yi   = expectation eff
                        in Just (sVtx, tVtx, vi, yi)
                      Left _ -> Nothing
                  ) (getStudyArms study)
              ) studies

          -- Build A_eff (effective stiffness) and b (RHS) on free nodes.
          accum2 :: [[Double]] -> [((Int, Int), Double)] -> [[Double]]
          accum2 m0 ents =
            let m = foldl' (\mm ((i, j), v) ->
                       [ if r == i
                            then [ if c == j then x + v else x
                                 | (c, x) <- zip [0..] row ]
                            else row
                       | (r, row) <- zip [0..] mm ]) m0 ents
            in m
          accumV :: [Double] -> [(Int, Double)] -> [Double]
          accumV v0 ents = foldl' (\v (i, x) ->
                              [ if k == i then y + x else y | (k, y) <- zip [0..] v ]) v0 ents
          aInit = replicate nFree (replicate nFree 0)
          bInit = replicate nFree 0

          (aMat0, bVec0) =
            foldl' (\(am, bm) (sV, tV, vi, yi) ->
                let kEff = 1.0 / (vi + tau2arm)
                    si   = Map.lookup sV vIdx
                    ti   = Map.lookup tV vIdx
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
                      Just i  -> sol !! i
                      Nothing -> 0

          -- Per-arm: r, h, k_eff
          armTerm (sV, tV, vi, yi) =
            let kEff = 1.0 / (vi + tau2arm)
                xS   = posOf sV
                xT   = posOf tV
                r    = xT - xS - yi
                hLev =
                  case (Map.lookup sV vIdx, Map.lookup tV vIdx) of
                    (Just s, Just t) ->
                      Mat.atIndex aInv (s, s)
                      + Mat.atIndex aInv (t, t)
                      - 2 * Mat.atIndex aInv (s, t)
                    (Just s, Nothing) -> Mat.atIndex aInv (s, s)
                    (Nothing, Just t) -> Mat.atIndex aInv (t, t)
                    _ -> 0
            in kEff * kEff * (r * r + hLev) - kEff

          deriv = 0.25 * sum (map armTerm armInfo)
      in Just deriv
  where
    mapMaybe f = foldr (\x acc -> case f x of Just y -> y : acc; Nothing -> acc) []

main :: IO ()
main = do
  Right studies <- readStudies "test/continuous.json"
  printf "Dataset: continuous.json (%d studies, Gaussian)\n\n" (length studies)

  -- Pre-compute fine GC grid for finite-difference reference.
  case springGrandCanonical studies meanArm 1000 30000 of
    Left e -> putStrLn $ "GC err: " ++ e
    Right (gcMode, grid, _) -> do
      printf "GC argmax τ² = %.6f\n\n" gcMode
      let lookupLZ t2 =
            let (_, lz) = head $ sortBy (comparing (\(t, _) -> abs (t - t2))) grid
            in lz
          h = 0.05
          fd t2 = (lookupLZ (t2 + h) - lookupLZ (t2 - h)) / (2 * h)

      printf "%-10s %-14s %-14s %-14s\n"
        ("τ²"::String) ("analytic"::String) ("finite-diff"::String) ("|Δ|"::String)
      printf "%s\n" (replicate 56 '-')
      mapM_ (\t2 -> do
        let an  = fromMaybe (0/0) (analyticDeriv t2 studies)
            fdv = fd t2
            d   = abs (an - fdv)
        printf "%-10.4f %+12.4f %+12.4f %12.3e\n" t2 an fdv d
        ) [0.5, 2.0, 5.0, gcMode, 15.0, 25.0]
