{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Step 0: baseline correctness tests for the existing GC code.
-- We do NOT change anything; we only check that current code is internally
-- consistent before extending it.
--
-- Tests:
--   T1 (Gaussian)  : springGrandCanonical's argmax-τ² grid point matches
--                    the τ² mode that springREML reports (within grid resolution).
--   T2 (Gaussian)  : at that τ², the per-grid NetworkEffects matches REML's
--                    NetworkEffects on every contrast (≤ 1e-6).
--   T3 (Binomial)  : springGrandCanonicalBin argmax matches springREMLBin
--                    (within grid resolution).
--   T4 (Binomial)  : at the GC mode, NetworkEffects effects match what an
--                    independent newtonSolve at that τ² produces (≤ 1e-6).
--
-- Run: stack runghc --package meta-analysis -- test/proto_verify_baseline.hs
module Main where

import qualified Data.Map.Strict as Map
import           Data.List       (sortBy)
import           Data.Ord        (comparing)
import           Text.Printf     (printf)

import Data.Meta.Effects       (TreatmentId (..), meanArm)
import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects (..), makeBinomialSprings,
                                makeSprings, updateSprings)
import Data.Meta.RandomEffects (springGrandCanonical, springGrandCanonicalBin,
                                springREML, springREMLBin,
                                irlsSolve)

-- A small Gaussian arm-level dataset is hard to come by inline; use the
-- 5-treatment continuous test fixture if available, else skip with a note.
gaussianFixture :: FilePath
gaussianFixture = "test/synth_bin/synth_041.json"   -- placeholder; binomial fixture

binomialFixture :: FilePath
binomialFixture = "test/synth_bin/synth_041.json"

-- ----------------------------------------------------------------------
-- T1+T2: Gaussian GC vs REML
-- ----------------------------------------------------------------------
-- Skipped here because synth_041 is binary; documented as TODO.
-- We run the binomial tests below.

-- ----------------------------------------------------------------------
-- T3+T4: Binomial GC vs REML
-- ----------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Step 0 baseline correctness (existing code, no changes) ==="
  putStrLn ""

  -- Gaussian first (Laplace is exact for Gaussian → REML mode and GC argmax
  -- MUST coincide).
  putStrLn "----- Gaussian: continuous.json -----"
  Right gStudies <- readStudies "test/continuous.json"
  printf "  %d studies\n" (length gStudies)
  case springREML gStudies meanArm Nothing (Just 500) (Just 1e-6) of
    Left e -> putStrLn $ "  springREML err: " ++ e
    Right (_, (gReml, _)) -> printf "  springREML τ² = %.6f\n" gReml
  case springGrandCanonical gStudies meanArm 100 3000 of
    Left e -> putStrLn $ "  springGrandCanonical err: " ++ e
    Right (gMode, _, _) -> printf "  springGrandCanonical argmax τ² = %.6f\n" gMode
  putStrLn ""

  -- Now binomial.
  putStrLn "----- Binomial: synth_041.json -----"
  Right studies <- readStudies binomialFixture
  printf "Dataset: %s  (%d studies)\n\n" binomialFixture (length studies)

  -- REML
  putStrLn "[T3] Binomial REML τ² mode:"
  case springREMLBin studies Nothing (Just 500) (Just 1e-6) of
    Left e -> putStrLn $ "  FAIL: " ++ e
    Right (_, (tauHat, _)) -> do
      printf "  springREMLBin τ² = %.6f\n" tauHat

      -- GC sweep, find argmax
      putStrLn "[T3] Binomial GC argmax τ² (uniform grid kTau=500, nMax=4000):"
      case springGrandCanonicalBin studies 500 4000 of
        Left e -> putStrLn $ "  FAIL: " ++ e
        Right (gcMode, grid, _) -> do
          printf "  springGrandCanonicalBin argmax τ² = %.6f\n" gcMode
          let dT = abs (gcMode - tauHat)
              gridStep = 1.0 / 500
          printf "  |Δτ²| = %.6f   (grid step = %.6f)\n" dT gridStep
          if dT <= 2 * gridStep
             then putStrLn "  PASS (within 2 × grid steps)\n"
             else putStrLn "  FAIL: GC mode and REML mode differ by > 2 grid steps\n"

          -- T4: effects at the mode
          putStrLn "[T4] Effects at the GC mode vs an independent irlsSolve at GC mode:"
          let Right springsnet0 = makeBinomialSprings studies Nothing (Just 1.0)
              netAtMode = updateSprings springsnet0 gcMode
              neIrls    = irlsSolve netAtMode 200 1e-9
              -- springGrandCanonicalBin's grid is [(τ², logZ)]; argmax mode
              -- already returned as third tuple element 'modeNE' below.
          case grid of
            _ -> case springGrandCanonicalBin studies 500 4000 of
              Right (_, _, neGc) -> do
                let est1 = networkEstimates neGc
                    est2 = networkEstimates neIrls
                    pairs = [ (a, b, (est1 Map.! a) Map.! b, (est2 Map.! a) Map.! b)
                            | (a, row) <- Map.toList est1
                            , (b, _)   <- Map.toList row
                            , a /= b ]
                    diffs = [ abs (v1 - v2) | (_, _, v1, v2) <- pairs ]
                    maxDiff = if null diffs then 0 else maximum diffs
                printf "  max |Δ effect| over %d contrasts = %.3e\n"
                  (length pairs) maxDiff
                if maxDiff <= 1e-6
                  then putStrLn "  PASS (effects agree)\n"
                  else putStrLn "  FAIL: effect estimates disagree\n"
              _ -> putStrLn "  FAIL: GC re-run failed\n"
