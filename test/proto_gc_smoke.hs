{-# LANGUAGE OverloadedStrings #-}
-- Smoke test: run the GC binomial pipeline (now wired through
-- newtonSolveReducedBin) on synth_041 and verify that the per-grid-point
-- NetworkEffects match what the full irlsSolve produces on the same network.
--
-- Run: stack runghc --package meta-analysis -- test/proto_gc_smoke.hs
module Main where

import qualified Data.Map.Strict as Map
import           Text.Printf     (printf)

import Data.Meta.Effects       (TreatmentId (..))
import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects (..), makeBinomialSprings, updateSprings)
import Data.Meta.RandomEffects (springGrandCanonicalBinAdaptive,
                                irlsSolve, newtonSolveReducedBin)

main :: IO ()
main = do
  let path = "test/synth_bin/synth_041.json"
  est <- readStudies path
  case est of
    Left err -> putStrLn $ "read err: " ++ err
    Right studies -> do

      ----------------------------------------------------------------
      -- 1. Side-by-side at a few fixed τ² values: irlsSolve vs reduced.
      ----------------------------------------------------------------
      case makeBinomialSprings studies Nothing (Just 1.0) of
        Left err -> putStrLn $ "build err: " ++ err
        Right springsnet0 -> do
          putStrLn "Side-by-side at fixed τ² (irlsSolve vs newtonSolveReducedBin):"
          printf "%-8s %-10s %14s %14s %14s\n"
            ("τ²" :: String) ("contrast" :: String)
            ("irls" :: String) ("reduced" :: String) ("|Δ|" :: String)
          mapM_ (\t2 -> do
            let net   = updateSprings springsnet0 t2
                neF   = irlsSolve net 200 1e-9
                neR   = newtonSolveReducedBin net 200 1e-9
                eF    = networkEstimates neF
                eR    = networkEstimates neR
                showT (TreatmentId t) = show t
                pairs =
                  [ (showT a ++ "->" ++ showT b, vF, vR, abs (vF - vR))
                  | (a, bm) <- Map.toList eF
                  , (b, vF) <- Map.toList bm
                  , let vR = (eR Map.! a) Map.! b
                  , a /= b ]
                maxDiff = if null pairs then 0
                          else maximum [d | (_, _, _, d) <- pairs]
            mapM_ (\(lbl, vF, vR, d) ->
                    printf "%-8.2f %-10s %14.6f %14.6f %14.3e\n"
                           t2 lbl vF vR d)
                  (take 3 pairs)
            printf "  ...max |Δ| over 30 contrasts at τ²=%.2f: %.3e\n"
                   t2 maxDiff
            putStrLn "") [0.1, 0.5, 1.5]

      ----------------------------------------------------------------
      -- 2. Run the full GC adaptive pipeline (uses reduced Newton internally).
      ----------------------------------------------------------------
      putStrLn "Full GC adaptive run (uses reducedNewton internally):"
      case springGrandCanonicalBinAdaptive studies 50 of
        Left err -> putStrLn $ "GC err: " ++ err
        Right (modeTau2, grid) -> do
          printf "  studies = %d\n" (length studies)
          printf "  mode τ² = %.6f\n" modeTau2
          printf "  grid points = %d\n" (length grid)
          let logZs = [lz | (_, lz, _) <- grid]
          printf "  log Z range: [%.4f, %.4f]\n"
                 (minimum logZs) (maximum logZs)
