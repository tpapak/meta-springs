{-# LANGUAGE BangPatterns #-}
-- Verify springGrandCanonicalAdaptive (library) matches springGrandCanonical
-- on small fixture, then benchmark vs dense on T=50.
module Main where

import           Data.List       (sortBy)
import           Data.Ord        (comparing)
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
import           Text.Printf     (printf)

import Data.Meta.Effects       (meanArm)
import Data.Meta.Studies       (readStudies)
import Data.Meta.RandomEffects (springGrandCanonical, springGrandCanonicalAdaptive)

hnLogPrior :: Double -> Double
hnLogPrior t2 | t2 <= 0   = -1e30
              | otherwise = -0.5 * log t2 - 0.5 * t2

-- Compute posterior summary using a τ² prior (HalfN(0,1)) and Jacobian
-- correction for log-spaced grids.
postSummary :: Bool -> [(Double, Double)] -> (Double, Double, Double, Double)
postSummary logSpaced grid =
  let lps = [lz + hnLogPrior t2 + (if logSpaced then log t2 else 0)
             | (t2, lz) <- grid, t2 > 0]
      ts  = [t2 | (t2, _) <- grid, t2 > 0]
      m   = maximum lps
      ws  = map (\lp -> exp (lp - m)) lps
      tot = sum ws
      ws' = map (/ tot) ws
      sorted = sortBy (comparing fst) (zip ts ws')
      cum    = scanl1 (+) (map snd sorted)
      tsSorted = map fst sorted
      wQuant q = let xs = zip cum tsSorted
                     go [] = last tsSorted
                     go ((c, t):rest) | c >= q = t | otherwise = go rest
                 in go xs
      median = wQuant 0.5
      mean   = sum (zipWith (*) ts ws')
      lo     = wQuant 0.025
      hi     = wQuant 0.975
  in (median, mean, lo, hi)

main :: IO ()
main = do
  -- A. Small fixture verification
  putStrLn "=== A. continuous.json (k=6) — adaptive vs dense ==="
  Right s1 <- readStudies "test/continuous.json"
  let dense = case springGrandCanonical s1 meanArm 100 3000 of
                Right r -> r; _ -> error "dense"
      adapt = case springGrandCanonicalAdaptive s1 meanArm 50 of
                Right r -> r; _ -> error "adapt"
      (denseMode, denseGrid, _) = dense
      (adaptMode, adaptGridFull) = adapt
      adaptGrid = [(t, lz) | (t, lz, _) <- adaptGridFull]
      (dMed, dMean, dLo, dHi) = postSummary False denseGrid
      (aMed, aMean, aLo, aHi) = postSummary True  adaptGrid
  printf "  τ² argmax (no prior):   dense=%.4f  adaptive=%.4f\n" denseMode adaptMode
  printf "  posterior median:       dense=%.4f  adaptive=%.4f  |Δ|=%.4f\n" dMed aMed (abs (dMed-aMed))
  printf "  posterior mean:         dense=%.4f  adaptive=%.4f  |Δ|=%.4f\n" dMean aMean (abs (dMean-aMean))
  printf "  posterior 2.5%%:         dense=%.4f  adaptive=%.4f  |Δ|=%.4f\n" dLo aLo (abs (dLo-aLo))
  printf "  posterior 97.5%%:        dense=%.4f  adaptive=%.4f  |Δ|=%.4f\n" dHi aHi (abs (dHi-aHi))

  -- B. T=50 timing benchmark
  putStrLn ""
  putStrLn "=== B. /tmp/g50.json (T=50, k=200) — wall time ==="
  Right s2 <- readStudies "/tmp/g50.json"
  printf "  %d studies\n" (length s2)
  t0 <- getCurrentTime
  let !_ = case springGrandCanonicalAdaptive s2 meanArm 50 of
             Right (m, _) -> m; _ -> 0/0
  t1 <- getCurrentTime
  printf "  springGrandCanonicalAdaptive (50 pts):  %.2f s\n"
         (realToFrac (diffUTCTime t1 t0) :: Double)
  printf "  reference dense (200 pts) was 112 s.\n"
