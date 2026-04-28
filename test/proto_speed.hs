{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Speed comparison: irlsSolve (calls nma → O((k+T+A)^3) per call) vs
-- newtonSolveReducedBin (direct construction → O((k+T)^3) per call).
--
-- Same τ²-grid sweep on the same dataset; we time both pipelines and
-- report per-fit wall and grid-sweep totals. To avoid lazy-evaluation
-- artefacts the result of every fit is forced via a sum-of-everything
-- accumulator before the clock is read.
--
-- Run: stack runghc --package meta-analysis -- test/proto_speed.hs
module Main where

import qualified Data.Map.Strict as Map
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
import           Text.Printf     (printf)

import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects (..), makeBinomialSprings,
                                updateSprings, springs, springEnergy)
import Data.Meta.RandomEffects (irlsSolve, newtonSolveReducedBin)

-- Force the entire NetworkEffects by summing every numeric we can reach.
forceNE :: NetworkEffects -> Double
forceNE ne =
  let est = networkEstimates ne
      var = networkVariances ne
      sumNested m = sum [v | row <- Map.elems m, v <- Map.elems row]
      springsSum = sum $ map springEnergy $ Map.elems $ springs $ springNetwork ne
  in sumNested est + sumNested var + springsSum

timeIO :: IO Double -> IO (Double, Double)
timeIO act = do
  t0 <- getCurrentTime
  !x  <- act
  t1 <- getCurrentTime
  return (x, realToFrac (diffUTCTime t1 t0))

main :: IO ()
main = do
  let path = "test/synth_bin/synth_041.json"
  est <- readStudies path
  case est of
    Left err -> putStrLn $ "read err: " ++ err
    Right studies -> do
      printf "Dataset: %s   (k = %d studies)\n\n" path (length studies)
      case makeBinomialSprings studies Nothing (Just 1.0) of
        Left err -> putStrLn $ "build err: " ++ err
        Right springsnet0 -> do
          -- 30-point τ²-grid mimicking the GC pipeline.
          let grid = [0.001 * (50 ** (fromIntegral i / fromIntegral 29))
                     | i <- [0 .. 29 :: Int]]
              maxIter = 200
              eps     = 1e-9

              fitOld t2 = do
                let net = updateSprings springsnet0 t2
                let !ne  = irlsSolve net maxIter eps
                return $! forceNE ne
              fitNew t2 = do
                let net = updateSprings springsnet0 t2
                let !ne  = newtonSolveReducedBin net maxIter eps
                return $! forceNE ne

          -- Warm up to amortise GHC interpreter startup.
          (_, _) <- timeIO (fitOld 0.5)
          (_, _) <- timeIO (fitNew 0.5)

          printf "30 grid points, max %d Newton iters, eps = %.0e\n\n"
                  maxIter eps
          (sumOld, tOld) <- timeIO (do
            xs <- mapM fitOld grid
            return $! sum xs)
          (sumNew, tNew) <- timeIO (do
            xs <- mapM fitNew grid
            return $! sum xs)

          printf "%-32s  %10s  %10s  %10s\n"
                  ("method"::String)
                  ("total (s)"::String)
                  ("per fit"::String)
                  ("checksum"::String)
          printf "%-32s  %10.3f  %10.3f  %10.3e\n"
                  ("irlsSolve  (old: nma)"::String) tOld
                  (tOld / fromIntegral (length grid)) sumOld
          printf "%-32s  %10.3f  %10.3f  %10.3e\n"
                  ("newtonSolveReducedBin (new)"::String) tNew
                  (tNew / fromIntegral (length grid)) sumNew
          printf "\n  speedup: %.2fx\n" (tOld / tNew)
          printf "  checksum delta: %.3e\n" (abs (sumOld - sumNew))

          -- Per-grid-point correctness check: max |Δ| over all effect estimates.
          let maxAbsEffectDelta t2 =
                let net = updateSprings springsnet0 t2
                    eO  = networkEstimates (irlsSolve net maxIter eps)
                    eN  = networkEstimates (newtonSolveReducedBin net maxIter eps)
                    diffs = [ abs (vO - (eN Map.! a) Map.! b)
                            | (a, row) <- Map.toList eO
                            , (b, vO)  <- Map.toList row ]
                in if null diffs then 0 else maximum diffs
              tausShown = [0.001, 0.01, 0.1, 0.5, 1.0, 5.0, 50.0]
          putStrLn "\n  per-grid max |Δ| on effects (irls vs reduced):"
          mapM_ (\t2 -> do
            let !d = maxAbsEffectDelta t2
            printf "    τ²=%-8.3f  max|Δ| = %.3e\n" t2 d
            ) tausShown
