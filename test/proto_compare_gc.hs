{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Single GC fit on synth_041 with the same HalfNormal(0,1) prior on τ.
-- Uses springGrandCanonicalBinAdaptive (which now goes through
-- newtonSolveReducedBin internally). Writes /tmp/gc_compare.json with
-- tau2 posterior summary + every pairwise contrast (Laplace mixture mean
-- + 95% CI) + wall time.
--
-- Run: stack runghc --package meta-analysis -- test/proto_compare_gc.hs
module Main where

import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import qualified Data.ByteString.Lazy as BL
import           Data.List            (sortBy)
import           Data.Ord             (comparing)
import qualified Data.Map.Strict      as Map
import           Data.Time.Clock      (getCurrentTime, diffUTCTime)
import           Text.Printf          (printf)

import Data.Meta.Effects       (TreatmentId (..))
import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects (..))
import Data.Meta.RandomEffects (springGrandCanonicalBinAdaptive)

-- HalfNormal(0,1) on τ → log-prior on τ²: -½ log(τ²) - ½ τ² (drop const).
hnLogPrior :: Double -> Double
hnLogPrior t2 | t2 <= 0   = -1e30
              | otherwise = -0.5 * log t2 - 0.5 * t2

-- Posterior weights from a (τ², logZ) grid under HN(0,1).
weights :: [Double] -> [Double] -> [Double]
weights t2s lzs =
  let lps = zipWith (\t lz -> lz + hnLogPrior t) t2s lzs
      m   = maximum lps
      ws  = map (\lp -> exp (lp - m)) lps
      tot = sum ws
  in map (/ tot) ws

wQuantile :: [Double] -> [Double] -> Double -> Double
wQuantile xs ws q =
  let sorted = sortBy (comparing fst) (zip xs ws)
      cum    = scanl1 (+) (map snd sorted)
      go [] = fst (last sorted)
      go ((c, x) : rest)
        | c >= q    = x
        | otherwise = go rest
  in go (zip cum (map fst sorted))

main :: IO ()
main = do
  let path = "test/synth_bin/synth_041.json"
  est <- readStudies path
  case est of
    Left err -> putStrLn $ "read err: " ++ err
    Right studies -> do
      t0 <- getCurrentTime
      case springGrandCanonicalBinAdaptive studies 100 of
        Left err -> putStrLn $ "GC err: " ++ err
        Right (_modeFlat, gridRes) -> do
          -- Force the grid by computing summaries before stopping the clock.
          let !t2s = [t  | (t, _, _) <- gridRes]
              !lzs = [lz | (_, lz, _) <- gridRes]
              wsHN = weights t2s lzs
              !tau2_med = wQuantile t2s wsHN 0.5
              !tau2_lo  = wQuantile t2s wsHN 0.025
              !tau2_hi  = wQuantile t2s wsHN 0.975
              !tau2_mean = sum (zipWith (*) t2s wsHN)

              -- mode τ² under HN prior = grid argmax of (logZ + log prior)
              lps = zipWith (\t lz -> lz + hnLogPrior t) t2s lzs
              !tau2_mode = fst $ foldr1 (\a b -> if snd a >= snd b then a else b)
                                        (zip t2s lps)

              -- Contrast posteriors: mixture of conditional Gaussians.
              -- Iterate over (TreatmentId, TreatmentId) pairs from the first
              -- grid point's network and look up by key in every grid point.
              showTid (TreatmentId t) = show t
              allPairs :: [(TreatmentId, TreatmentId)]
              allPairs =
                [ (a, b)
                | (_, _, ne) <- take 1 gridRes
                , (a, row) <- Map.toList (networkEstimates ne)
                , (b, _)   <- Map.toList row
                , a /= b ]

              effSummary (a, b) =
                let mus  = [ (networkEstimates ne Map.! a) Map.! b
                           | (_, _, ne) <- gridRes ]
                    vars = [ (networkVariances ne Map.! a) Map.! b
                           | (_, _, ne) <- gridRes ]
                    mixMean = sum (zipWith (*) wsHN mus)
                    mixVar  = sum (zipWith3 (\w m v -> w * (m * m + v))
                                            wsHN mus vars)
                              - mixMean * mixMean
                    sd_   = sqrt (max 0 mixVar)
                    ciLo  = mixMean - 1.96 * sd_
                    ciHi  = mixMean + 1.96 * sd_
                in (showTid a, showTid b, mixMean, ciLo, ciHi, sd_)
              effs = map effSummary allPairs

          t1 <- getCurrentTime
          let wall = realToFrac (diffUTCTime t1 t0) :: Double

          -- Write JSON
          let out = object
                [ "dataset" .= path
                , "prior"   .= ("HalfNormal(0,1) on tau" :: String)
                , "wall_sec" .= wall
                , "grid_nPts" .= length gridRes
                , "tau2" .= object
                    [ "mode"   .= tau2_mode
                    , "median" .= tau2_med
                    , "mean"   .= tau2_mean
                    , "ci_lo"  .= tau2_lo
                    , "ci_hi"  .= tau2_hi
                    ]
                , "effects" .= [ object [ "from" .= a
                                        , "to"   .= b
                                        , "mean" .= m
                                        , "ci_lo" .= lo
                                        , "ci_hi" .= hi
                                        , "sd"    .= s ]
                              | (a, b, m, lo, hi, s) <- effs ]
                ]
          BL.writeFile "/tmp/gc_compare.json" (A.encode out)
          printf "GC        wall=%.3fs  tau2 median=%.3f  mean=%.3f  CI=(%.3f,%.3f)\n"
                 wall tau2_med tau2_mean tau2_lo tau2_hi
