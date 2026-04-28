{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- GC fit on /tmp/t20.json. Same HalfNormal(0,1) prior on τ.
-- Writes /tmp/gc_t20.json with τ² mode/median/mean/CI + every contrast.

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

hnLogPrior :: Double -> Double
hnLogPrior t2 | t2 <= 0   = -1e30
              | otherwise = -0.5 * log t2 - 0.5 * t2

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
      go ((c, x) : rest) | c >= q    = x
                         | otherwise = go rest
  in go (zip cum (map fst sorted))

main :: IO ()
main = do
  let path = "/tmp/t20.json"
  est <- readStudies path
  case est of
    Left err -> putStrLn $ "read err: " ++ err
    Right studies -> do
      t0 <- getCurrentTime
      case springGrandCanonicalBinAdaptive studies 100 of
        Left err -> putStrLn $ "GC err: " ++ err
        Right (_modeFlat, gridRes) -> do
          let !t2s = [t  | (t, _, _) <- gridRes]
              !lzs = [lz | (_, lz, _) <- gridRes]
              wsHN = weights t2s lzs
              !tau2_med  = wQuantile t2s wsHN 0.5
              !tau2_lo   = wQuantile t2s wsHN 0.025
              !tau2_hi   = wQuantile t2s wsHN 0.975
              !tau2_mean = sum (zipWith (*) t2s wsHN)
              lps = zipWith (\t lz -> lz + hnLogPrior t) t2s lzs
              !tau2_mode = fst $ foldr1 (\a b -> if snd a >= snd b then a else b)
                                        (zip t2s lps)
              showTid (TreatmentId t) = show t
              allPairs :: [(TreatmentId, TreatmentId)]
              allPairs = [ (a, b)
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
                in (showTid a, showTid b, mixMean,
                    mixMean - 1.96 * sd_, mixMean + 1.96 * sd_, sd_)
              effs = map effSummary allPairs

          t1 <- getCurrentTime
          let wall = realToFrac (diffUTCTime t1 t0) :: Double
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
                    , "ci_hi"  .= tau2_hi ]
                , "effects" .= [ object [ "from" .= a, "to" .= b
                                        , "mean" .= m, "ci_lo" .= lo
                                        , "ci_hi" .= hi, "sd" .= s ]
                              | (a, b, m, lo, hi, s) <- effs ]
                ]
          BL.writeFile "/tmp/gc_t20.json" (A.encode out)
          printf "GC        wall=%.3fs  tau2 mode=%.3f median=%.3f mean=%.3f CI=(%.3f,%.3f)\n"
                 wall tau2_mode tau2_med tau2_mean tau2_lo tau2_hi
