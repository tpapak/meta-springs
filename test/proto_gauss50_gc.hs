{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- GC Gaussian fit on /tmp/g50.json. Uses springGrandCanonical (the exact
-- Gaussian path — no Laplace, just analytic per-arm τ-aux integration plus
-- 1-D τ² grid quadrature).
module Main where

import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import qualified Data.ByteString.Lazy as BL
import           Data.List            (sortBy)
import           Data.Ord             (comparing)
import qualified Data.Map.Strict      as Map
import           Data.Time.Clock      (getCurrentTime, diffUTCTime)
import           Text.Printf          (printf)

import Data.Meta.Effects       (TreatmentId (..), meanArm)
import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects (..))
import Data.Meta.RandomEffects (springGrandCanonical)

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
      go ((c, x) : rest) | c >= q = x | otherwise = go rest
  in go (zip cum (map fst sorted))

main :: IO ()
main = do
  Right studies <- readStudies "/tmp/g50.json"
  t0 <- getCurrentTime
  -- 201-point grid (kTau=100, nMax=200, τ² ∈ [0, 2] step 0.01).
  -- 5001-pt sweep took ~3 hr at this size — pure-Haskell linear algebra
  -- inside, no LAPACK on this path.
  case springGrandCanonical studies meanArm 100 200 of
    Left err -> putStrLn $ "GC err: " ++ err
    Right (modeFlat, grid, modeNE) -> do
      let !t2s = map fst grid
          !lzs = map snd grid
          wsHN = weights t2s lzs
          !tau2_med = wQuantile t2s wsHN 0.5
          !tau2_lo  = wQuantile t2s wsHN 0.025
          !tau2_hi  = wQuantile t2s wsHN 0.975
          !tau2_mean = sum (zipWith (*) t2s wsHN)
          lps = zipWith (\t lz -> lz + hnLogPrior t) t2s lzs
          !tau2_mode = fst $ foldr1 (\a b -> if snd a >= snd b then a else b) (zip t2s lps)

          showTid (TreatmentId t) = show t
          est = networkEstimates modeNE
          var = networkVariances modeNE
          -- For Gaussian GC at the mode-of-Ξ point: report the network's
          -- pointwise estimate + variance at that τ² (no mixture in this
          -- prototype; full mixture would need per-grid NetworkEffects).
          effs = [ (showTid a, showTid b,
                   (est Map.! a) Map.! b,
                   sqrt (max 0 ((var Map.! a) Map.! b)))
                 | (a, row) <- Map.toList est
                 , (b, _) <- Map.toList row
                 , a /= b ]

      t1 <- getCurrentTime
      let wall = realToFrac (diffUTCTime t1 t0) :: Double
      let out = object
            [ "wall_sec" .= wall
            , "grid_pts" .= length grid
            , "mode_flat" .= modeFlat
            , "tau2" .= object
                [ "mode" .= tau2_mode, "median" .= tau2_med
                , "mean" .= tau2_mean
                , "ci_lo" .= tau2_lo, "ci_hi" .= tau2_hi ]
            , "effects" .= [ object [ "from" .= a, "to" .= b
                                    , "mean" .= m, "sd" .= s ]
                           | (a, b, m, s) <- effs ]
            ]
      BL.writeFile "/tmp/gc_g50.json" (A.encode out)
      printf "GC        wall=%.3fs  tau2 mode=%.4f median=%.4f CI=(%.4f,%.4f)\n"
             wall tau2_mode tau2_med tau2_lo tau2_hi
