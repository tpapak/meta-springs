{-# LANGUAGE OverloadedStrings #-}
-- Prior-sensitivity sweep on continuouslong.json (Parkinson's off-time
-- reduction NMA, T=4, k=32). One GC fit, evaluate under 5 τ-priors.
module Main where

import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import qualified Data.ByteString.Lazy as BL
import           Data.Time.Clock      (getCurrentTime, diffUTCTime)
import           Text.Printf          (printf)

import Data.Meta.Effects       (meanArm)
import Data.Meta.Studies       (readStudies)
import Data.Meta.RandomEffects (springGrandCanonicalAdaptive,
                                Tau2Prior (..), posteriorUnder,
                                PosteriorSummary (..))

priors :: [(String, Tau2Prior)]
priors =
  [ ("Flat (REML)",            FlatTau2)
  , ("HalfNormal(τ; σ=0.5)",   HalfNormalTau 0.5)
  , ("HalfNormal(τ; σ=1)",     HalfNormalTau 1.0)
  , ("HalfNormal(τ; σ=5)",     HalfNormalTau 5.0)
  , ("HalfCauchy(τ; σ=0.5)",   HalfCauchyTau 0.5)
  ]

main :: IO ()
main = do
  Right studies <- readStudies "test/continuouslong.json"
  printf "GC fit on continuouslong (%d studies, Parkinson)\n" (length studies)
  t0 <- getCurrentTime
  case springGrandCanonicalAdaptive studies meanArm 100 of   -- ~100 log-spaced pts
    Left e -> putStrLn ("err: " ++ e)
    Right (_, gridFull) -> do
      let grid = [(t, lz) | (t, lz, _) <- gridFull]
          !forceLZ = sum [lz | (_, lz) <- grid]
      t1 <- forceLZ `seq` getCurrentTime
      let fitWall = realToFrac (diffUTCTime t1 t0) :: Double
      tA <- getCurrentTime
      let !rs = map (\(lbl, pr) -> (lbl, posteriorUnder True grid pr)) priors
      tB <- getCurrentTime
      let postWall = realToFrac (diffUTCTime tB tA) :: Double
      printf "GC fit (1×):                  %.3f s\n" fitWall
      printf "All %d posterior reweights:    %.4f s\n" (length priors) postWall
      printf "Total:                         %.3f s\n\n" (fitWall + postWall)
      printf "%-26s %-9s %-9s %-9s %-22s\n"
        ("prior"::String) ("τ² mode"::String)
        ("τ² median"::String) ("τ² mean"::String) ("τ² 95% CI"::String)
      printf "%s\n" (replicate 80 '-')
      mapM_ (\(lbl, ps) -> do
        let (lo, hi) = psCi95 ps
        printf "%-26s %-9.4f %-9.4f %-9.4f (%.4f, %.4f)\n"
          lbl (psMode ps) (psMedian ps) (psMean ps) lo hi
        ) rs

      let json = object
            [ "fit_wall_sec"  .= fitWall
            , "post_wall_sec" .= postWall
            , "results" .=
                [ object [ "label"  .= lbl
                         , "tau2_mode"   .= psMode ps
                         , "tau2_median" .= psMedian ps
                         , "tau2_mean"   .= psMean ps
                         , "tau2_lo"     .= fst (psCi95 ps)
                         , "tau2_hi"     .= snd (psCi95 ps) ]
                | (lbl, ps) <- rs ]
            ]
      BL.writeFile "/tmp/park_gc.json" (A.encode json)
      putStrLn "\nWrote /tmp/park_gc.json"
