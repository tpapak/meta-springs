{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import qualified Data.ByteString.Lazy as BL
import           Data.Time.Clock      (getCurrentTime, diffUTCTime)
import           Text.Printf          (printf)

import Data.Meta.Effects       (meanArm)
import Data.Meta.Studies       (readStudies)
import Data.Meta.RandomEffects (springGrandCanonical,
                                Tau2Prior (..), posteriorUnder,
                                PosteriorSummary (..))

priors :: [(String, Tau2Prior)]
priors =
  [ ("Flat / REML mode",     FlatTau2)            -- argmax = REML
  , ("HalfNormal(0,0.5) on τ", HalfNormalTau 0.5)
  , ("HalfNormal(0,1)   on τ", HalfNormalTau 1.0)
  , ("HalfCauchy(0,0.5) on τ", HalfCauchyTau 0.5)
  ]

main :: IO ()
main = do
  Right studies <- readStudies "/tmp/crins.json"
  printf "GC fit on Crins2014 (%d studies, T=2 pairwise)\n" (length studies)
  t0 <- getCurrentTime
  case springGrandCanonical studies meanArm 1000 5000 of
    Left e -> putStrLn ("err: " ++ e)
    Right (_, grid, _) -> do
      let !forceLZ = sum [lz | (_, lz) <- grid]
      t1 <- forceLZ `seq` getCurrentTime
      let fitWall = realToFrac (diffUTCTime t1 t0) :: Double

      -- For each prior, posterior τ²
      tA <- getCurrentTime
      let !rs = map (\(lbl, pr) -> (lbl, pr, posteriorUnder False grid pr)) priors
      tB <- getCurrentTime
      let postWall = realToFrac (diffUTCTime tB tA) :: Double

      printf "GC fit (1×):           %.3f s\n" fitWall
      printf "All %d posterior reweights: %.4f s\n" (length priors) postWall
      printf "Total: %.3f s\n\n" (fitWall + postWall)
      printf "%-26s %-7s %-7s %-7s %-22s\n"
        ("prior"::String) ("τ med"::String) ("τ mean"::String)
        ("τ mode"::String) ("τ 95%CI"::String)
      printf "%s\n" (replicate 80 '-')

      -- Convert τ² summary → τ summary (sqrt of τ² grid quantiles, not the
      -- quantiles of sqrt(τ²)).  bayesmeta reports τ; so for fair comparison
      -- we report τ ≈ sqrt(τ²) at each summary point.
      mapM_ (\(lbl, _, ps) -> do
        let mod_  = sqrt (psMode ps)
            med_  = sqrt (psMedian ps)
            mn_   = sqrt (psMean ps)
            (lo, hi) = psCi95 ps
        printf "%-26s %-7.4f %-7.4f %-7.4f (%.4f, %.4f)\n"
          lbl med_ mn_ mod_ (sqrt lo) (sqrt hi)
        ) rs

      let json = object
            [ "fit_wall_sec"  .= fitWall
            , "post_wall_sec" .= postWall
            , "results" .=
                [ object [ "label" .= lbl
                         , "tau_median" .= sqrt (psMedian ps)
                         , "tau_mean"   .= sqrt (psMean ps)
                         , "tau_mode"   .= sqrt (psMode ps)
                         , "tau_lo"     .= sqrt (fst (psCi95 ps))
                         , "tau_hi"     .= sqrt (snd (psCi95 ps)) ]
                | (lbl, _, ps) <- rs ]
            ]
      BL.writeFile "/tmp/crins_gc.json" (A.encode json)
      putStrLn "\nWrote /tmp/crins_gc.json"
