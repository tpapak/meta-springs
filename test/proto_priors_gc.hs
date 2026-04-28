{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Single GC fit on /tmp/g50.json, then evaluate the τ² posterior under
-- multiple τ-priors using the same cached grid. Writes /tmp/gc_priors.json.
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
  [ ("HalfN(0,1)",    HalfNormalTau 1.0)
  , ("HalfN(0,0.5)",  HalfNormalTau 0.5)
  , ("HalfN(0,2)",    HalfNormalTau 2.0)
  , ("HalfCauchy(0,1)",   HalfCauchyTau 1.0)
  , ("HalfCauchy(0,0.25)",HalfCauchyTau 0.25)
  ]

main :: IO ()
main = do
  Right studies <- readStudies "/tmp/g50.json"
  putStrLn "GC adaptive fit + τ² posteriors under 5 priors"
  t0 <- getCurrentTime
  case springGrandCanonicalAdaptive studies meanArm 50 of
    Left e -> putStrLn ("err: " ++ e)
    Right (_, gridFull) -> do
      -- Force the lazy grid by summing all logZ values before stopping clock.
      let !forceLZ = sum [lz | (_, lz, _) <- gridFull]
      t1 <- forceLZ `seq` getCurrentTime
      let fitWall = realToFrac (diffUTCTime t1 t0) :: Double
          grid   = [(t, lz) | (t, lz, _) <- gridFull]

          summarise (lbl, pr) =
            let ps  = posteriorUnder True grid pr
            in (lbl, ps)

      tA <- getCurrentTime
      let !results = map summarise priors
      tB <- getCurrentTime
      let postWall = realToFrac (diffUTCTime tB tA) :: Double

      printf "  GC fit (1×):                  %.2f s\n" fitWall
      printf "  All %d posterior reweights:    %.4f s\n" (length priors) postWall
      printf "  Total:                         %.2f s\n\n" (fitWall + postWall)

      printf "%-22s %-9s %-9s %-9s %-22s\n"
        ("prior"::String) ("mode"::String)
        ("median"::String) ("mean"::String) ("95% CI"::String)
      printf "%s\n" (replicate 75 '-')
      mapM_ (\(lbl, ps) ->
        let (lo, hi) = psCi95 ps
        in printf "%-22s %-9.4f %-9.4f %-9.4f (%.4f, %.4f)\n"
                  lbl (psMode ps) (psMedian ps) (psMean ps) lo hi
        ) results

      let json = object
            [ "fit_wall_sec"  .= fitWall
            , "post_wall_sec" .= postWall
            , "total_wall_sec".= (fitWall + postWall)
            , "summaries" .=
                [ object [ "prior"  .= lbl
                         , "mode"   .= psMode ps
                         , "median" .= psMedian ps
                         , "mean"   .= psMean ps
                         , "ci_lo"  .= fst (psCi95 ps)
                         , "ci_hi"  .= snd (psCi95 ps) ]
                | (lbl, ps) <- results ]
            ]
      BL.writeFile "/tmp/gc_priors.json" (A.encode json)
      putStrLn "\nWrote /tmp/gc_priors.json"
