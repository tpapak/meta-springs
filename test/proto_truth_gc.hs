{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import qualified Data.ByteString.Lazy as BL
import           Text.Printf          (printf)

import Data.Meta.Effects       (meanArm)
import Data.Meta.Studies       (readStudies)
import Data.Meta.RandomEffects (springGrandCanonical, springGrandCanonicalAdaptive,
                                Tau2Prior (..), posteriorUnder,
                                PosteriorSummary (..),
                                springREML)

main :: IO ()
main = do
  Right studies <- readStudies "/tmp/truth.json"
  let reml = case springREML studies meanArm Nothing (Just 500) (Just 1e-6) of
        Right (_, (t, _)) -> t
        _ -> 0/0
  let dense = case springGrandCanonical studies meanArm 1000 5000 of
        Right (_, g, _) -> g
        _ -> []
      adapt = case springGrandCanonicalAdaptive studies meanArm 50 of
        Right (_, g) -> [(t, lz) | (t, lz, _) <- g]
        _ -> []
  let dPS = posteriorUnder False dense (HalfNormalTau 1.0)
      aPS = posteriorUnder True  adapt (HalfNormalTau 1.0)
  printf "Truth τ²_contrast = 0.2500, REML = %.4f\n\n" reml
  printf "GC dense  : mode=%.4f med=%.4f mean=%.4f CI=(%.4f, %.4f)\n"
    (psMode dPS) (psMedian dPS) (psMean dPS) (fst (psCi95 dPS)) (snd (psCi95 dPS))
  printf "GC adapt  : mode=%.4f med=%.4f mean=%.4f CI=(%.4f, %.4f)\n"
    (psMode aPS) (psMedian aPS) (psMean aPS) (fst (psCi95 aPS)) (snd (psCi95 aPS))
  let mkObj ps = object [ "mode" .= psMode ps, "median" .= psMedian ps,
                          "mean" .= psMean ps,
                          "ci_lo" .= fst (psCi95 ps), "ci_hi" .= snd (psCi95 ps) ]
  BL.writeFile "/tmp/truth_gc.json" (A.encode (object
    [ "reml"  .= reml
    , "dense" .= mkObj dPS
    , "adapt" .= mkObj aPS
    ]))
