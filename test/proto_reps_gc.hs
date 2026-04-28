{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Fit GC dense on 20 reps; dump τ² posterior summary + per-contrast
-- posterior summary per rep. Uses HalfNormal(0,1) prior on τ.
module Main where

import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import           Data.List            (sortBy)
import           Data.Ord             (comparing)
import           Text.Printf          (printf)

import Data.Meta.Effects       (TreatmentId (..), meanArm)
import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects (..))
import Data.Meta.RandomEffects (springGrandCanonical,
                                Tau2Prior (..), posteriorUnder,
                                PosteriorSummary (..), logTau2Prior)

main :: IO ()
main = do
  let n_reps = 20
  putStrLn "GC dense fit on 20 reps"
  results <- mapM (\rep -> do
    let path = "/tmp/reps/rep_" ++ printf "%02d" (rep :: Int) ++ ".json"
    Right studies <- readStudies path
    case springGrandCanonical studies meanArm 1000 5000 of
      Left e -> error e
      Right (_, grid, modeNE) -> do
        -- REML estimate = argmax of log Z under flat τ² prior
        -- (= mode of the marginal likelihood, exactly what springREML computes)
        let psFlat   = posteriorUnder False grid FlatTau2
            reml     = psMode psFlat
        -- τ² posterior summary under HalfN(0,1)
        let ps = posteriorUnder False grid (HalfNormalTau 1.0)
        -- Effect posterior: mixture of conditional Gaussians at each grid pt.
        -- Each grid point has NetworkEffects, but the dense-grid call only
        -- returned modeNE (one). For full mixture we'd need per-grid NEs.
        -- For this comparison we use modeNE's estimate (point estimate from
        -- the GC argmax) and the posterior mean of the contrast under the
        -- mixture would require evalXi-with-NE. As a proxy, use modeNE.
        let est = networkEstimates modeNE
            showT (TreatmentId t) = show t
            effs =
              [ (showT a ++ "->" ++ showT b, v)
              | (a, row) <- Map.toList est
              , (b, v)   <- Map.toList row
              , a /= b ]
        printf "  rep %02d: REML=%.4f τ² mean=%.4f mode=%.4f\n"
          rep reml (psMean ps) (psMode ps)
        return $ object
          [ "rep"          .= rep
          , "reml"         .= reml
          , "tau2_mode"    .= psMode ps
          , "tau2_median"  .= psMedian ps
          , "tau2_mean"    .= psMean ps
          , "tau2_ci_lo"   .= fst (psCi95 ps)
          , "tau2_ci_hi"   .= snd (psCi95 ps)
          , "effects" .= [ object [ "label" .= lbl, "mean" .= v ]
                         | (lbl, v) <- effs ]
          ]
    ) [1 .. n_reps]
  BL.writeFile "/tmp/reps_gc.json" (A.encode (object [ "reps" .= results ]))
  putStrLn "\nWrote /tmp/reps_gc.json"
