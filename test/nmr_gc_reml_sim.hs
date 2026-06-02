{-# LANGUAGE OverloadedStrings #-}
-- Reads /tmp/nmrgc/rep_NNN.json + cov.json. Per rep:
--   REML regression (nmaRegressionRE) -> beta_hat, tau2_reml
--   adjust data by beta_hat*x on non-reference arms, then on the adjusted data:
--     springREML  -> tau2_springreml
--     springGrandCanonicalAdaptive -> GC tau2 (posterior mode & mean, flat prior)
-- Emits CSV to stdout: rep,beta_reml,tau2_reml,tau2_springreml,gc_mode,gc_mean
module Main where

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe)
import           Text.Printf          (printf)

import Data.Meta.Effects   (TreatmentId (..), StringIntId (..), Arm (..), meanArm, tidOfArm)
import Data.Meta.Studies   (Study (..), StudyId (..), getStudyId, getStudyArms, readStudies)
import Data.Meta.NMA       (nmaRegressionRE)
import Data.Meta.RandomEffects (springREML, springGrandCanonicalAdaptive,
                                posteriorUnder, PosteriorSummary (..), Tau2Prior (..))

ref :: TreatmentId
ref = TreatmentId (StringId "A")

-- subtract beta*x from every non-reference arm's mean
adjust :: Double -> Map.Map StudyId Double -> Study -> Study
adjust beta cov (ContinuousStudy sid arms) =
  let x = Map.findWithDefault 0 sid cov
      f a@(ContinuousArm tid m sd n)
        | tid == ref = a
        | otherwise  = ContinuousArm tid (m - beta * x) sd n
      f a = a
  in ContinuousStudy sid (map f arms)
adjust _ _ s = s

main :: IO ()
main = do
  Just covRaw <- A.decode <$> BL.readFile "/tmp/nmrgc/cov.json" :: IO (Maybe (Map.Map String Double))
  let cov = Map.fromList [ (StudyId (StringId k), v) | (k, v) <- Map.toList covRaw ]
  putStrLn "rep,beta_reml,tau2_reml,tau2_springreml,gc_mode,gc_mean"
  mapM_ (oneRep cov) [1 .. 200 :: Int]

oneRep :: Map.Map StudyId Double -> Int -> IO ()
oneRep cov r = do
  est <- readStudies (printf "/tmp/nmrgc/rep_%03d.json" r)
  case est of
    Left e -> printf "%d,NA,NA,NA,NA,NA  -- read err %s\n" r e
    Right studies ->
      case nmaRegressionRE studies cov (Just 0.1) meanArm of
        Left e -> printf "%d,NA,NA,NA,NA,NA  -- reml err %s\n" r e
        Right (beta, tau2reml, _) -> do
          let adj = map (adjust beta cov) studies
              sreml = case springREML adj meanArm (Just 0.1) Nothing Nothing of
                        Right (_, (t2, _)) -> t2 ; Left _ -> (-1)
              (gcMode, gcMean) =
                case springGrandCanonicalAdaptive adj meanArm 60 of
                  Right (_, grid) ->
                    let g = [ (t, lz) | (t, lz, _) <- grid, t > 0 ]
                        ps = posteriorUnder True g FlatTau2
                    in (psMode ps, psMean ps)
                  Left _ -> (-1, -1)
          printf "%d,%.5f,%.5f,%.5f,%.5f,%.5f\n" r beta tau2reml sreml gcMode gcMean
