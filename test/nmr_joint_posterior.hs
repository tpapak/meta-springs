{-# LANGUAGE OverloadedStrings #-}
-- Joint (beta, tau2) posterior for NMR via nmaRegressionPosterior, over a set of
-- simulated reps. CSV: rep,beta_mean,beta_sd,beta_lo,beta_hi,tau2_mode,tau2_mean,tau2_lo,tau2_hi
module Main where

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import           System.Environment   (getArgs)
import           Text.Printf          (printf)

import Data.Meta.Effects (TreatmentId (..), StringIntId (..), meanArm)
import Data.Meta.Studies (StudyId (..), readStudies)
import Data.Meta.NMA     (nmaRegressionPosterior, RegPosterior (..))

main :: IO ()
main = do
  args <- getArgs
  let reps = if null args then [1..15] else [read (head args) .. read (args !! 1)] :: [Int]
  Just covRaw <- A.decode <$> BL.readFile "/tmp/nmrgc/cov.json" :: IO (Maybe (Map.Map String Double))
  let cov = Map.fromList [ (StudyId (StringId k), v) | (k, v) <- Map.toList covRaw ]
  putStrLn "rep,beta_mean,beta_sd,beta_lo,beta_hi,tau2_mode,tau2_mean,tau2_lo,tau2_hi"
  mapM_ (\r -> do
    est <- readStudies (printf "/tmp/nmrgc/rep_%03d.json" r)
    case est of
      Left e -> printf "%d,NA -- %s\n" r e
      Right studies -> case nmaRegressionPosterior studies cov 160 meanArm of
        Left e -> printf "%d,NA -- %s\n" r e
        Right rp -> do
          let (bm,bs,bl,bh) = rpBeta rp
              (tmo,tme,tl,th) = rpTau2 rp
          printf "%d,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f\n" r bm bs bl bh tmo tme tl th
    ) reps
