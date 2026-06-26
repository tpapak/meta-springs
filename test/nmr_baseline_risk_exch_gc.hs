{-# LANGUAGE OverloadedStrings #-}
-- 2-D grand-canonical posterior for exchangeable baseline-risk: integrate BOTH τ² and σ_B²
-- (slope-springs "coming and going" like the τ-springs), vs the grouped-REML/EM σ_B² point.
-- Usage: stack runghc -- test/nmr_baseline_risk_exch_gc.hs <studies.json> <ref>
module Main where

import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode (LineBuffering))
import Data.List (sort, nub)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..), tidOfArm)
import Data.Meta.Studies (StudyId (..), readStudies, getStudyArms)
import Data.Meta.NMA (nmaRegressionBaselineRiskBinExchGC, BaselineExchGC (..), Tau2PriorR (..))

nm :: TreatmentId -> String
nm (TreatmentId (StringId s)) = s
nm (TreatmentId (IntId i)) = show i

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let (path, refName) = case args of (p:r:_) -> (p,r); [p] -> (p,"A"); _ -> ("test/nmr_br_sim.json","A")
      ref = TreatmentId (StringId refName)
  Right st <- readStudies path
  let nonref = filter (/= ref) $ sort $ nub $ concatMap (map tidOfArm . getStudyArms) st
  printf "=== Exchangeable baseline-risk: 2-D GC Xi(tau2, sigmaB2) — %s ref=%s ===\n\n" path refName
  -- half-normal priors on the heterogeneity SD and slope-dispersion SD
  case nmaRegressionBaselineRiskBinExchGC st ref 40 (HalfNormalTauR 0.5) (HalfNormalTauR 0.5) of
    Left e -> putStrLn ("ERROR: " ++ e)
    Right g -> do
      let (sMode,sMean,sLo,sHi) = begSigmaB2 g
          (tMode,tMean,tLo,tHi) = begTau2 g
          (bM,bSd,bLo,bHi)      = begB g
      printf "sigma_B^2 :  EM point = %.5f  (REML, on the boundary)\n" (begEMSigmaB2 g)
      printf "          :  GC mode  = %.5f   GC mean = %.5f   95%% [%.5f, %.5f]\n" sMode sMean sLo sHi
      printf "          :  -> the GC integration lifts sigma_B^2 off the boundary\n\n"
      printf "tau^2     :  GC mode = %.5f   mean = %.5f   95%% [%.4f, %.4f]\n\n" tMode tMean tLo tHi
      printf "common B  :  mean = %.5f (sd %.5f)  95%% [%.4f, %.4f]\n" bM bSd bLo bHi
      mapM_ (\t -> let (m,sd,lo,hi) = begBt g Map.! t
                   in printf "  b[%-11s] mean = %+.5f (sd %.5f)  95%% [%.4f, %.4f]\n" (nm t) m sd lo hi) nonref
