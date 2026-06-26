{-# LANGUAGE OverloadedStrings #-}
-- Continuous (mean-difference) baseline-risk NMR, Haskell Gauss-Newton fit.
-- Usage: stack runghc -- test/nmr_baseline_risk_md.hs <studies.json> <ref>
module Main where

import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode (LineBuffering))
import Data.List (sort, nub)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..), tidOfArm)
import Data.Meta.Studies (StudyId (..), readStudies, getStudyArms)
import Data.Meta.NMA (nmaRegressionBaselineRiskMDB, nmaRegressionBaselineRiskMD,
                      BaselineSlopeModel (..), BaselineRiskFit (..))

nm :: TreatmentId -> String
nm (TreatmentId (StringId s)) = s
nm (TreatmentId (IntId i)) = show i

main :: IO ()
main = do
  args <- getArgs
  let (path, refName) = case args of (p:r:_) -> (p,r); [p] -> (p,"A"); _ -> ("test/nmr_br_md_sim.json","A")
      ref = TreatmentId (StringId refName)
  Right st <- readStudies path
  let nonref = filter (/= ref) $ sort $ nub $ concatMap (map tidOfArm . getStudyArms) st
  printf "=== Haskell continuous (MD) baseline-risk: %s  ref=%s ===\n\n" path refName
  case nmaRegressionBaselineRiskMD st ref of
    Left e -> putStrLn ("FE error: " ++ e)
    Right f -> do
      printf "FE shared  mx = %.5f\n" (brMx f)
      printf "FE shared  B  = %.5f (se %.5f)\n" (brB f) (brBse f)
      mapM_ (\t -> printf "FE  d[%-3s] = %8.5f (se %.5f)\n" (nm t) (brD f Map.! t) (brDse f Map.! t)) nonref
  putStrLn ""
  let run :: String -> BaselineSlopeModel -> IO ()
      run lbl model = case nmaRegressionBaselineRiskMDB st ref model Nothing of
        Left e -> printf "%s: ERROR %s\n" lbl e
        Right f -> do
          printf "%-12s B(main)=%.5f (se %.5f)%s\n" lbl (brB f) (brBse f)
                 (maybe "" (\s -> printf "  sigmaB2=%.5f" s :: String) (brSigmaB2 f))
          mapM_ (\t -> printf "    b[%-3s] = %+.5f (se %.5f)\n" (nm t) (brBt f Map.! t) (brBtSe f Map.! t)) nonref
  run "unrelated" BLUnrelated
  run "exchangeable" BLExchangeable
  case nmaRegressionBaselineRiskMDB st ref BLShared (Just 0.1) of
    Right f -> printf "\nRE shared  B=%.5f  tau2=%.5f\n" (brB f) (brTau2 f)
    Left e  -> putStrLn e
