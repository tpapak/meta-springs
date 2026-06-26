{-# LANGUAGE OverloadedStrings #-}
-- Baseline-risk binary NMR (Achana/bnma model), Haskell Fisher-scoring fit.
-- Usage: stack runghc -- test/nmr_baseline_risk.hs <studies.json> <refTreatment>
-- Discovers the non-reference treatments from the data. Prints FE and RE fits.
module Main where

import System.Environment (getArgs)
import Data.List (sort, nub)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..), tidOfArm)
import Data.Meta.Studies (StudyId (..), readStudies, getStudyArms)
import Data.Meta.NMA (nmaRegressionBaselineRiskBin, nmaRegressionBaselineRiskBinRE,
                      BaselineRiskFit (..))

nameOf :: TreatmentId -> String
nameOf (TreatmentId (StringId s)) = s
nameOf (TreatmentId (IntId i))    = show i

main :: IO ()
main = do
  args <- getArgs
  let (path, refName) = case args of
        (p:r:_) -> (p, r)
        [p]     -> (p, "A")
        _       -> ("test/nmr_certolizumab.json", "Placebo")
      ref = TreatmentId (StringId refName)
  Right st <- readStudies path
  let trts   = sort $ nub $ concatMap (map tidOfArm . getStudyArms) st
      nonref = filter (/= ref) trts
  printf "=== Haskell baseline-risk (Achana/bnma model): %s  ref=%s ===\n\n" path refName
  case nmaRegressionBaselineRiskBin st ref of
    Left e -> putStrLn ("FE error: " ++ e)
    Right fit -> do
      printf "FE  mx = %.5f\n" (brMx fit)
      printf "FE  B  = %.5f (se %.5f)\n" (brB fit) (brBse fit)
      mapM_ (\t -> printf "FE  d[%-11s] = %8.5f (se %.5f)\n" (nameOf t)
                      (brD fit Map.! t) (brDse fit Map.! t)) nonref
  putStrLn ""
  case nmaRegressionBaselineRiskBinRE st ref Nothing of
    Left e -> putStrLn ("RE error: " ++ e)
    Right fit -> do
      printf "RE  B   = %.5f (se %.5f)\n" (brB fit) (brBse fit)
      printf "RE  tau2= %.5f  (tau = %.5f)\n" (brTau2 fit) (sqrt (brTau2 fit))
      mapM_ (\t -> printf "RE  d[%-11s] = %8.5f (se %.5f)\n" (nameOf t)
                      (brD fit Map.! t) (brDse fit Map.! t)) nonref
