{-# LANGUAGE OverloadedStrings #-}
-- Baseline-risk slope variants: shared / unrelated / exchangeable (Achana/bnma).
-- Usage: stack runghc -- test/nmr_baseline_risk_variants.hs <studies.json> <ref>
module Main where

import System.Environment (getArgs)
import Data.List (sort, nub)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..), tidOfArm)
import Data.Meta.Studies (StudyId (..), readStudies, getStudyArms)
import Data.Meta.NMA (nmaRegressionBaselineRiskBinB, BaselineSlopeModel (..), BaselineRiskFit (..))

nm :: TreatmentId -> String
nm (TreatmentId (StringId s)) = s
nm (TreatmentId (IntId i)) = show i

main :: IO ()
main = do
  args <- getArgs
  let (path, refName) = case args of (p:r:_) -> (p,r); [p] -> (p,"A"); _ -> ("test/nmr_br_sim.json","A")
      ref = TreatmentId (StringId refName)
  Right st <- readStudies path
  let nonref = filter (/= ref) $ sort $ nub $ concatMap (map tidOfArm . getStudyArms) st
  printf "=== baseline-risk slope variants: %s  ref=%s ===\n\n" path refName
  let run :: String -> BaselineSlopeModel -> IO ()
      run lbl model = case nmaRegressionBaselineRiskBinB st ref model Nothing of
        Left e -> printf "%s: ERROR %s\n" (lbl::String) e
        Right f -> do
          printf "%-12s  B(main) = %.5f (se %.5f)%s\n" lbl (brB f) (brBse f)
                 (maybe "" (\s -> printf "   sigmaB2 = %.5f" s :: String) (brSigmaB2 f))
          mapM_ (\t -> printf "    b[%-11s] = %+.5f (se %.5f)\n" (nm t)
                         (brBt f Map.! t) (brBtSe f Map.! t)) nonref
          printf "\n"
  run "shared" BLShared
  run "unrelated" BLUnrelated
  run "exchangeable" BLExchangeable
