{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..), meanArm)
import Data.Meta.Studies (StudyId (..), readStudies)
import Data.Meta.NMA (nmaRegressionGroupedREML)
showT (TreatmentId (StringId s)) = s; showT (TreatmentId (IntId i)) = show i
main = do
  Just cr <- A.decode <$> BL.readFile "/tmp/nmr5/cov.json" :: IO (Maybe (Map.Map String Double))
  let cov = Map.fromList [ (StudyId (StringId k), v) | (k,v)<-Map.toList cr ]
  Right st <- readStudies "/tmp/nmr5/data.json"
  case nmaRegressionGroupedREML st cov meanArm of
    Left e -> putStrLn e
    Right (t2s, b, bm, _) -> do
      printf "tau-group 0 (tau2 heterogeneity) = %.4f\n" (t2s Map.! 0)
      printf "tau-group 1 (sigmaB2 slope disp) = %.4f\n" (t2s Map.! 1)
      printf "common slope B = %.4f\n" b
      mapM_ (\(t,v) -> printf "  beta[%s] = %.4f\n" (showT t) v) (Map.toList bm)
