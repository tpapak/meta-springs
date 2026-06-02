{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..))
import Data.Meta.Studies (StudyId (..), readStudies)
import Data.Meta.NMA (nmaRegressionBin)
t s = TreatmentId (StringId s)
main = do
  Just cr <- A.decode <$> BL.readFile "/tmp/binnmr/cov.json" :: IO (Maybe (Map.Map String Double))
  let cov = Map.fromList [ (StudyId (StringId k), v) | (k,v)<-Map.toList cr ]
  Right st <- readStudies "/tmp/binnmr/data.json"
  case nmaRegressionBin st cov of
    Left e -> putStrLn e
    Right (beta, cs) -> printf "nmaRegressionBin: beta=%.4f  A->B=%.4f  A->C=%.4f\n"
                          beta (cs Map.! t "A" Map.! t "B") (cs Map.! t "A" Map.! t "C")
