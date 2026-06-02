{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..), meanArm)
import Data.Meta.Studies (StudyId (..), readStudies)
import Data.Meta.NMA (nmaRegression)
main = do
  Just cr <- A.decode <$> BL.readFile "/tmp/t20nmr/cov.json" :: IO (Maybe (Map.Map String Double))
  let cov = Map.fromList [ (StudyId (StringId k), v) | (k,v)<-Map.toList cr ]
  Right st <- readStudies "/tmp/t20nmr/data.json"
  t0 <- getCurrentTime
  let r = nmaRegression st cov meanArm
      b = either (const (0/0)) fst r
  b `seq` return ()
  t1 <- getCurrentTime
  printf "haskell nmaRegression FE: beta=%.5f  time=%.3f ms\n" b (1000*realToFrac (diffUTCTime t1 t0)::Double)
