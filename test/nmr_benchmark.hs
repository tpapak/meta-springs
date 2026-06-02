{-# LANGUAGE OverloadedStrings #-}
-- Benchmark + verify: time nmaRegressionRE over all reps, dump estimates.
module Main where
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Data.Either (rights)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..), meanArm)
import Data.Meta.Studies (StudyId (..), readStudies)
import Data.Meta.NMA (nmaRegressionRE)
main :: IO ()
main = do
  Just cr <- A.decode <$> BL.readFile "/tmp/nmrgc/cov.json" :: IO (Maybe (Map.Map String Double))
  let cov = Map.fromList [ (StudyId (StringId k), v) | (k,v)<-Map.toList cr ]
  reps <- mapM (\r -> readStudies (printf "/tmp/nmrgc/rep_%03d.json" (r::Int))) [1..200]
  let studiesL = rights reps
  printf "loaded %d reps\n" (length studiesL)
  t0 <- getCurrentTime
  let fits = [ nmaRegressionRE st cov (Just 0.1) meanArm | st <- studiesL ]
      acc  = sum [ b + t2 | Right (b,t2,_) <- fits ]   -- force every fit
  acc `seq` return ()
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double; n = length studiesL
  printf "springs nmaRegressionRE: %d fits in %.4f s = %.3f ms/fit\n" n secs (1000*secs/fromIntegral n)
  -- dump estimates for verification
  writeFile "/tmp/nmrgc/spring_bench.csv" $ unlines $ "rep,beta,tau2" :
    [ printf "%d,%.5f,%.5f" i b t2 | (i, Right (b,t2,_)) <- zip [1::Int ..] fits ]
