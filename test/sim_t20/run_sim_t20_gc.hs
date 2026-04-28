{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Run springGrandCanonicalAdaptive + springREML on every replica in
-- test/sim_t20/data/, collect τ² estimates and wall times into one
-- combined JSON.
--
-- Run:  stack runghc --package meta-analysis -- test/sim_t20/run_sim_t20_gc.hs

module Main where

import qualified Data.Aeson              as A
import           Data.Aeson              ((.=), (.:), object)
import qualified Data.Aeson.Types        as AT
import qualified Data.ByteString.Lazy    as BL
import           Data.Maybe              (mapMaybe)
import           Data.Time.Clock         (getCurrentTime, diffUTCTime)
import           Text.Printf             (printf)
import           System.IO               (hFlush, stdout)

import Data.Meta.Effects       (meanArm)
import Data.Meta.Studies       (readStudies)
import Data.Meta.RandomEffects (springGrandCanonicalAdaptive, springREML)

data Manifest = Manifest
  { mTau2Idx    :: !Int
  , mTau2       :: !Double
  , mRep        :: !Int
  , mFile       :: !FilePath
  , mKStudies   :: !Int
  , mMultiArmK  :: !Int
  } deriving Show

instance A.FromJSON Manifest where
  parseJSON = A.withObject "Manifest" $ \v -> Manifest
    <$> v .: "tau2_idx"
    <*> v .: "tau2"
    <*> v .: "rep"
    <*> v .: "file"
    <*> v .: "k_studies"
    <*> v .: "multi_arm_k"

runOne :: Manifest -> IO A.Value
runOne m = do
  est <- readStudies (mFile m)
  case est of
    Left err -> return $ object
      [ "tau2_idx"    .= mTau2Idx m
      , "tau2_true"   .= mTau2 m
      , "rep"         .= mRep m
      , "file"        .= mFile m
      , "error"       .= err
      ]
    Right studies -> do
      t0 <- getCurrentTime
      let !gc = springGrandCanonicalAdaptive studies meanArm 50
      t1 <- getCurrentTime
      let !rl = springREML studies meanArm Nothing (Just 1000) (Just 1e-7)
      t2 <- getCurrentTime
      let gcWall = realToFrac (diffUTCTime t1 t0) :: Double
          rlWall = realToFrac (diffUTCTime t2 t1) :: Double
          gcMode = case gc of Right (m', _) -> Just m'; Left _ -> Nothing
          remlT2 = case rl of Right (_, (t, _)) -> Just t; Left _ -> Nothing
      return $ object
        [ "tau2_idx"        .= mTau2Idx m
        , "tau2_true"       .= mTau2 m
        , "rep"             .= mRep m
        , "k_studies"       .= mKStudies m
        , "multi_arm_k"     .= mMultiArmK m
        , "gc_mode_tau2"    .= gcMode
        , "spring_reml_tau2".= remlT2
        , "gc_seconds"      .= gcWall
        , "reml_seconds"    .= rlWall
        ]

main :: IO ()
main = do
  raw <- BL.readFile "test/sim_t20/data/manifest.json"
  let mans = case A.eitherDecode raw of
                Right xs -> xs :: [Manifest]
                Left err -> error ("manifest decode: " ++ err)
  printf "Running GC + spring-REML on %d replicas...\n" (length mans)
  hFlush stdout
  t0 <- getCurrentTime
  results <- mapM (\(i, m) -> do
      r <- runOne m
      if i `mod` (25 :: Int) == 0
        then printf "  ... %d / %d\n" i (length mans) >> hFlush stdout
        else return ()
      return r
    ) (zip [1 :: Int ..] mans)
  t1 <- getCurrentTime
  let wall = realToFrac (diffUTCTime t1 t0) :: Double
  printf "done in %.1fs\n" wall
  BL.writeFile "test/sim_t20/results/gc_sreml.json"
    (A.encode $ object [ "results" .= results, "wall_seconds" .= wall ])
  putStrLn "wrote test/sim_t20/results/gc_sreml.json"
