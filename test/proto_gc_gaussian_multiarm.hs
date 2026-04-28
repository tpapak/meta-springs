{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Run springGrandCanonicalAdaptive and springREML on Gaussian NMA
-- datasets that contain multi-arm trials.  Writes a JSON file with
-- the GC posterior mode and the EM-Newton REML estimate for each
-- dataset.  R script (proto_gc_gaussian_multiarm.R) reads the same
-- list and runs netmeta + rma.mv (+ multinma if available) for the
-- same datasets, so the τ² values can be compared side by side.
--
-- Run: stack runghc --package meta-analysis -- test/proto_gc_gaussian_multiarm.hs
module Main where

import qualified Data.Aeson              as A
import           Data.Aeson              ((.=), object)
import qualified Data.ByteString.Lazy    as BL
import           Data.Time.Clock         (getCurrentTime, diffUTCTime)
import           System.IO               (hFlush, stdout)
import           Text.Printf             (printf)

import Data.Meta.Effects       (meanArm)
import Data.Meta.Studies       (readStudies)
import Data.Meta.RandomEffects (springGrandCanonicalAdaptive, springREML)

-- (label, file) pairs.  Real datasets first, synth_c after.
datasetList :: [(String, FilePath)]
datasetList =
  [ ("senn2013",     "test/nma_senn2013.json")
  , ("parkinson",    "test/nma_parkinson.json")
  , ("franchini2012","test/nma_franchini2012.json")
  , ("stowe2010",    "test/nma_stowe2010.json")
  , ("synth_c1",     "test/nma_synth_c1.json")
  , ("synth_c2",     "test/nma_synth_c2.json")
  , ("synth_c3",     "test/nma_synth_c3.json")
  , ("synth_c4",     "test/nma_synth_c4.json")
  , ("synth_c5",     "test/nma_synth_c5.json")
  , ("synth_c6",     "test/nma_synth_c6.json")
  , ("synth_c7",     "test/nma_synth_c7.json")
  , ("synth_c8",     "test/nma_synth_c8.json")
  , ("synth_c9",     "test/nma_synth_c9.json")
  , ("synth_c10",    "test/nma_synth_c10.json")
  ]

runOne :: (String, FilePath) -> IO A.Value
runOne (name, path) = do
  printf "%-15s ... " name
  hFlush stdout
  est <- readStudies path
  case est of
    Left err -> do
      putStrLn ("read err: " ++ err)
      return $ object [ "name" .= name
                      , "file" .= path
                      , "error" .= err
                      ]
    Right studies -> do
      t0 <- getCurrentTime
      let !gc = springGrandCanonicalAdaptive studies meanArm 50
      t1 <- getCurrentTime
      let !rl = springREML studies meanArm Nothing (Just 1000) (Just 1e-7)
      t2 <- getCurrentTime
      let gcWall = realToFrac (diffUTCTime t1 t0) :: Double
          rlWall = realToFrac (diffUTCTime t2 t1) :: Double
          gcMode = case gc of
                     Right (m, _) -> Just m
                     Left _       -> Nothing
          remlT2 = case rl of
                     Right (_, (t, _)) -> Just t
                     Left _            -> Nothing
      let fmt :: Maybe Double -> String
          fmt Nothing  = "—"
          fmt (Just x) = printf "%.5f" x
      printf "GC=%s REML=%s (gc %.2fs, reml %.2fs)\n"
        (fmt gcMode) (fmt remlT2) gcWall rlWall
      return $ object
        [ "name"            .= name
        , "file"            .= path
        , "k_studies"       .= length studies
        , "gc_mode_tau2"    .= gcMode
        , "spring_reml_tau2".= remlT2
        , "gc_seconds"      .= gcWall
        , "reml_seconds"    .= rlWall
        ]

main :: IO ()
main = do
  putStrLn "Gaussian (MD) τ² — GC mode vs spring-REML on multi-arm NMAs"
  putStrLn (replicate 70 '-')
  rows <- mapM runOne datasetList
  let out = object [ "results" .= rows ]
  BL.writeFile "test/gc_compare/gaussian_multiarm_haskell.json" (A.encode out)
  putStrLn ""
  putStrLn "wrote test/gc_compare/gaussian_multiarm_haskell.json"
