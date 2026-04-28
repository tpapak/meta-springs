{-# LANGUAGE OverloadedStrings #-}
-- Verify GC posterior mode (flat-in-τ² prior) matches REML mode to
-- grid resolution across a range of binomial NMA datasets.
module Main where

import           Data.Time.Clock      (getCurrentTime, diffUTCTime)
import           System.IO            (hFlush, stdout)

import Data.Meta.Studies       (readStudies)
import Data.Meta.RandomEffects (springGrandCanonicalBinAdaptive, springREMLBin)

datasets :: [(String, FilePath)]
datasets =
  [ ("test/binary.json",             "test/binary.json")
  , ("synth_015",                    "test/synth_bin/synth_015.json")
  , ("synth_025",                    "test/synth_bin/synth_025.json")
  , ("synth_041",                    "test/synth_bin/synth_041.json")
  , ("synth_090",                    "test/synth_bin/synth_090.json")
  , ("nmadb_473269",                 "test/nmadb/nmadb_473269.json")
  , ("nmadb_479650",                 "test/nmadb/nmadb_479650.json")
  , ("nmadb_474842",                 "test/nmadb/nmadb_474842.json")
  , ("nmadb_501194",                 "test/nmadb/nmadb_501194.json")
  ]

showR :: Int -> Double -> String
showR n x =
  let factor = 10 ^^ n :: Double
      r      = fromIntegral (round (x * factor) :: Integer) / factor
  in  show r

main :: IO ()
main = do
  putStrLn "Mode verification: REML τ² vs GC posterior mode (flat prior)"
  putStrLn ""
  putStrLn $ replicate 84 '-'
  putStrLn $ "dataset          k    REML τ²       GC mode       |Δ|        rel err"
  putStrLn $ replicate 84 '-'
  mapM_ runOne datasets
  where
    runOne (name, path) = do
      est <- readStudies path
      case est of
        Left err      -> putStrLn $ name ++ " read err: " ++ err
        Right studies -> do
          t0 <- getCurrentTime
          let remlRes = springREMLBin studies Nothing (Just 500) (Just 1e-6)
              gcRes   = springGrandCanonicalBinAdaptive studies 200
          case (remlRes, gcRes) of
            (Right (_, (tauR, _)), Right (modeGC, _)) -> do
              t1 <- getCurrentTime
              let wall = realToFrac (diffUTCTime t1 t0) :: Double
                  absD = abs (modeGC - tauR)
                  relD = if tauR > 1e-8
                           then 100 * absD / tauR
                           else 0
              putStrLn $ pad 15 name ++ "  "
                      ++ pad 4 (show (length studies))
                      ++ "  " ++ pad 12 (showR 6 tauR)
                      ++ "  " ++ pad 12 (showR 6 modeGC)
                      ++ "  " ++ pad 10 (showR 6 absD)
                      ++ "  " ++ showR 2 relD ++ "% "
                      ++ "(" ++ showR 1 wall ++ "s)"
              hFlush stdout
            _ -> putStrLn $ name ++ ": err"

    pad :: Int -> String -> String
    pad n s = take n (s ++ replicate n ' ')
