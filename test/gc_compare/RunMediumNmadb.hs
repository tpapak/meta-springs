{-# LANGUAGE OverloadedStrings #-}
-- Diagnose scaling: adaptive 100-point GC on nmadb_474842 (45 studies × 8 trts).
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import           Data.Time.Clock      (getCurrentTime, diffUTCTime)
import           System.IO            (hFlush, stdout)

import Data.Meta.Studies       (readStudies)
import Data.Meta.RandomEffects (springGrandCanonicalBinAdaptive)

main :: IO ()
main = do
  let path = "test/nmadb/nmadb_474842.json"
  est <- readStudies path
  case est of
    Left err      -> putStrLn $ "ERR: " ++ err
    Right studies -> do
      putStrLn $ show (length studies) ++ " studies from " ++ path
      hFlush stdout
      t0 <- getCurrentTime
      let res = springGrandCanonicalBinAdaptive studies 100
          forced = case res of
            Left _               -> 0 :: Double
            Right (m, g)         -> m + sum [lz | (_, lz, _) <- g]
      forced `seq` return ()
      t1 <- getCurrentTime
      let dt = realToFrac (diffUTCTime t1 t0) :: Double
      putStrLn $ "GC-adp on " ++ show (length studies)
                 ++ " studies: " ++ show dt ++ " s"
      case res of
        Left err -> putStrLn $ "err: " ++ err
        Right (m, g) -> do
          putStrLn $ "mode τ² = " ++ show m
          putStrLn $ "grid size = " ++ show (length g)
