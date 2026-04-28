{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Run springREMLBin on the 20 nmadb binary datasets.
-- Build:  stack ghc -- test/synth_bias/RunSpringNmadb.hs -o run_spring_nmadb
-- Run:    ./run_spring_nmadb
module Main where

import Control.Exception (catch, SomeException, evaluate)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import Text.Printf

import Data.Meta.Effects
import Data.Meta.NMA
import Data.Meta.RandomEffects
import Data.Meta.Studies

datasets :: [String]
datasets =
  [ "nmadb_501212", "nmadb_482472", "nmadb_481147", "nmadb_479806"
  , "nmadb_482521", "nmadb_476033"
  , "nmadb_501226", "nmadb_501412", "nmadb_480430", "nmadb_481731"
  , "nmadb_501311", "nmadb_501263"
  , "nmadb_501324", "nmadb_482440", "nmadb_501367", "nmadb_481216"
  , "nmadb_479574", "nmadb_501340"
  , "nmadb_473269", "nmadb_473552"
  ]

data NmadbResult = NmadbResult
  { nrName :: String
  , nrTau2 :: Double
  } deriving (Show)

instance ToJSON NmadbResult where
  toJSON r = object ["name" .= nrName r, "tau2" .= nrTau2 r]

allZero :: Study -> Bool
allZero (BinaryStudy _ arms) = all (\a -> case a of
  BinaryArm _ e _ -> e == 0; _ -> False) arms
allZero _ = False

main :: IO ()
main = do
  printf "%-20s %10s\n" ("Dataset" :: String) ("sp_tau2" :: String)
  putStrLn $ replicate 32 '-'

  results <- mapM (\ds -> catch (do
    let file = "test/nmadb/" <> ds <> ".json"
    estudies <- readStudies file
    case estudies of
      Left err -> do
        printf "%-20s ERROR: %s\n" ds err
        return Nothing
      Right studies -> do
        let studies' = filter (not . allZero) studies
        case springREMLBin studies' Nothing Nothing Nothing of
          Left err -> do
            printf "%-20s FAIL: %s\n" ds err
            return Nothing
          Right (_, (tau2Est, _)) -> do
            printf "%-20s %10.4f\n" ds tau2Est
            return $ Just $ NmadbResult ds tau2Est
    ) (\(e :: SomeException) -> do
      printf "%-20s EXCEPTION\n" ds
      return Nothing
    )) datasets

  let valid = catMaybes results
  B.writeFile "test/synth_bias/nmadb_spring_results.json" (encode valid)
  printf "\n%d/%d succeeded.\n" (length valid) (length datasets)
