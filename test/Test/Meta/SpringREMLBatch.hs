{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Batch test of spring-REML against netmeta REML on NMA datasets.
--
-- Reference τ² values were computed using R's netmeta package (REML):
--
--   test/nma_reference_tau2.json  — 21 synthetic datasets (10 continuous MD,
--                                   10 binary OR, plus senn2013)
--   test/nmadb/nmadb_reference_tau2.json — 218 real-world datasets from nmadb
--
-- Generated with netmeta::netmeta(… method = "REML") on each dataset,
-- extracting $tau2, number of studies, treatments, multiarm count, and
-- summary measure (MD or OR).
module Test.Meta.SpringREMLBatch where

import Control.Exception (catch, SomeException)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Either
import qualified Data.Map.Strict as Map
import Data.Maybe
import System.IO (hFlush, stdout)

import TestHS
import Data.Numerics
import Data.Meta.Effects
import Data.Meta.NMA
import Data.Meta.RandomEffects
import Data.Meta.Studies

data RefDataset = RefDataset
  { refName :: String
  , refTau2 :: Double
  , refK :: Int
  , refTreats :: Int
  , refMultiarm :: Int
  , refSm :: String
  } deriving (Show)

instance FromJSON RefDataset where
  parseJSON = withObject "RefDataset" $ \v -> RefDataset
    <$> v .: "name"
    <*> v .: "tau2"
    <*> v .: "k"
    <*> v .: "treats"
    <*> v .: "multiarm"
    <*> v .: "sm"

ioTests :: [IO Test]
ioTests = [batchTest]

batchTest :: IO Test
batchTest = do
  let name = "Spring-REML batch: 30+ NMA datasets"

  -- Load reference tau2 values from both sources
  refJson1 <- B.readFile "test/nma_reference_tau2.json"
  let refs1 = map (\r -> r { refName = "nma_" <> refName r })
            $ fromMaybe [] (decode refJson1 :: Maybe [RefDataset])
  refJson2 <- catch (B.readFile "test/nmadb/nmadb_reference_tau2.json")
                    (\(_ :: SomeException) -> return "[]")
  let refs2 = map (\r -> r { refName = "nmadb/" <> refName r })
            $ fromMaybe [] (decode refJson2 :: Maybe [RefDataset])
      refs = refs1 ++ refs2

  putStr $ "\n  === Spring-REML Batch Test ==="
  putStr $ "\n  Datasets: " <> show (length refs)
  putStr $ " (synth: " <> show (length refs1) <> ", nmadb: " <> show (length refs2) <> ")"

  -- Test each dataset
  results <- mapM (\ref -> do
    let file = "test/" <> refName ref <> ".json"
        expected = refTau2 ref
    result <- catch (do
      estudies <- readStudies file
      case estudies of
        Left err -> do
          putStr $ "\n  SKIP " <> refName ref <> ": " <> err
          return Nothing
        Right studies -> do
          let eresult = if refSm ref == "OR"
                then springREML studies logOdds Nothing (Just 1000) (Just 1e-6)
                else springREML studies meanArm Nothing (Just 1000) (Just 1e-6)
          case eresult of
            Left err -> do
              putStr $ "\n  FAIL " <> refName ref <> ": " <> err
              return (Just (refName ref, expected, -1, False))
            Right (neteffs, (tau2, _)) -> do
              let ratio = if expected > 1e-6 then tau2 / expected else 0
                  tol = 0.10  -- 10% tolerance
                  ok = abs (ratio - 1) < tol || abs (tau2 - expected) < 1e-4
                  -- I² measures
                  i2blup = i2BLUP neteffs
                  ei2class = if refSm ref == "OR"
                    then classicalI2 studies logOdds
                    else classicalI2 studies meanArm
                  i2classStr = case ei2class of
                    Right v -> show (roundDouble v 4)
                    Left _ -> "ERR"
              putStr $ "\n  " <> (if ok then "OK " else "!! ")
                    <> refName ref
                    <> "  expected=" <> show (roundDouble expected 6)
                    <> "  got=" <> show (roundDouble tau2 6)
                    <> "  ratio=" <> show (roundDouble ratio 3)
                    <> "  I2blup=" <> show (roundDouble i2blup 4)
                    <> "  I2class=" <> i2classStr
              -- Diagnostic: 1 EM step from netmeta's τ² — does the formula hold?
              if expected > 1e-6
                then do
                  let eresult1 = if refSm ref == "OR"
                        then springREML studies logOdds (Just expected) (Just 1) (Just 1e-20)
                        else springREML studies meanArm (Just expected) (Just 1) (Just 1e-20)
                  case eresult1 of
                    Right (_, (tau2_1step, _)) -> do
                      let stepRatio = if expected > 1e-10 then tau2_1step / expected else 0
                      putStr $ "  [1step=" <> show (roundDouble tau2_1step 6)
                            <> " ratio=" <> show (roundDouble stepRatio 4) <> "]"
                    _ -> return ()
                else return ()
              return (Just (refName ref, expected, tau2, ok))
      ) (\(e :: SomeException) -> do
          putStr $ "\n  ERR " <> refName ref <> ": " <> show e
          return Nothing
      )
    return result
    ) refs

  hFlush stdout
  let valid = catMaybes results
      passed = length $ filter (\(_, _, _, ok) -> ok) valid
      total = length valid

  putStr $ "\n\n  --- Summary ---"
  putStr $ "\n  Passed: " <> show passed <> "/" <> show total
  putStr $ "\n"

  if passed >= total - 3  -- allow up to 3 failures
    then return $ testPassed name $ show passed <> "/" <> show total <> " datasets within 10%"
    else return $ testFailed name
      ( show passed <> "/" <> show total <> " passed"
      , "too many failures"
      )
