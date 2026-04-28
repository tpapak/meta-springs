{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Run springREMLBin on the controlled synthetic bias datasets.
-- Reads manifest.json, runs each dataset, outputs JSON results.
--
-- Build:  stack ghc -- test/synth_bias/RunSpring.hs -o run_spring
-- Run:    ./run_spring
module Main where

import Control.Exception (catch, SomeException, evaluate)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.IORef
import Data.List (foldl')
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import System.IO
import Text.Printf

import Data.Meta.Effects
import Data.Meta.NMA
import Data.Meta.RandomEffects
import Data.Meta.Studies

data SynthEntry = SynthEntry
  { seName   :: String
  , seTau2   :: Double
  , seMu0    :: Double
  , seTreats :: Int
  } deriving (Show)

instance FromJSON SynthEntry where
  parseJSON = withObject "SynthEntry" $ \v -> SynthEntry
    <$> v .: "name"
    <*> v .: "tau2"
    <*> v .: "mu0"
    <*> v .: "treats"

data SpringResult = SpringResult
  { srName    :: String
  , srTau2    :: Double
  , srEffects :: [(Int, Double, Double)]  -- (trt id, estimate d_j-d_1, variance)
  , srIters   :: Int
  } deriving (Show)

instance ToJSON SpringResult where
  toJSON r = object
    [ "name"    .= srName r
    , "tau2"    .= srTau2 r
    , "effects" .= map (\(i, e, v) -> object ["tid" .= i, "est" .= e, "var" .= v])
                       (srEffects r)
    , "iters"   .= srIters r
    ]

allZero :: Study -> Bool
allZero (BinaryStudy _ arms) = all (\a -> case a of
  BinaryArm _ e _ -> e == 0
  _ -> False) arms
allZero _ = False

runOne :: Bool -> SynthEntry -> IO (Maybe SpringResult)
runOne corrected entry = catch (do
  let file = "test/synth_bias/data/" <> seName entry <> ".json"
  estudies <- readStudies file
  case estudies of
    Left err -> do
      printf "  %-36s ERROR: %s\n" (seName entry) err
      return Nothing
    Right studies -> do
      let studies' = filter (not . allZero) studies
      case springREMLBinOpt corrected studies' Nothing Nothing Nothing of
        Left err -> do
          printf "  %-36s FAIL: %s\n" (seName entry) err
          return Nothing
        Right (neteffs, (tau2Est, path)) -> do
          -- Effects: d_j - d_1 for all treatments j (relative to treatment 1)
          -- Actual convention (verified by BinomialSpringTest pairwise CE):
          --   ests[row][col] = θ_col - θ_row  (netmeta sign)
          -- So d_j - d_1 = θ_j - θ_1 = ests[1][j]
          let t1 = TreatmentId (IntId 1)
              ests = networkEstimates neteffs
              vars = networkVariances neteffs
              row1Ests = Map.findWithDefault Map.empty t1 ests
              row1Vars = Map.findWithDefault Map.empty t1 vars
              allTids = Map.keys row1Ests
              effects = [(extractTid tid,
                          Map.findWithDefault 0 tid row1Ests,
                          Map.findWithDefault 0 tid row1Vars)
                        | tid <- allTids]
              extractTid (TreatmentId (IntId i)) = i
              extractTid _ = 0
          return $ Just SpringResult
            { srName    = seName entry
            , srTau2    = tau2Est
            , srEffects = effects
            , srIters   = length path
            }
  ) (\(e :: SomeException) -> do
    printf "  %-36s EXCEPTION\n" (seName entry)
    return Nothing
  )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let corrected = "--corrected" `elem` args
      outFile = if corrected
                  then "test/synth_bias/spring_results_corrected.json"
                  else "test/synth_bias/spring_results.json"
  mfJson <- B.readFile "test/synth_bias/manifest.json"
  let entries = fromMaybe [] (decode mfJson :: Maybe [SynthEntry])

  printf "Running springREMLBin on %d datasets (correction=%s)...\n"
    (length entries) (show corrected)
  printf "%-36s %8s %8s %5s\n"
    ("Name" :: String) ("tau2_true" :: String) ("tau2_est" :: String)
    ("iters" :: String)
  putStrLn $ replicate 64 '-'

  acc <- newIORef ([] :: [SpringResult])
  let total = length entries
      writeOut = do
        xs <- readIORef acc
        B.writeFile outFile (encode (reverse xs))
  flip mapM_ (zip [1 :: Int ..] entries) $ \(i, entry) -> do
    mr <- runOne corrected entry
    case mr of
      Nothing -> return ()
      Just r -> do
        printf "[%4d/%4d] %-36s true=%.4f est=%.4f iters=%d\n"
          i total (srName r) (seTau2 entry) (srTau2 r) (srIters r)
        modifyIORef' acc (r :)
        -- Persist incrementally every 25 datasets
        when' (i `mod` 25 == 0) writeOut
  writeOut
  valid <- readIORef acc
  printf "\n%d/%d succeeded. Results in %s\n"
    (length valid) (length entries) outFile
  where
    when' c a = if c then a else return ()
