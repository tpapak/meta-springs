{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Run springREMLBin only on datasets with treats >= 50
module Main where

import Control.Exception (catch, SomeException)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map.Strict as Map
import Text.Printf

import Data.Meta.Effects
import Data.Meta.NMA
import Data.Meta.RandomEffects
import Data.Meta.Studies

data SynthEntry = SynthEntry
  { seName :: String, seTau2 :: Double, seMu0 :: Double, seTreats :: Int
  } deriving (Show)

instance FromJSON SynthEntry where
  parseJSON = withObject "SynthEntry" $ \v -> SynthEntry
    <$> v .: "name" <*> v .: "tau2" <*> v .: "mu0" <*> v .: "treats"

data SpringResult = SpringResult
  { srName :: String, srTau2 :: Double
  , srEffects :: [(Int, Double, Double)], srIters :: Int
  }

instance ToJSON SpringResult where
  toJSON r = object
    [ "name" .= srName r, "tau2" .= srTau2 r
    , "effects" .= map (\(i,e,v) -> object ["tid".=i,"est".=e,"var".=v]) (srEffects r)
    , "iters" .= srIters r ]

allZero (BinaryStudy _ arms) = all (\a -> case a of
  BinaryArm _ e _ -> e == 0; _ -> False) arms
allZero _ = False

main :: IO ()
main = do
  mfJson <- B.readFile "test/synth_bias/manifest.json"
  let entries = filter (\e -> seTreats e >= 50)
              $ fromMaybe [] (decode mfJson :: Maybe [SynthEntry])
  printf "Running springREMLBin on %d big datasets\n" (length entries)

  results <- mapM (\entry -> catch (do
    let file = "test/synth_bias/data/" <> seName entry <> ".json"
    printf "  %s ... " (seName entry)
    estudies <- readStudies file
    case estudies of
      Left err -> do printf "ERR %s\n" err; return Nothing
      Right studies -> do
        let studies' = filter (not . allZero) studies
        case springREMLBin studies' Nothing Nothing Nothing of
          Left err -> do printf "FAIL %s\n" err; return Nothing
          Right (neteffs, (tau2Est, path)) -> do
            let t1 = TreatmentId (IntId 1)
                ests = networkEstimates neteffs
                vars = networkVariances neteffs
                row1E = Map.findWithDefault Map.empty t1 ests
                row1V = Map.findWithDefault Map.empty t1 vars
                extract (TreatmentId (IntId i)) = i; extract _ = 0
                effects = [(extract tid,
                            Map.findWithDefault 0 tid row1E,
                            Map.findWithDefault 0 tid row1V)
                          | tid <- Map.keys row1E]
            printf "tau2=%.4f iters=%d\n" tau2Est (length path)
            return $ Just $ SpringResult (seName entry) tau2Est effects (length path)
    ) (\(e :: SomeException) -> do printf "EXC\n"; return Nothing)
    ) entries

  let valid = catMaybes results
  B.writeFile "test/synth_bias/spring_big_results.json" (encode valid)
  printf "%d/%d\n" (length valid) (length entries)
