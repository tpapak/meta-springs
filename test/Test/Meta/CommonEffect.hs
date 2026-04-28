module Test.Meta.CommonEffect where

import qualified Data.Map.Strict as Map

import TestHS

import Data.Numerics
import Data.Meta.Effects
import Data.Meta.Studies
import Data.Meta.NMA

ioTests :: [IO Test]
ioTests = [ pairwiseMD ]

pairwiseMD :: IO Test
pairwiseMD = do
  let name = "Common Effect pairwise meta-analysis on continuous Mean difference"
  let studiesFile = "test/continuous.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error reading studies", err)
    Right studies -> do
      let Right mdstudies = sequence $ map (flip studyToIVStudy meanDifference) studies
          Right studiesgraph = studiesGraph' mdstudies
          directs = directEffects' studiesgraph
          (MD e v) = head $ Map.elems directs
          foundce = mapEstimate (flip roundDouble 4) (MD e v)
          expected = MD 8.7666 (roundDouble (1.2739 ^ 2) 4)
      if foundce == expected
        then return $ testPassed name "ok"
        else return $ testFailed name (show expected, show foundce)
