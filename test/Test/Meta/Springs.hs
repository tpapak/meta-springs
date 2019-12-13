module Test.Meta.Springs where

import           Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import qualified Data.Csv                      as C
import           Data.Either
import qualified Data.Map.Strict               as Map
import qualified Data.Vector                   as V
import           Data.Tuple

import           TestHS

import qualified Data.Meta.Effects             as Meta
import qualified Data.Meta.Pairwise.CommonEffect
                                               as FE
import           Data.Meta.Springs
import           Data.Numerics

ioTests :: [IO Test]
ioTests = [test1]

test1 :: IO Test
test1 = do
  let name        = "read continuous csv and create springs"
  let studiesFile = "test/continuous.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData                                                                                      
                :: Either String (C.Header, V.Vector Meta.PairwiseStudy)
  case estudies of 
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, pwstudies) -> do
      let studies = map Meta.pairwiseToStudy $ V.toList pwstudies
          Right mdstudies = sequence $ map (flip Meta.studyToIVStudy Meta.meanDifference) studies
          springs = map studyToSpring mdstudies
          summaryspring = commonEffectSpring mdstudies
      print $ show springs
      print $ show summaryspring
      return $ testPassed name "passed!"
