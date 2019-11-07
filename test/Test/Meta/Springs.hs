module Test.Meta.Springs where

import           Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy             as B
import qualified Data.Map.Strict                  as Map
import qualified Data.Vector as V
import qualified Data.Csv             as C
import Data.Either

import           TestHS

import Data.Numerics
import qualified Data.Meta.Effects as Meta
import           Data.Meta.Springs

ioTests :: [IO Test]
ioTests = [ test1
          ]

test1 :: IO Test
test1 = do
  let name = "read continuous study"
  let studiesFile = "test/continuous.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeWith C.defaultDecodeOptions C.HasHeader csvData
               :: Either String (V.Vector Meta.PairwiseStudy)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right studies -> do
      {-putStrLn "Studies file"-}
      {-putStrLn $ show studies-}
      return $ testPassed name $ "passed!"

