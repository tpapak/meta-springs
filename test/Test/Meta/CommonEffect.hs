module Test.Meta.CommonEffect where

import           Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import qualified Data.Map.Strict               as Map
import qualified Data.Vector                   as V
import qualified Data.Csv                      as C
import           Data.Either

import           TestHS

import           Data.Numerics
import           Data.Meta.Effects
import           Data.Meta.Studies
import           Data.Meta.Pairwise.CommonEffect
import           Data.Meta.NMA

{-fastTests :: [Test]-}
{-fastTests = [ -}
              {-test1-}
            {-]-}

ioTests :: [IO Test]
ioTests = [ md
          , testLogOR
          , consistentPairwise
          , springsmd
          , continuousNoMultiarm
  --, smd
  --, rr
  --, testrd
          ]

consistentPairwise :: IO Test
consistentPairwise = do
  let name = "Consistent pairwise SPRINGS"
  let studiesFile = "test/conspair.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      let enetes = springNMA studies (Just 4) Nothing meanArm
       in case enetes of
            Left err -> return $ testFailed name $ ("no nets \n", show studies <> "\n" <> show enetes)
            Right netes -> do
              let outcome = ((networkEstimates netes) Map.!
                              (TreatmentId (StringId "A")) Map.!
                                    (TreatmentId (StringId "B"))) 
              let founde  = roundDouble outcome 8
                  expected = 4
               in if expected == founde
                    then return $ testPassed name $ show founde <> "passed!"
                    else do putStr $ debugmsg netes 
                            return $ testFailed name $ (show expected
                              --, show founde )
                                   , show founde <> show netes)

springsmd :: IO Test
springsmd = do
  let name = "Common Effect pairwise meta-analysis Mean difference with SPRINGS"
  let studiesFile = "test/continuous.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      let enetes = springNMA studies (Just 7) Nothing meanArm
       in case enetes of
            Left err -> return $ testFailed name $ ("no nets \n", show studies <> "\n" <> show enetes)
            Right netes -> do
              let eff = ((networkEstimates netes) Map.!
                             (TreatmentId (StringId "B")) Map.!
                             (TreatmentId (StringId "A"))) 
                  var = ((networkVariances netes) Map.!
                             (TreatmentId (StringId "A")) Map.!
                             (TreatmentId (StringId "B"))) 
                  founde  = (roundDouble eff 4, roundDouble var 4)
                  expected = (8.7666, (roundDouble (1.2739 ^ 2) 4))
               in if expected == founde
                    then return $ testPassed name $ show founde <> "passed!"
                    else do 
                      putStr $ debugmsg netes
                      return $ testFailed name $ (show expected, show founde)

md :: IO Test
md = do
  let name = "Common Effect pairwise meta-analysis on continuous Mean difference"
  let studiesFile = "test/continuous.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      let Right mdstudies =
            sequence $ map (flip studyToIVStudy meanDifference) studies
          Right studiesgraph = studiesGraph' mdstudies
          directs = directEffects' studiesgraph
          (MD e v) = head $ Map.elems directs
          foundce  = mapEstimate (flip roundDouble 4) (MD e v)
          expected = MD 8.7666 (roundDouble (1.2739 ^ 2) 4)
      if foundce == expected
        then return $ testPassed name $ "nice!!"
        else return $ testFailed name $ (show expected, show mdstudies <> show studies)

testLogOR :: IO Test
testLogOR = do
  let name = "Common Effect pairwise meta-analysis on Odds ratios"
  let studiesFile = "test/binary.json"
  estudies <- readStudies studiesFile
  case estudies of
    Right studies -> do
      let Right orstudies =
            sequence $ map (flip studyToIVStudy logOddsRatio) studies
          Right studiesgraph = studiesGraph' orstudies
          directs = directEffects' studiesgraph
          (LogOR e v) = head $ Map.elems directs
          foundce  = mapEstimate (flip roundDouble 4) (LogOR e v)
          expected = LogOR (-0.7241) (roundDouble (0.1538501 ^ 2) 4)
      if foundce == expected
        then return $ testPassed name $ "nice!!"
        else return $ testFailed name $ (show expected, show foundce)

--smd :: IO Test
--smd = do
  --let name        = "Common Effect meta-analysis on continuous SMD"
  --let studiesFile = "test/continuous.csv"
  --csvData <- B.readFile studiesFile
  --let estudies =
        --C.decodeByName csvData :: Either
            --String
            --(C.Header, V.Vector CSVStudy)
  --case estudies of
    --Left  err          -> return $ testFailed name ("error parsing csv", err)
    --Right (_, studies) -> do
      --let emds =
            --rights
              -- $ fmap (standardizedMeanDifference . pairwiseStudyToComparison)
              -- $ V.toList studies
      --let ce        = commonEffect emds
      --let (SMD e v) = ce
      --let foundce = (mapEstimate (\c -> roundDouble c 4) (SMD e v))
      --let expected  = SMD 0.4143 0.0041
      --if foundce == expected
        --then return $ testPassed name $ "nice!!"
        --else return $ testFailed name $ (show expected, show foundce)

--rr :: IO Test
--rr = do
  --let name        = "Common Effect meta-analysis on binary RR"
  --let studiesFile = "test/binary.csv"
  --csvData <- B.readFile studiesFile
  --let estudies =
        --C.decodeByName csvData :: Either
            --String
            --(C.Header, V.Vector CSVStudy)
  --case estudies of
    --Left  err          -> return $ testFailed name $ ("error parsing csv", err)
    --Right (_, studies) -> do
      --let
        --emds = rights $ fmap (riskRatio . pairwiseStudyToComparison) $ V.toList
          --studies
      --let ce       = commonEffect emds
      --let foundce  = (mapEstimate (\c -> roundDouble c 4) ce)
      --let expected = RR 0.5775 (CI 0.4511 0.7392)
      --if foundce == expected
        --then return $ testPassed name $ "nice!!"
        --else return $ testFailed name $ (show expected, show foundce)

--testrd :: IO Test
--testrd = do
  --let name        = "Common Effect meta-analysis on binary RD"
  --let studiesFile = "test/binary.csv"
  --csvData <- B.readFile studiesFile
  --let estudies =
        --C.decodeByName csvData :: Either
            --String
            --(C.Header, V.Vector CSVStudy)
  --case estudies of
    --Left  err          -> return $ testFailed name $ ("error parsing csv", err)
    --Right (_, studies) -> do
      --let
        --emds =
          --rights $ fmap (riskDifference . pairwiseStudyToComparison) $ V.toList
            --studies
      --let ce      = commonEffect emds
      --let foundce = (mapEstimate (\c -> roundDouble c 4) ce)
      --let expected = RD (-0.1119)
            -- $ roundDouble (ciToVariance (CI (-0.1499) (-0.0739))) 4
      --if foundce == expected
        --then return $ testPassed name $ "nice!!"
        --else return $ testFailed name $ (show expected, show foundce)
        --
continuousNoMultiarm :: IO Test
continuousNoMultiarm = do
  let name = "NMA continuous NoMultiarms"
  let studiesFile = "test/contNoMultiarm.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      --let enetes = springNMA studies Nothing (Just 0.000234374) meanArm
      let enetes = springNMA studies Nothing Nothing meanArm
        in case enetes of
             Left err -> return $ testFailed name 
               ("problem with reading studies 1\n", show err)
             Right netes -> 
               let effs  = map (`roundDouble` 5)
                    $ Map.elems $ networkEstimates netes Map.! (TreatmentId (IntId 1))
                   vars  = map (\x-> roundDouble x 5)
                    $ Map.elems $ (networkVariances netes) Map.! (TreatmentId (IntId 1))
                   founde = [zip effs vars]
                   expeffs = map ((flip roundDouble) 5) [0.000000000,-0.022293198,-0.014363957,-0.032296552,-0.006013469,-0.005280683]
                   expvars = map ((flip roundDouble) 5) [0.000000e+00,4.888726e-05,6.719252e-05,8.624463e-05,4.351166e-05,1.079675e-05 ]
                   expected = [zip expeffs expvars]
                in if expected == founde
                      then return $ testPassed name $ show founde <> "passed!"
                      else return $ testFailed name $ (show expected, show founde)
