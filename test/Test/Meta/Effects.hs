module Test.Meta.Effects where

import           Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import qualified Data.Map.Strict               as Map
import qualified Data.Vector                   as V
import           Data.Either

import           TestHS

import           Data.Numerics
import Data.Graph.AdjacencyList
import           Data.Meta.Effects
import           Data.Meta.Studies

{-fastTests :: [Test]-}
{-fastTests = [ -}
              {-test1-}
            {-]-}

ioTests :: [IO Test]
ioTests = [ ivstudies
          , testSMD
  , testLogOR
  --, testRR
  , testOR
  --, testRD
  --, reverseOR
  --, reverseRR
  --, reverseRD
  ]

ivstudies :: IO Test
ivstudies = do
  let name        = "turn csv to IV MDs and their graph"
  let studiesFile = "test/continuous.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      let Right mdstudies =
            sequence $ map (flip studyToIVStudy meanDifference) studies
          estudiesgraph = studiesGraph' mdstudies
       in case estudiesgraph of
            Left err ->
              return
              $ testFailed name
              $ (err, "studiegraph error")
            Right studiesgraph -> do
              let effects = map getEffectsOfIVStudy mdstudies
              return $ testPassed name $ "passed!" 

testSMD :: IO Test
testSMD = do
  let name        = "standardized mean differences"
  let studiesFile = "test/continuous.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      let Right smds = sequence 
                     $ map (flip studyToIVStudy standardizedMeanDifference) studies
          gsvgs = map ((\(IVStudy s cts) -> 
                              let ctsl = contrastsToList cts
                                  Contrast ta tb e = head ctsl
                               in ((roundDouble (expectation e) 3), 
                                   (roundDouble (variance e) 3)))) smds
          gs = map fst gsvgs
          vgs = map snd gsvgs
          correctgs  = [0.095, 0.664, 0.277, 0.367, 0.462, 0.185]
          correctvgs = [0.033, 0.011, 0.031, 0.050, 0.043, 0.023]
      --print smds
      if gs == correctgs && vgs == correctvgs
        then return $ testPassed name $ "passed!"
        else
          return
            $ testFailed name
            $ ( show correctgs <> show correctvgs <> "\n"
            , (show gs <> show vgs <> "\n")
            )

testOR :: IO Test
testOR = do
  let name        = "Odds Ratio"
  let studiesFile = "test/binary.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      let Right ors = sequence $ map (flip studyToIVStudy oddsRatio) studies
          estimates  = 
            map (\(IVStudy s (Contrasts cts)) -> 
                  let Just e 
                        = Map.lookup 
                          (ComparisonId 
                            (TreatmentId (StringId "A")) 
                            (TreatmentId (StringId "B"))) cts
                   in ((roundDouble (point e) 4)
                       ,((roundDouble (lower $ ci e) 4)
                       ,(roundDouble (upper $ ci e) 4)))
                ) ors
          effects = map fst estimates
          cils       = map (fst . snd) estimates
          cius       = map (snd . snd) estimates
          correctORs = [0.8526, 0.7500, 0.2667, 0.6810, 0.6934, 0.6591]
          correctcil = [0.3895, 0.2612, 0.1661, 0.3143, 0.2984, 0.2329]
          correctciu = [1.8662, 2.1534, 0.4280, 1.4755, 1.6114, 1.8650]
      if effects == correctORs
        then if (correctcil == cils) && (correctciu == cius)
          then return $ testPassed name $ "passed!"
          else
            return
              $ testFailed name
              $ ( "wrong ORs CIs"
              , show cils
                <> show correctcil
                <> " "
                <> show cius
                <> show correctciu
              )
          else
            return
             $ testFailed name
             $ (show correctORs, show effects)

testLogOR :: IO Test
testLogOR = do
  let name        = "log Odds Ratio"
  let studiesFile = "test/binary.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      let Right lors = sequence $ map (flip studyToIVStudy logOddsRatio) studies
          estimates  = 
            map (\(IVStudy s (Contrasts cts)) -> 
                  let Just e 
                        = Map.lookup 
                          (ComparisonId 
                            (TreatmentId (StringId "A")) 
                            (TreatmentId (StringId "B"))) cts
                   in ((roundDouble (point e) 7)
                      ,(roundDouble (sqrt $ variance e) 7)
                      )
                ) lors
          effects = map fst estimates
          variances = map snd estimates
          correctlnORs = [-0.1594557,-0.2876821,-1.3217558,-0.3841625,-0.3661537,-0.4168938]
          correctvars  = [0.3996753,0.5381295,0.2414367,0.3944681,0.4302434,0.5306994]
      if effects == correctlnORs
        then if correctvars == variances
          then return $ testPassed name $ "passed!"
          else
            return
              $ testFailed name
              $ ( "wrong LogORs Variances"
              , show variances
                <> show correctvars
              )
          else
            return
             $ testFailed name
             $ (show correctlnORs, show estimates)

--testRR :: IO Test
--testRR = do
  --let name        = "Risk Ratio"
  --let studiesFile = "test/binary.json"
  --estudies <- readStudies studiesFile
  --case estudies of
    --Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    --Right studies -> do
      --let
        --rrs = rights $ V.toList $ V.map
          --(riskRatio . csvStudyToComparison)
          --studies
        --estimates  = map ((\s -> (roundDouble s 4)) . point) rrs
        --cils       = map ((\s -> (roundDouble s 4)) . lower . ci) rrs
        --cius       = map ((\s -> (roundDouble s 4)) . upper . ci) rrs
        --correctRRs = [0.7500, 0.8000, 0.7368, 0.3125, 0.7273, 0.8889]
        --correctcil = [0.3858, 0.3524, 0.3976, 0.2039, 0.3273, 0.4982]
        --correctciu = [1.4581, 1.8162, 1.3655, 0.4790, 1.6159, 1.5861]
      --if estimates == correctRRs
        --then if (correctcil == cils) && (correctciu == cius)
          --then return $ testPassed name $ "passed!"
          --else
            --return
            -- $ testFailed name
            -- $ ( "wrong RRs CIs"
              --, show cils
              -- <> show correctcil
              -- <> " "
              -- <> show cius
              -- <> show correctciu
              --)
        --else
          --return
          -- $ testFailed name
          -- $ ("wrong RRs", show estimates <> show correctRRs)



--testRD :: IO Test
--testRD = do
  --let name        = "Risk Difference"
  --let studiesFile = "test/binary.json"
  --estudies <- readStudies studiesFile
  --case estudies of
    --Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    --Right studies -> do
      --let rds = rights $ V.toList $ V.map
            --(riskDifference . csvStudyToComparison)
            --studies
          --estimates  = map ((\s -> (roundDouble s 4)) . point) rds
          --cils       = map ((\s -> (roundDouble s 4)) . lower . ci) rds
          --cius       = map ((\s -> (roundDouble s 4)) . upper . ci) rds
          --correctRDs = [-0.0615, -0.0500, -0.0625, -0.1375, -0.0750, -0.0308]
          --correctcil = [-0.2025, -0.2327, -0.1875, -0.1833, -0.2608, -0.1818]
          --correctciu = [0.0794, 0.1327, 0.0625, -0.0917, 0.1108, 0.1202]
      --if estimates == correctRDs
        --then if (correctciu == cius)
          --then if (correctcil == cils)
            --then return $ testPassed name $ "passed!"
            --else return $ testFailed
              --name
              --(show correctcil, "\n wrong RDs CIs lower" <> show cils)
          --else return $ testFailed
            --name
            --(show correctciu, "\n wrong RDs CIs upper" <> show cius)
        --else
          --return
          -- $ testFailed name
          -- $ (show correctRDs, "wrong RDs" <> show estimates)

--reverseRD :: IO Test
--reverseRD = do
  --let name        = "Reverse Risk Difference"
  --let studiesFile = "test/binary.json"
  --estudies <- readStudies studiesFile
  --case estudies of
    --Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    --Right studies -> do
      --let rds = map reverseEffect $ rights $ V.toList $ V.map
            --(riskDifference . csvStudyToComparison)
            --studies
          --estimates  = map ((\s -> (roundDouble s 4)) . point) rds
          --cils       = map ((\s -> (roundDouble s 4)) . lower . ci) rds
          --cius       = map ((\s -> (roundDouble s 4)) . upper . ci) rds
          --correctRDs = [0.0615, 0.0500, 0.0625, 0.1375, 0.0750, 0.0308]
          --correctcil = [-0.0794, -0.1327, -0.0625, 0.0917, -0.1108, -0.1202]
          --correctciu = [0.2025, 0.2327, 0.1875, 0.1833, 0.2608, 0.1818]
      --if estimates == correctRDs
        --then if (correctciu == cius)
          --then if (correctcil == cils)
            --then return $ testPassed name $ "passed!"
            --else return $ testFailed
              --name
              --(show correctcil, "\n wrong RDs CIs lower" <> show cils)
          --else return $ testFailed
            --name
            --(show correctciu, "\n wrong RDs CIs upper" <> show cius)
        --else
          --return
          -- $ testFailed name
          -- $ (show correctRDs, "wrong RDs" <> show estimates)

{-
reverseOR :: IO Test
reverseOR = do
  let name        = "reverse Odds Ratio"
  let studiesFile = "test/binary.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      let ors = map reverseEffect $ rights $ V.toList $ V.map
            (oddsRatio . csvStudyToComparison)
            studies
          estimates  = map ((\s -> (roundDouble s 4)) . point) ors
          cils       = map ((\s -> (roundDouble s 4)) . lower . ci) ors
          cius       = map ((\s -> (roundDouble s 4)) . upper . ci) ors
          correctORs = [1.4422, 1.3333, 1.4684, 3.7500, 1.5172, 1.1729]
          correctcil = [0.6206, 0.4644, 0.6777, 2.3363, 0.5362, 0.5359]
          correctciu = [3.3515, 3.8282, 3.1814, 6.0193, 4.2933, 2.5672]
      if estimates == correctORs
        then if (correctcil == cils) && (correctciu == cius)
          then return $ testPassed name $ "passed!"
          else
            return
            $ testFailed name
            $ ( "wrong ORs CIs"
              , show cils
              <> show correctcil
              <> " "
              <> show cius
              <> show correctciu
              )
        else
          return
          $ testFailed name
          $ ("wrong ORs", show estimates <> show correctORs)

reverseRR :: IO Test
reverseRR = do
  let name        = "reverse Risk Ratio"
  let studiesFile = "test/binary.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      let ors = map reverseEffect $ rights $ V.toList $ V.map
            (riskRatio . csvStudyToComparison)
            studies
          estimates  = map ((\s -> (roundDouble s 4)) . point) ors
          cils       = map ((\s -> (roundDouble s 4)) . lower . ci) ors
          cius       = map ((\s -> (roundDouble s 4)) . upper . ci) ors
          correctRRs = [1.3333, 1.2500, 1.3571, 3.2000, 1.3750, 1.1250]
          correctcil = [0.6858, 0.5506, 0.7324, 2.0875, 0.6189, 0.6305]
          correctciu = [2.5922, 2.8379, 2.5150, 4.9053, 3.0550, 2.0074]
      if estimates == correctRRs
        then if (correctcil == cils) && (correctciu == cius)
          then return $ testPassed name $ "passed!"
          else
            return
            $ testFailed name
            $ ( "wrong RRs CIs"
              , show cils
              <> show correctcil
              <> " "
              <> show cius
              <> show correctciu
              )
        else
          return
          $ testFailed name
          $ ("wrong RRs", show estimates <> show correctRRs)
-}
