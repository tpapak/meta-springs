module Test.Meta.NMA where

import           Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import qualified Data.Map.Strict               as Map
import qualified Data.Vector                   as V
import qualified Data.Csv                      as C
import           Data.Either

import           TestHS

import           Data.Numerics
import           Data.Meta.Studies
import           Data.Meta.Effects
import           Data.Meta.Pairwise.CommonEffect
import           Data.Meta.NMA

ioTests :: [IO Test]
ioTests = [ pinnedVertices
          , diabetesCE
          , fixedTau
          , continuousFixedTau
          ]

pinnedVertices :: IO Test
pinnedVertices = do
  let name = "Two NMAs with different pinned Vertices should give same effects"
  let studiesFile = "test/diabetes.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      let enetes1 = springNMA studies Nothing Nothing logOdds
          enetes2 = springNMA studies (Just 26) Nothing logOdds
       in case enetes1 of
            Left err -> return $ testFailed name ("no nets nma 1\n", show err <> "\n" <> show enetes1)
            Right netes1 -> do
              case enetes2 of
                Left err -> return $ testFailed name ("no nets nma 2\n", show err <> "\n" <> show enetes2)
                Right netes2 -> do
                  let elor1 = Map.map (Map.map (`roundDouble` 6)) $ networkEstimates netes1
                      elor2 = Map.map (Map.map (`roundDouble` 6)) $ networkEstimates netes2
                   in if elor1 == elor2 then return $ testPassed name "passed!" else return $ testFailed name ( "nma 1: \n" <> show elor1
                                                          , "nma 2: \n" <> show elor2 )

diabetesCE :: IO Test
diabetesCE = do
  let name = "NMA Diabetes common effect logOR"
  let studiesFile = "test/diabetes.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      let enetes = springNMA studies (Just 4) Nothing logOdds
       in case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right netes -> do
              let effs  = map (`roundDouble` 4)
                    $ Map.elems $ networkEstimates netes Map.! TreatmentId (StringId "ACE")
                  vars  = map (`roundDouble` 4)
                    $ Map.elems $ networkVariances netes Map.! TreatmentId (StringId "ACE")
                  founde = zip effs vars
                  expected = [(0,0)
                             ,(-0.0989, 0.0033)
                             ,(0.3115, 0.0022)
                             ,(0.1108, 0.0023)
                             ,(0.3656, 0.0030)
                             ,(0.1184, 0.0017)
                             ]
                  --logORs  0.00000000 -0.09888805  0.31152304  0.11084269  0.36558493  0.11836717  
                  --variances 0.003271733 0.002221581 0.002278977 0.003041829 0.001733474
               in if expected == founde
                    then return $ testPassed name $ show founde <> "passed!"
                    else do putStr $ debugmsg netes
                            return $ testFailed name (show expected, show founde)

fixedTau :: IO Test
fixedTau = do
  let name = "NMA Diabetes random effects fixed tau2=0.02094292"
  let studiesFile = "test/diabetes.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      let enetes = springNMA studies Nothing (Just 0.02094292) logOdds
       in case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right netes -> do
              let effs  = map (`roundDouble` 5)
                    $ Map.elems $ networkEstimates netes Map.! (TreatmentId (StringId "ACE"))
                  vars  = map (\x-> roundDouble x 5)
                    $ Map.elems $ (networkVariances netes) Map.! (TreatmentId (StringId "ACE"))
                  effs2 = map (\x-> roundDouble x 5)
                    $ Map.elems $ (networkEstimates netes) Map.! (TreatmentId (StringId "CCB"))
                  vars2 = map (\x-> roundDouble x 5)
                    $ Map.elems $ (networkVariances netes) Map.! (TreatmentId (StringId "CCB"))
                  founde = [zip effs vars, zip effs2 vars2]
                  expeffs = map ((flip roundDouble) 5) [0.0, -0.06732346, 0.34236550, 0.17290810, 0.41353789, 0.12080775]
                  expvars = map ((flip roundDouble) 5) [0.0, 0.011756111, 0.006772119, 0.006854693, 0.007849492, 0.006017346]
                  expeffs2 = map ((flip roundDouble) 5) [-0.17290810,-0.24023156, 0.16945740, 0.00000000, 0.24062980,-0.05210034]
                  expvars2 = map ((flip roundDouble) 5) [0.006854693,0.009666549,0.004689804,0.000000000,0.007793313,0.007518061]
                  expected = [zip expeffs expvars, zip expeffs2 expvars2]
               in if expected == founde
                    then return $ testPassed name $ show (founde) <> "passed!"
                    else do putStr $ debugmsg netes
                            return $ testFailed name $ (show expected, show founde)

continuousFixedTau :: IO Test
continuousFixedTau = do
  let name = "NMA continuous NoMultiarms"
  let studiesFile = "test/contNoMultiarm.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      let enetes = springNMA studies Nothing (Just 0.000234374) meanArm
        in case enetes of
             Left err -> return $ testFailed name 
               ("problem with reading studies 1\n", show err)
             Right netes -> 
               let effs  = map (`roundDouble` 5)
                    $ Map.elems $ networkEstimates netes Map.! (TreatmentId (IntId 1))
                   vars  = map (\x-> roundDouble x 5)
                    $ Map.elems $ (networkVariances netes) Map.! (TreatmentId (IntId 1))
                   founde = [zip effs vars]
                   expeffs = map ((flip roundDouble) 5) [0.000000000,-0.032091817,-0.020579331,-0.027655860,-0.009889617,-0.012893638]
                   expvars = map ((flip roundDouble) 5) [0.000000,1.379041e-04,2.074571e-04,2.036564e-04,1.276184e-04,6.912706e-05]
                   expected = [zip expeffs expvars]
                in if expected == founde
                      then return $ testPassed name $ show founde <> "passed!"
                      else return $ testFailed name $ (show expected, show founde <> "FAILeD \n" <> show (networkEstimates netes))
