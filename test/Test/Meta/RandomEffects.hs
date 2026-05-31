module Test.Meta.RandomEffects where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Csv as C
import Data.Either
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Meta.Effects
import Data.Meta.NMA
import Data.Meta.RandomEffects
import Data.Meta.Studies
import Test.Meta.GSLEstimators
import Data.Numerics
import qualified Data.Vector as V
import TestHS

ioTests :: [IO Test]
ioTests =
  [ pairwiseTau,
    diabetesRootNP,
    diabetesMinNP,
    cocalc,
    viecht1minNP,
    viecht1rootNP,
    viecht2REML,
    viecht2rootQDoFDiff,
    viecht3MinNP,
    viecht3RootNP,
    viecht3REML,
    sim1cont,
    randomNoMultiArm,
    predictionIntervalPairwise
  ]

diabetesMinNP :: IO Test
diabetesMinNP = do
  let name = "Get Tau for pairwise root of NP"
  let studiesFile = "test/diabetes.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      -- let enetes = springNMA studies (Just 2) (Just 0.96099292) logOdds
      -- let enetes = springREML studies logOdds (Just 0.019) (Just 1000) (Just 10E-5)
      let enetes = springREML studies logOdds (Just 0.019) (Just 1000) (Just 10E-5)
       in case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right (netes, taus) -> do
              let effs =
                    map (`roundDouble` 4) $
                      Map.elems $
                        networkEstimates netes Map.! TreatmentId (StringId "ACE")
                  vars =
                    map (`roundDouble` 4) $
                      Map.elems $
                        networkVariances netes Map.! TreatmentId (StringId "ACE")
                  founde = zip effs vars
                  expected =
                    [ (0, 0),
                      (-0.0989, 0.0033),
                      (0.3115, 0.0022),
                      (0.1108, 0.0023),
                      (0.3656, 0.0030),
                      (0.1184, 0.0017)
                    ]
                  foundEnergy = tauEnergy $ springNetwork netes
                  totalenergy = totalEnergy $ springNetwork netes
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  tausq = tau2 $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes
                  np = tauCostFunction $ springNetwork netes
               in if expected == founde
                    then do
                      putStr $ "number of Arms " <> show numberOfArms <> "\n"
                      putStr $ "tau energy " <> show foundEnergy <> "\n"
                      putStr $ "\n" <> "Q - dof = " <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "found TauSq: " <> show tausq <> "\n"
                      return $ testPassed name $ show netes <> "passed!"
                    else do
                      putStr $ "\n" <> "number of Arms " <> show numberOfArms <> "\n"
                      putStr $ "\n" <> "tau2" <> show tausq <> "\n"
                      putStr $ "\n" <> "tau energy " <> show foundEnergy <> "\n"
                      putStr $ "\n" <> "Q = " <> show (2 * foundEnergy) <> "\n"
                      putStr $ "\n" <> "dofs" <> show dofs <> "\n"
                      putStr $ "\n" <> "2*Q - dofs: " <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "\n" <> "NP: " <> show np <> "\n"
                      putStr $ "taus: " <> show taus <> "\n"
                      putStr $ "found TauSq: " <> show tausq <> "\n"
                      putStr $ "\n" <> "debug: " <> show (debugmsg netes) <> "\n"
                      return $ testFailed name (show expected, show ("Tau energy " <> show foundEnergy))

diabetesRootNP :: IO Test
diabetesRootNP = do
  let name = "Get Tau for pairwise root of NP"
  let studiesFile = "test/diabetes.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      -- let enetes = springNMA studies (Just 2) (Just 0.96099292) logOdds
      -- let enetes = springREML studies logOdds (Just 0.019) (Just 1000) (Just 10E-5)
      let enetes = springREML studies logOdds (Just 0.019) (Just 1000) (Just 10E-5)
       in case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right (netes, taus) -> do
              let effs =
                    map (`roundDouble` 4) $
                      Map.elems $
                        networkEstimates netes Map.! TreatmentId (StringId "ACE")
                  vars =
                    map (`roundDouble` 4) $
                      Map.elems $
                        networkVariances netes Map.! TreatmentId (StringId "ACE")
                  founde = zip effs vars
                  expected =
                    [ (0, 0),
                      (-0.0989, 0.0033),
                      (0.3115, 0.0022),
                      (0.1108, 0.0023),
                      (0.3656, 0.0030),
                      (0.1184, 0.0017)
                    ]
                  foundEnergy = tauEnergy $ springNetwork netes
                  totalenergy = totalEnergy $ springNetwork netes
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  tausq = tau2 $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes
                  np = tauCostFunction $ springNetwork netes
               in if expected == founde
                    then do
                      putStr $ "number of Arms " <> show numberOfArms <> "\n"
                      putStr $ "tau energy " <> show foundEnergy <> "\n"
                      putStr $ "\n" <> "Q - dof = " <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "found TauSq: " <> show tausq <> "\n"
                      return $ testPassed name $ show "passed!"
                    else do
                      putStr $ "\n" <> "number of Arms " <> show numberOfArms <> "\n"
                      putStr $ "\n" <> "tau2" <> show tausq <> "\n"
                      putStr $ "\n" <> "tau energy " <> show foundEnergy <> "\n"
                      putStr $ "\n" <> "Q = " <> show (2 * foundEnergy) <> "\n"
                      putStr $ "\n" <> "dofs" <> show dofs <> "\n"
                      putStr $ "\n" <> "2*Q - dofs" <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "\n" <> "NP: " <> show np <> "\n"
                      putStr $ "taus: " <> show taus <> "\n"
                      putStr $ "found TauSq: " <> show tausq <> "\n"
                      return $ testFailed name (show expected, show ("Tau energy " <> show foundEnergy))

pairwiseTau :: IO Test
pairwiseTau = do
  let name = "Get Tau for pairwise continuous studies REML tau2:9.606085"
  let studiesFile = "test/continuous.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      -- let enetes = springREML studies meanArm (Just 8.1) Nothing  (Just 10E-5)
      -- let enetes = springREML studies meanArm (Just 8.1) Nothing  (Just 10E-5)
      let enetes = springREML studies meanArm (Just 8.1) (Just 10) (Just 10E-5)
       in case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right (netes, taus) -> do
              let eff =
                    ( networkEstimates netes
                        Map.! TreatmentId (StringId "B")
                        Map.! TreatmentId (StringId "A")
                    )
                  var =
                    ( networkVariances netes
                        Map.! TreatmentId (StringId "B")
                        Map.! TreatmentId (StringId "A")
                    )
                  founde = (roundDouble eff 4, roundDouble var 4)
                  expected = (7.6137, roundDouble (1.940424 ^ 2) 4)
                  foundEnergy = (tauEnergy $ springNetwork netes)
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  dess = designs $ springNetwork netes
                  vss = vs2ts $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes
                  foundTauSquare = tau2 $ springNetwork netes
                  qqT = qqTau $ springNetwork netes
                  entau = tauEnergy $ springNetwork netes
               in if expected == founde
                    then return $ testPassed name $ show foundTauSquare <> "passed!"
                    else do
                      putStr $ "\n" <> "number of Arms " <> show numberOfArms
                      putStr $ "\n" <> "dofs " <> show dofs <> "\n"
                      putStr $ "\n" <> "found Tau2 :" <> show foundTauSquare <> "\n"
                      putStr $ "\n" <> "found Tau2s :" <> show taus <> "\n"
                      return $ testFailed name (show expected, show (founde))

cocalc :: IO Test
cocalc = do
  let name = "Get Tau for pairwise continuous cocalc example"
  let studiesFile = "test/cocalc.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      -- let enetes = springNMA studies Nothing (Just 0.499740321133196) meanArm
      let enetes = springREML studies meanArm (Just 8.1) Nothing (Just 10E-6)
       in -- let enetes = springREML studies meanArm (Just 8.1) Nothing  (Just 10E-6)
          -- let enetes = springREML studies meanArm (Just 8.1) Nothing  (Just 10E-5)
          -- let enetes = springNMA studies Nothing (Just 0.524429108454666) meanArm
          -- let enetes = springNMA studies Nothing (Just 0.488576) meanArm
          case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            -- Right netes -> do
            Right (netes, tau) -> do
              let eff =
                    ( networkEstimates netes
                        Map.! TreatmentId (StringId "B")
                        Map.! TreatmentId (StringId "A")
                    )
                  var =
                    ( networkVariances netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  founde = roundDouble eff 5
                  expected = roundDouble 0.143436360224868 5
                  foundEnergy = (tauEnergy $ springNetwork netes)
                  totalenergy = (totalEnergy $ springNetwork netes)
                  qqT = (qqTau $ springNetwork netes)
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes
                  logl = logLikelihood $ springNetwork netes
                  foundTauSquare = tau2 $ springNetwork netes
                  cost = tauCostFunction $ springNetwork netes
               in if expected == founde
                    then -- in if expected /= founde
                      return $ testPassed name $ show founde <> "passed!"
                    else do
                      putStr $ "\n" <> "number of Arms " <> show numberOfArms
                      putStr $ "\n" <> "tau energy " <> show foundEnergy <> "\n"
                      putStr $ "\n" <> "total energy " <> show totalenergy <> "\n"
                      putStr $ "\n" <> "Qτ: " <> show qqT <> "\n"
                      putStr $ "\n" <> "Q - dof = " <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "\n" <> "summary effect " <> show founde <> "\n"
                      putStr $ "\n" <> "tauEdges " <> show (tauEdges $ springNetwork netes) <> "\n"
                      putStr $ "\n" <> "log likelihood :" <> show logl <> "\n"
                      putStr $ "\n" <> "Qτ: " <> show qqT <> "\n"
                      putStr $ "\n" <> "Cost Function: " <> show cost <> "\n"
                      putStr $ "\n" <> "DEBUG :" <> (show $ debugmsg netes) <> "\n"
                      putStr $ "\n" <> "Tau2 :" <> show foundTauSquare <> "\n"
                      return $ testFailed name (show expected, show (founde))

-- Replicate Viechbauer's REML convergence examples found
-- https://www.metafor-project.org/doku.php/tips:convergence_problems_rma
viecht1minNP :: IO Test
viecht1minNP = do
  let name = "Replicate Viechbauer's REML convergence examples found minimize NP"
  let studiesFile = "test/viecht1.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      let enetes = springREML studies meanArm (Just 0.019) (Just 100000) (Just 10E-6)
       in -- let enetes = springNMA studies Nothing (Just 0.19878) meanArm
          case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right (netes, taus) -> do
              -- Right netes -> do
              let eff =
                    ( networkEstimates netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  var =
                    ( networkVariances netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  founde = (roundDouble eff 8, roundDouble var 4)
                  expected = (0.49182, roundDouble (0.30166 ^ 2) 4)
                  foundEnergy = (tauEnergy $ springNetwork netes)
                  ent = (totalEnergy $ springNetwork netes)
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes
                  logl = logLikelihood $ springNetwork netes
                  foundTauSquare = tau2 $ springNetwork netes
                  vss = vs2ts $ springNetwork netes
                  studySprings = springs $ springNetwork netes
                  qqT = (qqTau $ springNetwork netes)
                  cost = tauCostFunction $ springNetwork netes
               in if expected == founde
                    then return $ testPassed name $ show founde <> "passed!"
                    else do
                      putStr $ "\n" <> "number of Arms " <> show numberOfArms
                      putStr $ "\n" <> "Q - dof = " <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "\n" <> "summary effect " <> show founde <> "\n"
                      putStr $ "\n" <> "tauEdges " <> show (tauEdges $ springNetwork netes) <> "\n"
                      -- putStr $  "\n" <> "Tau2s :" <> show taus <> "\n"
                      putStr $ "\n" <> "Tau2 :" <> show foundTauSquare <> "\n"
                      putStr $ "\n" <> "2* En: " <> show (2 * ent) <> "\n"
                      putStr $ "\n" <> "2* En: " <> show (2 * ent) <> "\n"
                      putStr $ "\n" <> "Qτ: " <> show qqT <> "\n"
                      putStr $ "\n" <> "Cost Function: " <> show cost <> "\n"
                      putStr $ "\n" <> "tau energy " <> show foundEnergy <> "\n"
                      putStr $ "\n" <> "debug: " <> show (debugmsg netes) <> "\n"
                      return $ testFailed name (show expected, show (founde))

-- Replicate Viechbauer's REML convergence examples found
-- https://www.metafor-project.org/doku.php/tips:convergence_problems_rma
viecht1maxQtau :: IO Test
viecht1maxQtau = do
  let name = "Replicate Viechbauer's REML convergence examples found by maximizing Qtau"
  let studiesFile = "test/viecht1.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      let enetes = springREML studies meanArm (Just 0.019) (Just 100000) (Just 10E-6)
       in case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right (netes, taus) -> do
              -- Right netes -> do
              let eff =
                    ( networkEstimates netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  var =
                    ( networkVariances netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  founde = (roundDouble eff 8, roundDouble var 4)
                  expected = (0.49182, roundDouble (0.30166 ^ 2) 4)
                  foundEnergy = (tauEnergy $ springNetwork netes)
                  ent = (totalEnergy $ springNetwork netes)
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes
                  logl = logLikelihood $ springNetwork netes
                  foundTauSquare = tau2 $ springNetwork netes
                  vss = vs2ts $ springNetwork netes
                  studySprings = springs $ springNetwork netes
                  qqT = (qqTau $ springNetwork netes)
               in if expected == founde
                    then return $ testPassed name $ show founde <> "passed!"
                    else do
                      putStr $ "\n" <> "number of Arms " <> show numberOfArms
                      putStr $ "\n" <> "Q - dof = " <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "\n" <> "summary effect " <> show founde <> "\n"
                      putStr $ "\n" <> "tauEdges " <> show (tauEdges $ springNetwork netes) <> "\n"
                      putStr $ "\n" <> "Tau2s :" <> show taus <> "\n"
                      putStr $ "\n" <> "Tau2 :" <> show foundTauSquare <> "\n"
                      putStr $ "\n" <> "2* En: " <> show (2 * ent) <> "\n"
                      putStr $ "\n" <> "Qτ: " <> show qqT <> "\n"
                      putStr $ "\n" <> "Qt' :" <> (show $ qqTau' $ springNetwork netes) <> "\n"
                      putStr $ "\n" <> "tau energy " <> show foundEnergy <> "\n"
                      putStr $ "\n" <> "debug: " <> show (debugmsg netes) <> "\n"
                      return $ testFailed name (show expected, show (founde))

-- Replicate Viechbauer's REML convergence examples found
-- https://www.metafor-project.org/doku.php/tips:convergence_problems_rma
viecht1rootNP :: IO Test
viecht1rootNP = do
  let name = "Replicate Viechbauer's REML convergence examples found minimize NP"
  let studiesFile = "test/viecht1.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      let enetes = springREML studies meanArm (Just 0.019) (Just 100000) (Just 10E-6)
       in -- let enetes = springNMA studies Nothing (Just 0.19878) meanArm
          case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right (netes, taus) -> do
              let eff =
                    ( networkEstimates netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  var =
                    ( networkVariances netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  founde = (roundDouble eff 8, roundDouble var 4)
                  expected = (0.49182, roundDouble (0.30166 ^ 2) 4)
                  foundEnergy = (tauEnergy $ springNetwork netes)
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes
                  logl = logLikelihood $ springNetwork netes
                  foundTauSquare = tau2 $ springNetwork netes
                  vss = vs2ts $ springNetwork netes
                  studySprings = springs $ springNetwork netes
                  qqT = (qqTau $ springNetwork netes)
               in if expected == founde
                    then return $ testPassed name $ show founde <> "passed!"
                    else do
                      putStr $ "\n" <> "number of Arms " <> show numberOfArms
                      putStr $ "\n" <> "tau energy " <> show foundEnergy <> "\n"
                      putStr $ "\n" <> "Qτ: " <> show qqT <> "\n"
                      putStr $ "\n" <> "Q - dof = " <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "\n" <> "summary effect " <> show founde <> "\n"
                      putStr $ "\n" <> "tauEdges " <> show (tauEdges $ springNetwork netes) <> "\n"
                      putStr $ "\n" <> "Tau2s :" <> show taus <> "\n"
                      putStr $ "\n" <> "Tau2 :" <> show foundTauSquare <> "\n"
                      putStr $ "\n" <> "debug: " <> show (debugmsg netes) <> "\n"
                      return $ testFailed name (show expected, show (founde))

viecht2REML :: IO Test
viecht2REML = do
  let name = "Replicate Viechbauer's REML convergence examples found"
  let studiesFile = "test/viecht2.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      let enetes = springREML studies meanArm (Just 10.0019) (Just 1000) (Just 10E-5)
       in -- let enetes = springNMA studies Nothing (Just 0.19878) meanArm
          case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right (netes, taus) -> do
              let eff =
                    ( networkEstimates netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  var =
                    ( networkVariances netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  founde = (roundDouble eff 8, roundDouble var 4)
                  expected = (0.49182, roundDouble (0.30166 ^ 2) 4)
                  foundEnergy = (tauEnergy $ springNetwork netes)
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes
                  logl = logLikelihood $ springNetwork netes
                  foundTauSquare = tau2 $ springNetwork netes
               in if expected == founde
                    then return $ testPassed name $ show founde <> "passed!"
                    else do
                      putStr $ "\n" <> "number of Arms " <> show numberOfArms
                      putStr $ "\n" <> "tau energy " <> show foundEnergy <> "\n"
                      putStr $ "\n" <> "summary effect " <> show founde <> "\n"
                      putStr $ "\n" <> "tauEdges " <> show (tauEdges $ springNetwork netes) <> "\n"
                      putStr $ "\n" <> "Tau2 :" <> show foundTauSquare <> "\n"
                      putStr $ "found TauSq: " <> show taus <> "\n"
                      putStr $ "\n" <> "Q - dof = " <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "\n" <> "log likelihood :" <> show logl <> "\n"
                      return $ testFailed name (show expected, show (founde))

viecht2rootQDoFDiff :: IO Test
viecht2rootQDoFDiff = do
  let name = "Replicate Viechbauer's REML convergence examples found"
  let studiesFile = "test/viecht2.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      -- let enetes = springREML studies meanArm (Just 10.0019) (Just 1000) (Just 10E-5)
      let enetes = springREML studies meanArm (Just 0.019) (Just 1000) (Just 10E-5)
       in -- let enetes =  studies meanArm (Just 10.0019) (Just 1000) (Just 10E-5)
          -- let enetes = springNMA studies Nothing (Just 0.19878) meanArm
          case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right (netes, taus) -> do
              let eff =
                    ( networkEstimates netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  var =
                    ( networkVariances netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  founde = (roundDouble eff 8, roundDouble var 4)
                  expected = (0.49182, roundDouble (0.30166 ^ 2) 4)
                  foundEnergy = (tauEnergy $ springNetwork netes)
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes
                  logl = logLikelihood $ springNetwork netes
                  foundTauSquare = tau2 $ springNetwork netes
               in if expected == founde
                    then return $ testPassed name $ show founde <> "passed!"
                    else do
                      putStr $ "\n" <> "number of Arms " <> show numberOfArms
                      putStr $ "\n" <> "tau energy " <> show foundEnergy <> "\n"
                      putStr $ "\n" <> "summary effect " <> show founde <> "\n"
                      putStr $ "\n" <> "tauEdges " <> show (tauEdges $ springNetwork netes) <> "\n"
                      putStr $ "\n" <> "Tau2 :" <> show foundTauSquare <> "\n"
                      putStr $ "found TauSq: " <> show taus <> "\n"
                      putStr $ "\n" <> "Q - dof = " <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "\n" <> "log likelihood :" <> show logl <> "\n"
                      return $ testFailed name (show expected, show (founde))

viecht3MinNP :: IO Test
viecht3MinNP = do
  let name = "Replicate Viechbauer's REML convergence examples found"
  let studiesFile = "test/viecht3.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      let enetes = springREML studies meanArm (Just 1.9) (Just 10000) (Just 10E-5)
       in -- let enetes = springNMA studies Nothing (Just 0.19878) meanArm
          case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right (netes, taus) -> do
              let eff =
                    ( networkEstimates netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  var =
                    ( networkVariances netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  founde = (roundDouble eff 8, roundDouble var 4)
                  expected = (0.49182, roundDouble (0.30166 ^ 2) 4)
                  foundEnergy = (tauEnergy $ springNetwork netes)
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes
                  logl = logLikelihood $ springNetwork netes
                  foundTauSquare = tau2 $ springNetwork netes
               in if expected == founde
                    then return $ testPassed name $ show founde <> "passed!"
                    else do
                      putStr $ "\n" <> "number of Arms " <> show numberOfArms
                      putStr $ "\n" <> "tau energy " <> show foundEnergy <> "\n"
                      putStr $ "\n" <> "summary effect " <> show founde <> "\n"
                      putStr $ "\n" <> "tauEdges " <> show (tauEdges $ springNetwork netes) <> "\n"
                      putStr $ "\n" <> "Tau2 :" <> show foundTauSquare <> "\n"
                      putStr $ "found TauSq: " <> show taus <> "\n"
                      putStr $ "\n" <> "Q - dof = " <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "\n" <> "log likelihood :" <> show logl <> "\n"
                      return $ testFailed name (show expected, show (founde))

viecht3RootNP :: IO Test
viecht3RootNP = do
  let name = "Replicate Viechbauer's REML convergence examples found"
  let studiesFile = "test/viecht3.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      let enetes = springREML studies meanArm (Just 0.19) (Just 10000) (Just 10E-5)
       in -- let enetes = springNMA studies Nothing (Just 0.19878) meanArm
          case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right (netes, taus) -> do
              let eff =
                    ( networkEstimates netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  var =
                    ( networkVariances netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  founde = (roundDouble eff 8, roundDouble var 4)
                  expected = (0.49182, roundDouble (0.30166 ^ 2) 4)
                  foundEnergy = (tauEnergy $ springNetwork netes)
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes
                  logl = logLikelihood $ springNetwork netes
                  foundTauSquare = tau2 $ springNetwork netes
               in if expected == founde
                    then return $ testPassed name $ show founde <> "passed!"
                    else do
                      putStr $ "\n" <> "number of Arms " <> show numberOfArms
                      putStr $ "\n" <> "tau energy " <> show foundEnergy <> "\n"
                      putStr $ "\n" <> "summary effect " <> show founde <> "\n"
                      putStr $ "\n" <> "tauEdges " <> show (tauEdges $ springNetwork netes) <> "\n"
                      putStr $ "\n" <> "Tau2 :" <> show foundTauSquare <> "\n"
                      putStr $ "found TauSq: " <> show taus <> "\n"
                      putStr $ "\n" <> "Q - dof = " <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "\n" <> "log likelihood :" <> show logl <> "\n"
                      return $ testFailed name (show expected, show (founde))

viecht3REML :: IO Test
viecht3REML = do
  let name = "Replicate Viechbauer's REML convergence examples found"
  let studiesFile = "test/viecht3.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      let enetes = springREML studies meanArm (Just 0.0019) (Just 10000) (Just 10E-5)
       in -- let enetes = springNMA studies Nothing (Just 0.19878) meanArm
          case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right (netes, taus) -> do
              let eff =
                    ( networkEstimates netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  var =
                    ( networkVariances netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  founde = (roundDouble eff 8, roundDouble var 4)
                  expected = (0.49182, roundDouble (0.30166 ^ 2) 4)
                  foundEnergy = (tauEnergy $ springNetwork netes)
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes
                  logl = logLikelihood $ springNetwork netes
                  foundTauSquare = tau2 $ springNetwork netes
                  qqT = (qqTau $ springNetwork netes)
               in if expected == founde
                    then return $ testPassed name $ show founde <> "passed!"
                    else do
                      putStr $ "\n" <> "number of Arms " <> show numberOfArms
                      putStr $ "\n" <> "tau energy " <> show foundEnergy <> "\n"
                      putStr $ "\n" <> "summary effect " <> show founde <> "\n"
                      putStr $ "\n" <> "tauEdges " <> show (tauEdges $ springNetwork netes) <> "\n"
                      putStr $ "\n" <> "Tau2 :" <> show foundTauSquare <> "\n"
                      putStr $ "found TauSq: " <> show taus <> "\n"
                      putStr $ "\n" <> "Q - dof = " <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "\n" <> "log likelihood :" <> show logl <> "\n"
                      putStr $ "\n" <> "Qτ: " <> show qqT <> "\n"
                      return $ testFailed name (show expected, show $ networkEstimates netes)

sim1cont :: IO Test
sim1cont = do
  let name = "sim cont 1 ftau2, ftheta, fv_AB, min_sample_size, max_sample_size, fn:  0.8, 0.5, 1.5, 51, 149, 2000"
  -- let studiesFile = "test/simfew.json"
  let studiesFile = "test/cocalc.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error merging arms to studies", err)
    Right studies -> do
      -- let enetes = springREML studies meanArm (Just 2.019) (Just 1000) (Just 10E-5)
      let enetes = springREML studies meanArm (Just 0.19) (Just 10000) (Just 10E-5)
       in case enetes of
            Left err -> return $ testFailed name ("no nets \n", show studies <> "\n" <> show enetes)
            Right (netes, taus) -> do
              let eff =
                    ( networkEstimates netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  var =
                    ( networkVariances netes
                        Map.! TreatmentId (StringId "A")
                        Map.! TreatmentId (StringId "B")
                    )
                  founde = (roundDouble eff 8, roundDouble var 4)
                  expected = (0.49182, roundDouble (0.30166 ^ 2) 4)
                  tEnergy = (tauEnergy $ springNetwork netes)
                  foundEnergy = (totalEnergy $ springNetwork netes)
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes
                  foundTauSquare = tau2 $ springNetwork netes
                  qqT = (qqTau $ springNetwork netes)
                  thiEnergy = (thetaiEnergy $ springNetwork netes)
                  thiEnergy' = (thetaiEnergy' $ springNetwork netes)
                  cost = tauCostFunction $ springNetwork netes
               in if expected == founde
                    then return $ testPassed name $ show founde <> "passed!"
                    else do
                      putStr $ "\n" <> "number of Arms " <> show numberOfArms
                      putStr $ "\n" <> "Tau2 :" <> show foundTauSquare <> "\n"
                      putStr $ "found TauSq: " <> show taus <> "\n"
                      putStr $ "total Energy: " <> show foundEnergy <> "\n"
                      putStr $ "tau Energy: " <> show tEnergy <> "\n"
                      putStr $ "\n" <> "Q - dof = " <> show (2 * foundEnergy - dofs) <> "\n"
                      putStr $ "\n" <> "thetaIEnergy = " <> show (thiEnergy) <> "\n"
                      putStr $ "\n" <> "thetaIEnergy' = " <> show (thiEnergy') <> "\n"
                      putStr $ "\n" <> "tauCost function = " <> show (cost) <> "\n"
                      return $ testFailed name (show expected, show $ networkEstimates netes)

randomNoMultiArm :: IO Test
randomNoMultiArm = do
  let name = "NMA random effects NoMultiarms "
  let studiesFile = "test/contNoMultiarm.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      -- let enetes = springNMA studies Nothing (Just 0.000234374) meanArm
      let enetes = springNMA studies Nothing Nothing meanArm
       in case enetes of
            Left err ->
              return $
                testFailed
                  name
                  ("problem with reading studies 1\n", show err)
            Right netes ->
              let effs =
                    map (`roundDouble` 5) $
                      Map.elems $
                        networkEstimates netes Map.! (TreatmentId (IntId 1))
                  vars =
                    map (\x -> roundDouble x 5) $
                      Map.elems $
                        (networkVariances netes) Map.! (TreatmentId (IntId 1))
                  founde = [zip effs vars]
                  expeffs = map (flip roundDouble 5) [0.000000000, -0.022293198, -0.014363957, -0.032296552, -0.006013469, -0.005280683]
                  expvars = map (flip roundDouble 5) [0.000000e+00, 4.888726e-05, 6.719252e-05, 8.624463e-05, 4.351166e-05, 1.079675e-05]
                  expected = [zip expeffs expvars]
               in if expected == founde
                    then return $ testPassed name $ show founde <> "passed!"
                    else return $ testFailed name $ (show expected, show founde)

-- | Verify that study-specific κ_eff gives the correct prediction interval.
-- For pairwise random effects (continuous.json), we verify:
--   κ_i = 1/σ_i² + 1/(τ² + Var(μ̂))
-- by computing it two ways:
--   1. From the spring network (studyKeff)
--   2. Algebraically from the study data
predictionIntervalPairwise :: IO Test
predictionIntervalPairwise = do
  let name = "Prediction interval from κ_eff matches algebraic formula (pairwise)"
  let studiesFile = "test/continuous.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error reading studies", err)
    Right studies -> do
      let enetes = springREML studies meanArm (Just 8.1) Nothing (Just 10E-6)
       in case enetes of
            Left err -> return $ testFailed name ("no nets \n", show err)
            Right (netes, _) -> do
              let spnet = springNetwork netes
                  τ2 = tau2 spnet
                  -- From spring network
                  σ2s = studyWithinVariance spnet
                  keffs = studyKeff spnet
                  piVars = studyPredictionVariance spnet
                  piNew = predictionIntervalVariance spnet
                  -- Algebraic computation from study data
                  studyVariances = map (\st ->
                    let arms = getStudyArms st
                        armVars = map (\arm -> case meanArm arm of
                          Right (Mean _ v) -> v
                          Left _ -> 0) arms
                     in sum armVars
                    ) studies
                  ws = map (\σ2 -> 1 / (σ2 + τ2)) studyVariances
                  varMuHat = 1 / sum ws
                  algebraicKeffs = map (\σ2 ->
                    1 / σ2 + 1 / (τ2 + varMuHat)
                    ) studyVariances
                  algebraicPIVars = map (1 /) algebraicKeffs
                  algebraicPINew = τ2 + varMuHat
                  -- Compare (round to 6 decimals)
                  springPIVars = map (`roundDouble` 6) $ Map.elems piVars
                  algPIVars = map (`roundDouble` 6) algebraicPIVars
                  springPINew = roundDouble piNew 6
                  algPINew = roundDouble algebraicPINew 6
                  match = springPIVars == algPIVars && springPINew == algPINew
               in if match
                    then do
                      putStr $ "\n  τ² = " <> show τ2
                      putStr $ "\n  Var(μ̂) = " <> show varMuHat
                      putStr $ "\n  PI_new = τ² + Var(μ̂) = " <> show piNew
                      putStr $ "\n  σ_i² = " <> show (Map.elems σ2s)
                      putStr $ "\n  κ_i = " <> show (Map.elems keffs)
                      putStr $ "\n  Var(θ_i|data) = 1/κ_i = " <> show (Map.elems piVars) <> "\n"
                      return $ testPassed name "κ_eff prediction intervals verified!"
                    else do
                      putStr $ "\n  τ² = " <> show τ2
                      putStr $ "\n  Var(μ̂) = " <> show varMuHat
                      putStr $ "\n  Spring PI vars: " <> show springPIVars
                      putStr $ "\n  Algebraic PI vars: " <> show algPIVars
                      putStr $ "\n  Spring PI new: " <> show springPINew
                      putStr $ "\n  Algebraic PI new: " <> show algPINew <> "\n"
                      return $ testFailed name (show algPIVars, show springPIVars)
