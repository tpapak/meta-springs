{-# LANGUAGE BangPatterns #-}
-- | Worked example and NMA simulation for SSPIs
module Test.Meta.NMAWorkedExample where

import Data.Either
import qualified Data.Graph.AdjacencyList as G
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import System.IO (hFlush, stdout)

import TestHS
import Data.Numerics
import Data.Meta.Effects
import Data.Meta.NMA
import Data.Meta.RandomEffects
import Data.Meta.Studies

ioTests :: [IO Test]
ioTests = [nmaWorkedExample, nmaSspiSimulation]

-- | Worked example: SSPIs for the contNoMultiarm NMA dataset
nmaWorkedExample :: IO Test
nmaWorkedExample = do
  let name = "NMA Worked Example: SSPIs for contNoMultiarm"
  let studiesFile = "test/contNoMultiarm.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error reading studies", err)
    Right studies -> do
      let tau2Fixed = 2.34374e-4
          enetes = springNMA studies Nothing (Just tau2Fixed) meanArm
       in case enetes of
            Left err -> return $ testFailed name ("NMA failed", show err)
            Right netes -> do
              let spnet = springNetwork netes
                  τ2 = tau2 spnet
                  σ2s = studyWithinVariance spnet
                  keffs = studyKeff spnet
                  piVars = studyPredictionVariance spnet
                  piNew = predictionIntervalVariance spnet
                  nStudies = Map.size σ2s

              hFlush stdout
              putStr $ "\n  === NMA Worked Example: contNoMultiarm ==="
              putStr $ "\n  Studies: " <> show nStudies
                    <> ", Treatments: 6, τ² = " <> show τ2
              putStr $ "\n  PI_new = " <> show (roundDouble piNew 8)
              putStr $ "\n"
              putStr $ "\n  Study | σ_i²       | κ_i      | 1/κ_i      | SE(θ_i)"
              putStr $ "\n  ------|------------|----------|------------|--------"

              let rows = Map.toList piVars
              mapM_ (\(sv, pv) ->
                let s2 = σ2s Map.! sv
                    ke = keffs Map.! sv
                 in putStr $ "\n  " <> (if sv < 10 then " " else "") <> show sv
                      <> "   | " <> show (roundDouble s2 6)
                      <> " | " <> show (roundDouble ke 4)
                      <> " | " <> show (roundDouble pv 6)
                      <> " | " <> show (roundDouble (sqrt pv) 6)
                ) rows
              putStr $ "\n"
              hFlush stdout

              return $ testPassed name $
                show nStudies <> " studies, τ²=" <> show τ2

-- | NMA SSPI coverage simulation
-- Generates a 4-treatment network, runs NMA, computes SSPIs, checks coverage.
-- Each study is a pairwise comparison; we check whether the true
-- comparison effect falls within the SSPI.
nmaSspiSimulation :: IO Test
nmaSspiSimulation = do
  let name = "NMA SSPI coverage simulation (4 treatments, τ² known)"
  let nSims = 500
      μTrue = [0.0, 0.3, 0.1, 0.5] :: [Double]
      τ2True = 0.05
      z975 = 1.959964
      -- (treatA_idx, treatB_idx, sdA, nA, sdB, nB)
      studyDesigns =
        [ (1,2, 1.0,80, 1.0,80), (1,2, 1.2,60, 1.1,65)
        , (1,2, 0.9,100, 0.9,100), (1,2, 1.0,50, 1.0,50)
        , (1,3, 1.0,90, 1.0,85), (1,3, 1.2,55, 1.1,60)
        , (1,3, 0.8,120, 0.9,110), (1,3, 1.0,70, 1.0,70)
        , (2,3, 1.0,60, 1.0,60), (2,3, 1.1,80, 1.0,80)
        , (2,3, 0.9,90, 1.0,85), (1,4, 1.0,70, 1.0,75)
        , (1,4, 1.2,50, 1.1,55), (1,4, 0.9,100, 0.9,100)
        , (3,4, 1.0,65, 1.0,60), (3,4, 1.1,50, 1.0,55)
        , (2,4, 1.0,80, 1.0,80), (2,4, 0.9,90, 1.0,85)
        ]
      k = length studyDesigns

      lcg s = let s' = (1103515245 * s + 12345) `mod` (2^31 :: Int)
               in (fromIntegral s' / fromIntegral (2^31 :: Int) :: Double, s')
      normalLCG mu sigma s =
        let (u1, s1) = lcg s
            (u2, s2) = lcg s1
            r = sqrt (-2 * log (max 1e-10 u1))
            z = r * cos (2 * pi * u2)
         in (mu + sigma * z, s2)

      simOne :: Int -> (Int, Int, Int)
      simOne s0 =
        let -- Generate data: for each study, generate arm effects and observations
            (studyInfo, sEnd) = foldl'
              (\(acc, s) (idx, (tA,tB,sdA,nA,sdB,nB)) ->
                let μA = μTrue !! (tA - 1)
                    μB = μTrue !! (tB - 1)
                    (εA, s1) = normalLCG 0 (sqrt (τ2True/2)) s
                    (εB, s2) = normalLCG 0 (sqrt (τ2True/2)) s1
                    -- True comparison effect for this study
                    trueδ = (μA + εA) - (μB + εB)
                    -- Observed arm means (true + sampling noise)
                    (noiseA, s3) = normalLCG 0 (sdA / sqrt (fromIntegral nA)) s2
                    (noiseB, s4) = normalLCG 0 (sdB / sqrt (fromIntegral nB)) s3
                    obsA = μA + εA + noiseA
                    obsB = μB + εB + noiseB
                    -- Within-study variance of comparison
                    σ2 = sdA^2 / fromIntegral nA + sdB^2 / fromIntegral nB
                    -- Observed comparison effect
                    yi = obsA - obsB
                    sid = StudyId (IntId (idx + 1))
                    tidA = TreatmentId (IntId tA)
                    tidB = TreatmentId (IntId tB)
                    study = ContinuousStudy sid
                      [ ContinuousArm tidA obsA sdA nA
                      , ContinuousArm tidB obsB sdB nB
                      ]
                 in (acc ++ [(study, trueδ, yi, σ2, tidA, tidB)], s4)
              ) ([], s0) (zip [0..] studyDesigns)
            studies = map (\(s,_,_,_,_,_) -> s) studyInfo
         in case springNMA studies Nothing (Just τ2True) meanArm of
              Left _ -> (0, 0, k)
              Right netes ->
                let netEsts = networkEstimates netes
                    netVars = networkVariances netes
                    -- Check coverage for each study
                    (cCov, kCov) = foldl'
                      (\(!cc, !kc) (_study, trueδ, yi, σ2, tidA, tidB) ->
                        let -- Network estimate for this comparison
                            muHatAB = (netEsts Map.! tidA) Map.! tidB
                            -- Network variance for this comparison
                            varMuAB = (netVars Map.! tidA) Map.! tidB
                            -- Crude SSPI (μ known)
                            crudeVar = σ2 * τ2True / (σ2 + τ2True)
                            bCrude = σ2 / (σ2 + τ2True)
                            thetaCrude = bCrude * muHatAB + (1 - bCrude) * yi
                            -- κ_eff SSPI (μ estimated with network variance)
                            keff = 1/σ2 + 1/(τ2True + varMuAB)
                            keffVar = 1 / keff
                            thetaKeff = (yi/σ2 + muHatAB/(τ2True + varMuAB)) / keff
                            -- Coverage
                            crudeOk = if abs (trueδ - thetaCrude) <= z975 * sqrt crudeVar then 1 else 0
                            keffOk = if abs (trueδ - thetaKeff) <= z975 * sqrt keffVar then 1 else 0
                         in (cc + crudeOk, kc + keffOk)
                      ) (0, 0) studyInfo
                 in (cCov, kCov, k)

      -- Run simulations
      (cTotal, kTotal, nTotal, _) = foldl'
        (\(!cA, !kA, !nA, !s) _ ->
          let (c, ke, n) = simOne s
              (_, s') = lcg s
           in (cA + c, kA + ke, nA + n, s')
        ) (0, 0, 0, 42) [1..nSims]

      crudeCov = fromIntegral cTotal / fromIntegral nTotal :: Double
      keffCov = fromIntegral kTotal / fromIntegral nTotal :: Double

  hFlush stdout
  putStr $ "\n  === NMA SSPI Coverage (4 treatments, τ² known) ==="
  putStr $ "\n  " <> show k <> " studies, " <> show nSims <> " sims"
  putStr $ "\n  Crude: " <> show (roundDouble crudeCov 4)
  putStr $ "\n  κ_eff: " <> show (roundDouble keffCov 4)
  putStr $ "\n  (target: 0.95)\n"
  hFlush stdout

  return $ testPassed name $
    "crude=" <> show (roundDouble crudeCov 4) <>
    " keff=" <> show (roundDouble keffCov 4)
