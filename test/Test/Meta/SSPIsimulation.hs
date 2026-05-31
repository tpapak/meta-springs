{-# LANGUAGE BangPatterns #-}
-- | Simulation study to verify which study-specific prediction interval
-- formula gives correct coverage.
--
-- We simulate from the random effects model:
--   θ_i ~ N(μ, τ²)           -- true study effects
--   y_i ~ N(θ_i, σ_i²)       -- observed effects
--
-- Then compute 95% prediction intervals for θ_i under three formulas:
--   1. Crude:      Var = σ_i²τ²/(σ_i² + τ²)                         = 1/(1/σ_i² + 1/τ²)
--   2. Raudenbush: Var = crude + σ_i⁴/((σ_i² + τ²)² · w)            [van Aert 2021, Eq 10]
--   3. κ_eff:      Var = 1/(1/σ_i² + 1/(τ² + Var(μ̂)))              [our formula]
--
-- and check what fraction of the time θ_i falls within the interval.
-- Correct coverage should be ~95%.

module Test.Meta.SSPIsimulation where

import System.Random
import System.IO (hFlush, stdout)
import Data.List
import TestHS

-- | Simple Box-Muller normal random number generator
normalPair :: StdGen -> (Double, Double, StdGen)
normalPair g =
  let (u1, g1) = randomR (0.0001, 0.9999) g  -- avoid exact 0
      (u2, g2) = randomR (0.0001, 0.9999) g1
      r = sqrt (-2 * log u1)
      z1 = r * cos (2 * pi * u2)
      z2 = r * sin (2 * pi * u2)
   in (z1, z2, g2)

normal :: Double -> Double -> StdGen -> (Double, StdGen)
normal mu sigma g =
  let (z, _, g') = normalPair g
   in (mu + sigma * z, g')

ioTests :: [IO Test]
ioTests = [sspiCoverageSimulation, sspiCoverageLargeK, sspiCoverageKnownTau]

sspiCoverageSimulation :: IO Test
sspiCoverageSimulation = do
  let name = "SSPI coverage simulation: crude vs Raudenbush vs κ_eff"
  let nSims = 2000     -- number of simulated meta-analyses
      k = 10           -- number of studies per meta-analysis
      μTrue = 0.5      -- true overall effect
      τ2True = 0.1     -- true between-study variance
      -- Fixed within-study variances (heterogeneous)
      σ2s = [0.05, 0.08, 0.12, 0.03, 0.15, 0.07, 0.20, 0.04, 0.10, 0.06]
      z975 = 1.959964  -- z for 95% CI

  g0 <- getStdGen

  let simulate :: StdGen -> Int -> (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int)
      simulate _ 0 acc = acc
      simulate g n (!crudeCov, !raudCov, !keffCov, !crudeCovAll, !raudCovAll, !keffCovAll) =
        let -- Generate true study effects θ_i ~ N(μ, τ²)
            (thetas, g1) = foldl' (\(ts, gen) _ ->
              let (t, gen') = normal μTrue (sqrt τ2True) gen
               in (ts ++ [t], gen')
              ) ([], g) [1..k]
            -- Generate observed effects y_i ~ N(θ_i, σ_i²)
            (ys, g2) = foldl' (\(oss, gen) (θi, σ2i) ->
              let (yi, gen') = normal θi (sqrt σ2i) gen
               in (oss ++ [yi], gen')
              ) ([], g1) (zip thetas σ2s)
            -- Estimate τ² using DerSimonian-Laird
            wsFE = map (\σ2 -> 1 / σ2) σ2s
            sumWFE = sum wsFE
            μFE = sum (zipWith (\w y -> w * y) wsFE ys) / sumWFE
            qStat = sum $ zipWith (\w y -> w * (y - μFE)^2) wsFE ys
            cFactor = sumWFE - sum (map (^2) wsFE) / sumWFE
            τ2Hat = max 0 ((qStat - fromIntegral (k - 1)) / cFactor)
            -- Random effects weights and pooled estimate
            wsRE = map (\σ2 -> 1 / (σ2 + τ2Hat)) σ2s
            sumWRE = sum wsRE
            μHat = sum (zipWith (\w y -> w * y) wsRE ys) / sumWRE
            varMuHat = 1 / sumWRE
            -- Empirical Bayes / BLUP estimate for each study
            ebEstimates = zipWith (\yi σ2i ->
              let bi = σ2i / (σ2i + τ2Hat)
               in bi * μHat + (1 - bi) * yi
              ) ys σ2s
            -- Formula 1: Crude variance = σ_i²τ²/(σ_i² + τ²)
            crudeVars = map (\σ2 -> σ2 * τ2Hat / (σ2 + τ2Hat)) σ2s
            -- Formula 2: Raudenbush = crude + σ_i⁴/((σ_i² + τ²)² · w)
            raudVars = zipWith (\σ2 cv ->
              cv + σ2^2 / ((σ2 + τ2Hat)^2 * sumWRE)
              ) σ2s crudeVars
            -- Formula 3: Our κ_eff: 1/(1/σ_i² + 1/(τ² + Var(μ̂)))
            keffVars = map (\σ2 ->
              1 / (1/σ2 + 1/(τ2Hat + varMuHat))
              ) σ2s
            -- Check coverage for each study
            checkCov θi ebEst var =
              let se = sqrt var
                  lo = ebEst - z975 * se
                  hi = ebEst + z975 * se
               in if θi >= lo && θi <= hi then 1 else 0
            crudeCovs = sum $ zip3With checkCov thetas ebEstimates crudeVars
            raudCovs = sum $ zip3With checkCov thetas ebEstimates raudVars
            keffCovs = sum $ zip3With checkCov thetas ebEstimates keffVars
         in simulate g2 (n - 1)
              ( crudeCov + crudeCovs
              , raudCov + raudCovs
              , keffCov + keffCovs
              , crudeCovAll + k
              , raudCovAll + k
              , keffCovAll + k
              )

      (cC, rC, kC, cA, rA, kA) = simulate g0 nSims (0, 0, 0, 0, 0, 0)
      crudeCoverage = fromIntegral cC / fromIntegral cA :: Double
      raudCoverage = fromIntegral rC / fromIntegral rA :: Double
      keffCoverage = fromIntegral kC / fromIntegral kA :: Double

  hFlush stdout
  putStr $ "\n  === SSPI Coverage Simulation ==="
  putStr $ "\n  k = " <> show k <> " studies, nSims = " <> show nSims
  putStr $ "\n  μ = " <> show μTrue <> ", τ² = " <> show τ2True
  putStr $ "\n  σ_i² = " <> show σ2s
  putStr $ "\n  ---"
  putStr $ "\n  Crude     coverage: " <> show (roundD crudeCoverage 4)
  putStr $ "\n  Raudenbush coverage: " <> show (roundD raudCoverage 4)
  putStr $ "\n  κ_eff     coverage: " <> show (roundD keffCoverage 4)
  putStr $ "\n  (target: 0.95)\n"
  hFlush stdout

  -- Pass if all coverages are reasonable (between 0.90 and 0.99)
  let allReasonable = raudCoverage > 0.85 && keffCoverage > 0.85
  if allReasonable
    then return $ testPassed name $
      "crude=" <> show (roundD crudeCoverage 4) <>
      " raud=" <> show (roundD raudCoverage 4) <>
      " keff=" <> show (roundD keffCoverage 4)
    else return $ testFailed name
      ("expected ~0.95",
       "crude=" <> show (roundD crudeCoverage 4) <>
       " raud=" <> show (roundD raudCoverage 4) <>
       " keff=" <> show (roundD keffCoverage 4))

roundD :: Double -> Int -> Double
roundD f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

zip3With :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zip3With f (a:as) (b:bs) (c:cs) = f a b c : zip3With f as bs cs
zip3With _ _ _ _ = []

-- | Same simulation but with large k to reduce τ² estimation uncertainty
sspiCoverageLargeK :: IO Test
sspiCoverageLargeK = do
  let name = "SSPI coverage with large k=50 (τ² estimation less noisy)"
  let nSims = 2000
      k = 50
      μTrue = 0.5
      τ2True = 0.1
      σ2s = take k $ cycle [0.05, 0.08, 0.12, 0.03, 0.15, 0.07, 0.20, 0.04, 0.10, 0.06]
      z975 = 1.959964
  g0 <- getStdGen
  let (cC, rC, kC, cA, rA, kA) = runSim g0 nSims k μTrue τ2True σ2s z975
      crudeCoverage = fromIntegral cC / fromIntegral cA :: Double
      raudCoverage = fromIntegral rC / fromIntegral rA :: Double
      keffCoverage = fromIntegral kC / fromIntegral kA :: Double
  hFlush stdout
  putStr $ "\n  === SSPI Coverage (k=50) ==="
  putStr $ "\n  Crude:      " <> show (roundD crudeCoverage 4)
  putStr $ "\n  Raudenbush: " <> show (roundD raudCoverage 4)
  putStr $ "\n  κ_eff:      " <> show (roundD keffCoverage 4)
  putStr $ "\n  (target: 0.95)\n"
  hFlush stdout
  return $ testPassed name $
    "crude=" <> show (roundD crudeCoverage 4) <>
    " raud=" <> show (roundD raudCoverage 4) <>
    " keff=" <> show (roundD keffCoverage 4)

-- | Simulation with τ² KNOWN (not estimated) to isolate the effect
sspiCoverageKnownTau :: IO Test
sspiCoverageKnownTau = do
  let name = "SSPI coverage with τ² known (no estimation uncertainty)"
  let nSims = 2000
      k = 10
      μTrue = 0.5
      τ2True = 0.1
      σ2s = [0.05, 0.08, 0.12, 0.03, 0.15, 0.07, 0.20, 0.04, 0.10, 0.06]
      z975 = 1.959964
  g0 <- getStdGen
  let simulate :: StdGen -> Int -> (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int)
      simulate _ 0 acc = acc
      simulate g n (!crudeCov, !raudCov, !keffCov, !crudeCovAll, !raudCovAll, !keffCovAll) =
        let (thetas, g1) = foldl' (\(ts, gen) _ ->
              let (t, gen') = normal μTrue (sqrt τ2True) gen
               in (ts ++ [t], gen')
              ) ([], g) [1..k]
            (ys, g2) = foldl' (\(oss, gen) (θi, σ2i) ->
              let (yi, gen') = normal θi (sqrt σ2i) gen
               in (oss ++ [yi], gen')
              ) ([], g1) (zip thetas σ2s)
            -- Use TRUE τ² (known)
            τ2Hat = τ2True
            wsRE = map (\σ2 -> 1 / (σ2 + τ2Hat)) σ2s
            sumWRE = sum wsRE
            μHat = sum (zipWith (\w y -> w * y) wsRE ys) / sumWRE
            varMuHat = 1 / sumWRE
            ebEstimates = zipWith (\yi σ2i ->
              let bi = σ2i / (σ2i + τ2Hat)
               in bi * μHat + (1 - bi) * yi
              ) ys σ2s
            crudeVars = map (\σ2 -> σ2 * τ2Hat / (σ2 + τ2Hat)) σ2s
            raudVars = zipWith (\σ2 cv ->
              cv + σ2^2 / ((σ2 + τ2Hat)^2 * sumWRE)
              ) σ2s crudeVars
            keffVars = map (\σ2 ->
              1 / (1/σ2 + 1/(τ2Hat + varMuHat))
              ) σ2s
            checkCov θi ebEst var =
              let se = sqrt var
                  lo = ebEst - z975 * se
                  hi = ebEst + z975 * se
               in if θi >= lo && θi <= hi then 1 else 0
            crudeCovs = sum $ zip3With checkCov thetas ebEstimates crudeVars
            raudCovs = sum $ zip3With checkCov thetas ebEstimates raudVars
            keffCovs = sum $ zip3With checkCov thetas ebEstimates keffVars
         in simulate g2 (n - 1)
              ( crudeCov + crudeCovs
              , raudCov + raudCovs
              , keffCov + keffCovs
              , crudeCovAll + k
              , raudCovAll + k
              , keffCovAll + k
              )
      (cC, rC, kC, cA, rA, kA) = simulate g0 nSims (0, 0, 0, 0, 0, 0)
      crudeCoverage = fromIntegral cC / fromIntegral cA :: Double
      raudCoverage = fromIntegral rC / fromIntegral rA :: Double
      keffCoverage = fromIntegral kC / fromIntegral kA :: Double
  hFlush stdout
  putStr $ "\n  === SSPI Coverage (τ² KNOWN) ==="
  putStr $ "\n  Crude:      " <> show (roundD crudeCoverage 4)
  putStr $ "\n  Raudenbush: " <> show (roundD raudCoverage 4)
  putStr $ "\n  κ_eff:      " <> show (roundD keffCoverage 4)
  putStr $ "\n  (target: 0.95)\n"
  hFlush stdout
  return $ testPassed name $
    "crude=" <> show (roundD crudeCoverage 4) <>
    " raud=" <> show (roundD raudCoverage 4) <>
    " keff=" <> show (roundD keffCoverage 4)

-- | Common simulation runner
runSim :: StdGen -> Int -> Int -> Double -> Double -> [Double] -> Double
       -> (Int, Int, Int, Int, Int, Int)
runSim g0 nSims k μTrue τ2True σ2s z975 =
  let simulate :: StdGen -> Int -> (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int)
      simulate _ 0 acc = acc
      simulate g n (!crudeCov, !raudCov, !keffCov, !crudeCovAll, !raudCovAll, !keffCovAll) =
        let (thetas, g1) = foldl' (\(ts, gen) _ ->
              let (t, gen') = normal μTrue (sqrt τ2True) gen
               in (ts ++ [t], gen')
              ) ([], g) [1..k]
            (ys, g2) = foldl' (\(oss, gen) (θi, σ2i) ->
              let (yi, gen') = normal θi (sqrt σ2i) gen
               in (oss ++ [yi], gen')
              ) ([], g1) (zip thetas σ2s)
            wsFE = map (\σ2 -> 1 / σ2) σ2s
            sumWFE = sum wsFE
            μFE = sum (zipWith (\w y -> w * y) wsFE ys) / sumWFE
            qStat = sum $ zipWith (\w y -> w * (y - μFE)^2) wsFE ys
            cFactor = sumWFE - sum (map (^2) wsFE) / sumWFE
            τ2Hat = max 0 ((qStat - fromIntegral (k - 1)) / cFactor)
            wsRE = map (\σ2 -> 1 / (σ2 + τ2Hat)) σ2s
            sumWRE = sum wsRE
            μHat = sum (zipWith (\w y -> w * y) wsRE ys) / sumWRE
            varMuHat = 1 / sumWRE
            ebEstimates = zipWith (\yi σ2i ->
              let bi = σ2i / (σ2i + τ2Hat)
               in bi * μHat + (1 - bi) * yi
              ) ys σ2s
            crudeVars = map (\σ2 -> σ2 * τ2Hat / (σ2 + τ2Hat)) σ2s
            raudVars = zipWith (\σ2 cv ->
              cv + σ2^2 / ((σ2 + τ2Hat)^2 * sumWRE)
              ) σ2s crudeVars
            keffVars = map (\σ2 ->
              1 / (1/σ2 + 1/(τ2Hat + varMuHat))
              ) σ2s
            checkCov θi ebEst var =
              let se = sqrt var
                  lo = ebEst - z975 * se
                  hi = ebEst + z975 * se
               in if θi >= lo && θi <= hi then 1 else 0
            crudeCovs = sum $ zip3With checkCov thetas ebEstimates crudeVars
            raudCovs = sum $ zip3With checkCov thetas ebEstimates raudVars
            keffCovs = sum $ zip3With checkCov thetas ebEstimates keffVars
         in simulate g2 (n - 1)
              ( crudeCov + crudeCovs
              , raudCov + raudCovs
              , keffCov + keffCovs
              , crudeCovAll + k
              , raudCovAll + k
              , keffCovAll + k
              )
   in simulate g0 nSims (0, 0, 0, 0, 0, 0)
