{-# LANGUAGE BangPatterns #-}
-- | Spring-REML validation tests.
--
-- Pairwise tests compare spring-REML against textbook Fisher-scoring REML
-- (both should give the same τ² for 2-arm studies).
--
-- NMA tests compare against R netmeta::netmeta(… method = "REML") on
-- specific datasets (contNoMultiarm, diabetes, smokingcessation).
--
-- Synthetic tests validate against netmeta REML on 21 generated NMA
-- datasets (10 continuous MD, 10 binary OR, plus senn2013).
-- Reference τ² values are from test/nma_reference_tau2.json.
module Test.Meta.REMLValidation where

import Data.Either
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe

import TestHS
import Data.Numerics
import Data.Meta.Effects
import Data.Meta.NMA
import Data.Meta.RandomEffects
import Data.Meta.Studies

ioTests :: [IO Test]
ioTests =
  [ pairwiseREMLvsSpringREML
  , pairwiseViecht1
  , pairwiseCocalc
  , nmaContNoMultiarm
  , nmaDiabetes
  , nmaSmokingCessation
  ] ++ synthTests

-- | Textbook pairwise REML using Fisher scoring.
--
-- The REML profile log-likelihood for y_i ~ N(μ, σ_i² + τ²):
--   ℓ_R(τ²) = -½ [Σ log(σ_i² + τ²) + log(Σ w_i*) + Q*]
--
-- Score (derivative w.r.t. τ²):
--   U = -½ tr(P) + ½ Σ w_i*² r_i²
--
-- where w_i* = 1/(σ_i² + τ²), μ̂ = Σw_i*y_i / Σw_i*,
--       r_i = y_i - μ̂, P = W - W11'W/(1'W1)
--
-- Fisher information:
--   I = ½ tr(P²) = ½ [Σw² - 2Σw³/Σw + (Σw²)²/(Σw)²]
--
-- Fisher scoring update:
--   τ²_{n+1} = τ²_n + U/I
textbookREML
  :: [(Double, Double)]  -- ^ (y_i, σ_i²) pairs
  -> Double              -- ^ initial τ² guess
  -> Double              -- ^ epsilon convergence
  -> Int                 -- ^ max iterations
  -> (Double, [Double])  -- ^ (τ², iteration path)
textbookREML yvs tau0 eps maxiter = go tau0 [] 0
  where
    go tau path iter
      | iter >= maxiter = (tau, reverse (tau:path))
      | abs step < eps  = (tau', reverse (tau':path))
      | otherwise       = go tau' (tau:path) (iter + 1)
      where
        ws  = map (\(_, v) -> 1 / (v + tau)) yvs
        sw  = sum ws
        sw2 = sum $ map (^2) ws
        sw3 = sum $ map (^3) ws
        mu  = sum (zipWith (\w (y,_) -> w * y) ws yvs) / sw
        rs  = map (\(y, _) -> y - mu) yvs
        -- Score = -½ tr(P) + ½ Σ w_i² r_i²
        trP  = sw - sw2 / sw
        wr2  = sum $ zipWith (\w r -> w^2 * r^2) ws rs
        score = (-0.5) * trP + 0.5 * wr2
        -- Fisher info = ½ tr(P²)
        trP2 = sw2 - 2 * sw3 / sw + sw2^2 / sw^2
        info = 0.5 * trP2
        step = score / info
        tau' = max 0 (tau + step)

-- | Extract pairwise effects from studies for textbook REML.
-- Only works for pairwise (2-arm) studies.
studiesToPairwiseEffects
  :: (ArmEffect a b, Gaussian a, Gaussian b)
  => (Arm -> Either String a)
  -> [Study]
  -> Either String [(Double, Double)]  -- (y_i, σ_i²)
studiesToPairwiseEffects armEffect studies =
  sequence $ map (\study ->
    let arms = getStudyArms study
     in case arms of
          [a, b] ->
            case (armEffect a, armEffect b) of
              (Right ea, Right eb) ->
                let eff = relatedRelative ea
                    effb = relatedRelative eb
                    d = expectation eff - expectation effb
                    v = variance eff + variance effb
                 in Right (d, v)
              (Left err, _) -> Left err
              (_, Left err) -> Left err
          _ -> Left "Not a 2-arm study"
  ) studies

-- | Test: spring-REML matches textbook REML on pairwise continuous data
pairwiseREMLvsSpringREML :: IO Test
pairwiseREMLvsSpringREML = do
  let name = "spring-REML vs textbook REML: pairwise continuous"
  let studiesFile = "test/continuous.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error reading studies", err)
    Right studies -> do
      -- Textbook REML
      let eyvs = studiesToPairwiseEffects meanArm studies
      case eyvs of
        Left err -> return $ testFailed name ("Error computing effects", err)
        Right yvs -> do
          let (textbookTau2, textbookPath) = textbookREML yvs 0.5 1e-8 1000

          -- Spring-REML
          let espringResult = springREML studies meanArm Nothing (Just 1000) (Just 1e-8)
          case espringResult of
            Left err -> return $ testFailed name ("spring-REML error", err)
            Right (netes, (springTau2, springPath)) -> do
              let diff = abs (textbookTau2 - springTau2)
                  tol = 1e-3
                  spnet = springNetwork netes
                  qτ = qqTau spnet
                  dofs = fromIntegral $ dof spnet

              putStr $ "\n  === Pairwise REML comparison (continuous) ==="
              putStr $ "\n  Textbook REML τ²: " <> show (roundDouble textbookTau2 6)
              putStr $ "\n  Spring-REML  τ²: " <> show (roundDouble springTau2 6)
              putStr $ "\n  Difference:      " <> show (roundDouble diff 8)
              putStr $ "\n  Qτ at solution:  " <> show (roundDouble qτ 6)
              putStr $ "\n  dof:             " <> show dofs
              putStr $ "\n  Qτ - dof:        " <> show (roundDouble (qτ - dofs) 8)
              putStr $ "\n  Textbook path:   " <> show (map (flip roundDouble 4) textbookPath)
              putStr $ "\n  Spring path:     " <> show (map (flip roundDouble 4) springPath)
              putStr $ "\n"

              if diff < tol
                then return $ testPassed name $
                  "τ² match! textbook=" <> show (roundDouble textbookTau2 4) <>
                  " spring=" <> show (roundDouble springTau2 4)
                else return $ testFailed name
                  ( "textbook τ²=" <> show textbookTau2
                  , "spring τ²=" <> show springTau2 <> " diff=" <> show diff
                  )

-- | Pairwise: viecht1 (REML τ² = 0.198784)
pairwiseViecht1 :: IO Test
pairwiseViecht1 = do
  let name = "spring-REML pairwise: viecht1"
  estudies <- readStudies "test/viecht1.json"
  case estudies of
    Left err -> return $ testFailed name ("Error", err)
    Right studies -> do
      let espringResult = springREML studies meanArm Nothing (Just 1000) (Just 1e-8)
      case espringResult of
        Left err -> return $ testFailed name ("spring-REML error", err)
        Right (_, (springTau2, _)) -> do
          let expected = 0.198784
              diff = abs (expected - springTau2)
          putStr $ "\n  viecht1: spring τ²=" <> show (roundDouble springTau2 6)
                <> " expected=" <> show expected <> " diff=" <> show (roundDouble diff 8)
          if diff < 1e-3
            then return $ testPassed name $ show (roundDouble springTau2 6)
            else return $ testFailed name ("expected " <> show expected, "got " <> show springTau2)

-- | Pairwise: cocalc (REML τ² = 0.515392)
pairwiseCocalc :: IO Test
pairwiseCocalc = do
  let name = "spring-REML pairwise: cocalc"
  estudies <- readStudies "test/cocalc.json"
  case estudies of
    Left err -> return $ testFailed name ("Error", err)
    Right studies -> do
      let espringResult = springREML studies meanArm Nothing (Just 1000) (Just 1e-8)
      case espringResult of
        Left err -> return $ testFailed name ("spring-REML error", err)
        Right (_, (springTau2, _)) -> do
          let expected = 0.515392
              diff = abs (expected - springTau2)
          putStr $ "\n  cocalc: spring τ²=" <> show (roundDouble springTau2 6)
                <> " expected=" <> show expected <> " diff=" <> show (roundDouble diff 8)
          if diff < 1e-3
            then return $ testPassed name $ show (roundDouble springTau2 6)
            else return $ testFailed name ("expected " <> show expected, "got " <> show springTau2)

-- | NMA test: contNoMultiarm (continuous, no multiarm studies, 6 treatments)
-- netmeta REML gives τ² = 0.0002344181
nmaContNoMultiarm :: IO Test
nmaContNoMultiarm = do
  let name = "spring-REML NMA: contNoMultiarm (6 treatments, no multiarm)"
  let studiesFile = "test/contNoMultiarm.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error reading studies", err)
    Right studies -> do
      let espringResult = springREML studies meanArm Nothing (Just 1000) (Just 1e-8)
      case espringResult of
        Left err -> return $ testFailed name ("spring-REML error", err)
        Right (netes, (springTau2, springPath)) -> do
          let netmetaREML = 0.0002344181  -- from R netmeta
              diff = abs (netmetaREML - springTau2)
              tol = 1e-4
              spnet = springNetwork netes

          putStr $ "\n  === NMA: contNoMultiarm ==="
          putStr $ "\n  netmeta REML τ²: " <> show netmetaREML
          putStr $ "\n  Spring-REML  τ²: " <> show (roundDouble springTau2 8)
          putStr $ "\n  Difference:      " <> show (roundDouble diff 8)
          putStr $ "\n  dof:             " <> show (dof spnet)
          putStr $ "\n  Spring path (last 5): " <> show (map (flip roundDouble 6) (reverse $ take 5 $ reverse springPath))
          putStr $ "\n"

          if diff < tol
            then return $ testPassed name $
              "τ² match! netmeta=" <> show netmetaREML <>
              " spring=" <> show (roundDouble springTau2 6)
            else return $ testFailed name
              ( "netmeta τ²=" <> show netmetaREML
              , "spring τ²=" <> show springTau2 <> " diff=" <> show diff
              )

-- | NMA test: smokingcessation (binary logOR, 4 treatments, 24 studies, multiarm)
-- netmeta REML gives τ² = 0.4502296
nmaSmokingCessation :: IO Test
nmaSmokingCessation = do
  let name = "spring-REML NMA: smokingcessation (4 treats, multiarm, high τ²)"
  let studiesFile = "test/smokingcessation.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error reading studies", err)
    Right studies -> do
      let espringResult = springREML studies logOdds Nothing (Just 1000) (Just 1e-8)
      case espringResult of
        Left err -> return $ testFailed name ("spring-REML error", err)
        Right (netes, (springTau2, springPath)) -> do
          let netmetaREML = 0.4502296
              diff = abs (netmetaREML - springTau2)
              tol = 0.05
              spnet = springNetwork netes

          putStr $ "\n  === NMA: smokingcessation (multiarm, high τ²) ==="
          putStr $ "\n  netmeta REML τ²: " <> show netmetaREML
          putStr $ "\n  Spring-REML  τ²: " <> show (roundDouble springTau2 6)
          putStr $ "\n  Difference:      " <> show (roundDouble diff 6)
          putStr $ "\n  dof:             " <> show (dof spnet)
          putStr $ "\n  Spring path (last 5): " <> show (map (flip roundDouble 4) (reverse $ take 5 $ reverse springPath))
          -- Contrast prediction intervals (new study)
          let cpiVars = contrastPredictionVariances netes
          putStr $ "\n  --- Contrast Prediction Interval SEs (new study) ---"
          mapM_ (\(a, inner) ->
            mapM_ (\(b, v) ->
              if a < b
                then putStr $ "\n    " <> show a <> " vs " <> show b
                      <> ": seTE.predict=" <> show (roundDouble (sqrt v) 4)
                else return ()
              ) $ Map.toList inner
            ) $ Map.toList cpiVars
          -- Network variance (for comparison)
          let nvars = networkVariances netes
          putStr $ "\n  --- Network Variances (seTE.random) ---"
          mapM_ (\(a, inner) ->
            mapM_ (\(b, v) ->
              if a < b
                then putStr $ "\n    " <> show a <> " vs " <> show b
                      <> ": seTE.random=" <> show (roundDouble (sqrt v) 4)
                else return ()
              ) $ Map.toList inner
            ) $ Map.toList nvars
          -- Study-specific prediction intervals
          let spiVars = studyPredictionVariance spnet
          putStr $ "\n  --- Study Prediction Interval Variances (1/κ_i) ---"
          mapM_ (\(sv, v) ->
            putStr $ "\n    study " <> show sv <> ": " <> show (roundDouble v 4)
            ) $ Map.toList spiVars
          putStr $ "\n  I²_BLUP: " <> show (roundDouble (i2BLUP netes) 4)
          putStr $ "\n"

          if diff < tol
            then return $ testPassed name $
              "τ² close! netmeta=" <> show netmetaREML <>
              " spring=" <> show (roundDouble springTau2 4)
            else return $ testFailed name
              ( "netmeta τ²=" <> show netmetaREML
              , "spring τ²=" <> show springTau2 <> " diff=" <> show diff
              )

-- | NMA test: diabetes (binary logOR, multiarm studies, 6 treatments)
-- netmeta REML gives τ² = 0.0136471
nmaDiabetes :: IO Test
nmaDiabetes = do
  let name = "spring-REML NMA: diabetes (6 treatments, multiarm)"
  let studiesFile = "test/diabetes.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error reading studies", err)
    Right studies -> do
      let espringResult = springREML studies logOdds Nothing (Just 1000) (Just 1e-8)
      case espringResult of
        Left err -> return $ testFailed name ("spring-REML error", err)
        Right (netes, (springTau2, springPath)) -> do
          let netmetaREML = 0.0136471  -- from R netmeta
              diff = abs (netmetaREML - springTau2)
              tol = 1e-3
              spnet = springNetwork netes

          putStr $ "\n  === NMA: diabetes (multiarm) ==="
          putStr $ "\n  netmeta REML τ²: " <> show netmetaREML
          putStr $ "\n  Spring-REML  τ²: " <> show (roundDouble springTau2 8)
          putStr $ "\n  Difference:      " <> show (roundDouble diff 8)
          putStr $ "\n  dof:             " <> show (dof spnet)
          putStr $ "\n  Spring path (last 5): " <> show (map (flip roundDouble 6) (reverse $ take 5 $ reverse springPath))
          putStr $ "\n"

          if diff < tol
            then return $ testPassed name $
              "τ² match! netmeta=" <> show netmetaREML <>
              " spring=" <> show (roundDouble springTau2 6)
            else return $ testFailed name
              ( "netmeta τ²=" <> show netmetaREML
              , "spring τ²=" <> show springTau2 <> " diff=" <> show diff
              )

-- | Generic test: run springREML on a dataset and compare τ² against netmeta.
synthREMLTest :: String -> String -> Double -> String -> IO Test
synthREMLTest label file expected sm = do
  let name = "synth: " <> label
  estudies <- readStudies file
  case estudies of
    Left err -> return $ testFailed name ("Error reading studies", err)
    Right studies -> do
      let eresult = if sm == "OR"
            then springREML studies logOdds Nothing (Just 1000) (Just 1e-6)
            else springREML studies meanArm Nothing (Just 1000) (Just 1e-6)
      case eresult of
        Left err -> return $ testFailed name ("spring-REML error", err)
        Right (_, (tau2, _)) -> do
          let diff = abs (expected - tau2)
              ratio = if expected > 1e-6 then tau2 / expected else 0
          if diff < 1e-3 || abs (ratio - 1) < 0.01
            then return $ testPassed name $
              "τ²=" <> show (roundDouble tau2 6) <>
              " expected=" <> show expected
            else return $ testFailed name
              ( "expected τ²=" <> show expected
              , "got τ²=" <> show tau2 <> " ratio=" <> show (roundDouble ratio 4)
              )

synthTests :: [IO Test]
synthTests =
  [ synthREMLTest "senn2013"  "test/nma_senn2013.json"  0.0999 "MD"
  , synthREMLTest "synth_c1"  "test/nma_synth_c1.json"  1.3515 "MD"
  , synthREMLTest "synth_c2"  "test/nma_synth_c2.json"  0.5231 "MD"
  , synthREMLTest "synth_c3"  "test/nma_synth_c3.json"  2.0222 "MD"
  , synthREMLTest "synth_c4"  "test/nma_synth_c4.json"  0.4249 "MD"
  , synthREMLTest "synth_c5"  "test/nma_synth_c5.json"  0.0282 "MD"
  , synthREMLTest "synth_c6"  "test/nma_synth_c6.json"  0.8585 "MD"
  , synthREMLTest "synth_c7"  "test/nma_synth_c7.json"  1.8589 "MD"
  , synthREMLTest "synth_c8"  "test/nma_synth_c8.json"  0.0101 "MD"
  , synthREMLTest "synth_c9"  "test/nma_synth_c9.json"  0.4085 "MD"
  , synthREMLTest "synth_c10" "test/nma_synth_c10.json" 0.2986 "MD"
  , synthREMLTest "synth_b1"  "test/nma_synth_b1.json"  1.5682 "OR"
  , synthREMLTest "synth_b2"  "test/nma_synth_b2.json"  0.7149 "OR"
  , synthREMLTest "synth_b3"  "test/nma_synth_b3.json"  0.4071 "OR"
  , synthREMLTest "synth_b4"  "test/nma_synth_b4.json"  0.5408 "OR"
  , synthREMLTest "synth_b5"  "test/nma_synth_b5.json"  0.8122 "OR"
  , synthREMLTest "synth_b6"  "test/nma_synth_b6.json"  0.2016 "OR"
  , synthREMLTest "synth_b7"  "test/nma_synth_b7.json"  0.5492 "OR"
  , synthREMLTest "synth_b8"  "test/nma_synth_b8.json"  0.3307 "OR"
  , synthREMLTest "synth_b9"  "test/nma_synth_b9.json"  1.5337 "OR"
  , synthREMLTest "synth_b10" "test/nma_synth_b10.json" 0.2463 "OR"
  ]
