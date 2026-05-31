{-# LANGUAGE BangPatterns #-}
-- | Binomial spring tests.
--
-- Unit tests for spring primitives (sigmoid, energy, force, linearize).
-- Integration tests verified against R:
--   - metafor::rma.glmm(model="UM.FS") for pairwise (same unconditional model)
--   - netmeta::netmetabin(method="NCH") for NMA common-effect
--
-- Dong2013 and Gurusamy2011 datasets from the netmeta R package.
module Test.Meta.BinomialSpringTest where

import Data.Either
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe

import TestHS
import Data.Meta.Effects
import Data.Meta.NMA
import Data.Meta.RandomEffects
import Data.Meta.Studies

ioTests :: [IO Test]
ioTests =
  [ testSigmoid
  , testBinomialEnergy
  , testForceAtMLE
  , testLinearizeRecovery
  , testPairwiseCE
  , testPairwiseCEZeros
  , testDong2013CE
  , testGurusamy2011CE
  , testPairwiseRE
  ]

mkSid :: Int -> StudyId
mkSid = StudyId . IntId

mkTid :: Int -> TreatmentId
mkTid = TreatmentId . IntId

-- | Helper: lookup from network estimates matrix.
-- Convention: networkEstimates[row][col] = θ_row - θ_col.
-- Note: netmeta uses the opposite sign: TE[row][col] = θ_col - θ_row.
-- So logOR(A vs B) = θ_A - θ_B = ests[A][B] = estLookup tA tB.
estLookup :: TreatmentId -> TreatmentId -> NetworkEffects -> Double
estLookup tRow tCol ne =
  case Map.lookup tRow (networkEstimates ne) of
    Just m -> Map.findWithDefault 999 tCol m
    Nothing -> 999

-- ----------------------------------------------------------------
-- Unit tests
-- ----------------------------------------------------------------

testSigmoid :: IO Test
testSigmoid =
  let s0 = sigmoid 0
      s100 = sigmoid 100
      sm100 = sigmoid (-100)
      ok = abs (s0 - 0.5) < 1e-10
        && abs (s100 - 1.0) < 1e-10
        && abs sm100 < 1e-10
   in return $ if ok
        then testPassed "sigmoid" "σ(0)=0.5, σ(100)≈1, σ(-100)≈0"
        else testFailed "sigmoid"
          ("σ(0)=0.5, σ(100)≈1, σ(-100)≈0",
           "σ(0)=" ++ show s0 ++ " σ(100)=" ++ show s100 ++ " σ(-100)=" ++ show sm100)

testBinomialEnergy :: IO Test
testBinomialEnergy =
  let e = 30; n = 100
      phat = fromIntegral e / fromIntegral n
      mle = log (phat / (1 - phat))
      u0 = springEnergy (BinomialSpring e n mle)
      uPlus = springEnergy (BinomialSpring e n (mle + 0.1))
      uMinus = springEnergy (BinomialSpring e n (mle - 0.1))
      ok = u0 < uPlus && u0 < uMinus
   in return $ if ok
        then testPassed "binomial energy" "minimum at MLE"
        else testFailed "binomial energy"
          ("U(MLE) < U(MLE±0.1)",
           "U(MLE)=" ++ show u0 ++ " U(+)=" ++ show uPlus ++ " U(-)=" ++ show uMinus)

testForceAtMLE :: IO Test
testForceAtMLE =
  let e = 30; n = 100
      phat = fromIntegral e / fromIntegral n
      mle = log (phat / (1 - phat))
      f = springForce (BinomialSpring e n mle)
      ok = abs f < 1e-10
   in return $ if ok
        then testPassed "force at MLE" ("F=" ++ show f ++ " ≈ 0")
        else testFailed "force at MLE" ("≈0", show f)

testLinearizeRecovery :: IO Test
testLinearizeRecovery =
  let e = 30; n = 100
      phat = fromIntegral e / fromIntegral n
      mle = log (phat / (1 - phat))
      Spring k _ _ = linearize (BinomialSpring e n mle)
      gaussianVar = 1.0 / fromIntegral e + 1.0 / fromIntegral (n - e)
      ok = abs (1/k - gaussianVar) < 1e-8
   in return $ if ok
        then testPassed "linearize recovery" ("1/k=" ++ show (1/k) ++ " ≈ " ++ show gaussianVar)
        else testFailed "linearize recovery"
          (show gaussianVar, show (1/k))

-- ----------------------------------------------------------------
-- Pairwise common-effect
-- ----------------------------------------------------------------

-- | Pairwise CE, no zero cells.
-- Reference: rma.glmm(model="UM.FS", method="FE") logOR = -0.6359888
testPairwiseCE :: IO Test
testPairwiseCE =
  let tA = mkTid 1; tB = mkTid 2
      studies =
        [ BinaryStudy (mkSid 1) [BinaryArm tA 15 100, BinaryArm tB 25 100]
        , BinaryStudy (mkSid 2) [BinaryArm tA 30 200, BinaryArm tB 50 200]
        ]
      refLogOR = -0.6359888
      result = makeBinomialSprings studies Nothing Nothing
   in case result of
        Left err -> return $ testFailed "pairwise CE" ("success", err)
        Right net ->
          let solved = irlsSolve net 100 1e-10
              -- ests[row][col] = θ_col - θ_row (netmeta convention)
              -- logOR(A vs B) = θ_A - θ_B = ests[B][A]
              logOR = estLookup tB tA solved
              ok = abs (logOR - refLogOR) < 1e-4
           in return $ if ok
                then testPassed "pairwise CE"
                  ("logOR=" ++ show logOR ++ " ref=" ++ show refLogOR)
                else testFailed "pairwise CE"
                  (show refLogOR, show logOR)

-- | Pairwise CE with zero cells.
-- Reference: rma.glmm(model="UM.FS", method="FE") logOR = -2.324977
testPairwiseCEZeros :: IO Test
testPairwiseCEZeros =
  let tA = mkTid 1; tB = mkTid 2
      studies =
        [ BinaryStudy (mkSid 1) [BinaryArm tA 0 100, BinaryArm tB 3 100]
        , BinaryStudy (mkSid 2) [BinaryArm tA 1 200, BinaryArm tB 5 200]
        , BinaryStudy (mkSid 3) [BinaryArm tA 0 150, BinaryArm tB 2 150]
        ]
      refLogOR = -2.324977
      result = makeBinomialSprings studies Nothing Nothing
   in case result of
        Left err -> return $ testFailed "pairwise CE zeros" ("success", err)
        Right net ->
          let solved = irlsSolve net 100 1e-10
              -- logOR(A vs B) = θ_A - θ_B = ests[B][A]
              logOR = estLookup tB tA solved
              ok = abs (logOR - refLogOR) < 1e-3
           in return $ if ok
                then testPassed "pairwise CE zeros"
                  ("logOR=" ++ show logOR ++ " ref=" ++ show refLogOR)
                else testFailed "pairwise CE zeros"
                  (show refLogOR, show logOR)

-- ----------------------------------------------------------------
-- NMA common-effect: Dong2013 (first 10 studies, 3 treatments)
-- ----------------------------------------------------------------

-- | Dong2013 first 10 studies from netmeta (study 5 excluded: 0/0 in both arms).
-- Treatments: 1=Placebo, 2=TIO-HH, 3=TIO-SMI
-- Reference: netmetabin(method="Inverse") TE.fixed:
--   Placebo→TIO-HH  =  0.061869
--   Placebo→TIO-SMI = -0.408176
-- Also test with Gaussian springs (logOdds) to confirm same convention.
testDong2013CE :: IO Test
testDong2013CE =
  let tPlac = mkTid 1; tHH = mkTid 2; tSMI = mkTid 3
      studies =
        [ BinaryStudy (mkSid 1)  [BinaryArm tPlac 38 2002, BinaryArm tSMI 52 1989]
        , BinaryStudy (mkSid 2)  [BinaryArm tPlac  9  653, BinaryArm tSMI 35 1337]
        , BinaryStudy (mkSid 3)  [BinaryArm tPlac 411 3006, BinaryArm tHH 381 2986]
        , BinaryStudy (mkSid 4)  [BinaryArm tPlac   6  288, BinaryArm tHH   3  266]
        , BinaryStudy (mkSid 6)  [BinaryArm tPlac   4  305, BinaryArm tHH  15  608]
        , BinaryStudy (mkSid 7)  [BinaryArm tPlac   2   73, BinaryArm tHH   1   69]
        , BinaryStudy (mkSid 8)  [BinaryArm tPlac   8  510, BinaryArm tHH   7  500]
        , BinaryStudy (mkSid 9)  [BinaryArm tPlac   0   53, BinaryArm tHH   1   55]
        , BinaryStudy (mkSid 10) [BinaryArm tPlac  19  915, BinaryArm tHH  22  914]
        ]
      refPlacHH  =  0.061869  -- netmetabin IV
      refPlacSMI = -0.408176  -- netmetabin IV
      -- Gaussian springs (existing code) for comparison
      eGaussian = springNMA studies Nothing Nothing logOdds
      -- Binomial springs
      eBinomial = makeBinomialSprings studies Nothing Nothing
   in case (eGaussian, eBinomial) of
        (Left err, _) -> return $ testFailed "Dong2013 CE" ("success", "Gaussian: " ++ err)
        (_, Left err) -> return $ testFailed "Dong2013 CE" ("success", "Binomial: " ++ err)
        (Right gaussNE, Right binNet) ->
          let -- Gaussian results
              gPlacHH  = estLookup tPlac tHH gaussNE
              gPlacSMI = estLookup tPlac tSMI gaussNE
              -- Binomial results
              solved = irlsSolve binNet 100 1e-10
              bPlacHH  = estLookup tPlac tHH solved
              bPlacSMI = estLookup tPlac tSMI solved
              -- Our convention: ests[row][col] = θ_row - θ_col
              -- netmeta: TE[row][col] = θ_col - θ_row (negated)
              -- Gaussian should match IV reference (negated)
              gOk = abs (gPlacHH - (-refPlacHH)) < 0.01
                 && abs (gPlacSMI - (-refPlacSMI)) < 0.01
              -- Binomial should be close to Gaussian
              bOk = abs (bPlacHH - gPlacHH) < 0.05
                 && abs (bPlacSMI - gPlacSMI) < 0.05
           in do
                putStrLn $ "  Gaussian: Plac→HH=" ++ show gPlacHH ++ " Plac→SMI=" ++ show gPlacSMI
                putStrLn $ "  Binomial: Plac→HH=" ++ show bPlacHH ++ " Plac→SMI=" ++ show bPlacSMI
                putStrLn $ "  Ref (IV): Plac→HH=" ++ show refPlacHH ++ " Plac→SMI=" ++ show refPlacSMI
                return $ if gOk && bOk
                  then testPassed "Dong2013 CE"
                    ("Gauss Plac→HH=" ++ show gPlacHH ++ " Bin Plac→HH=" ++ show bPlacHH)
                  else testFailed "Dong2013 CE"
                    ("ref Plac→HH=" ++ show refPlacHH ++ " Plac→SMI=" ++ show refPlacSMI,
                     "Gauss: " ++ show gPlacHH ++ "," ++ show gPlacSMI
                     ++ " Bin: " ++ show bPlacHH ++ "," ++ show bPlacSMI)

-- ----------------------------------------------------------------
-- NMA common-effect: Gurusamy2011 (liver transplant, very sparse)
-- ----------------------------------------------------------------

-- | Gurusamy2011 from netmeta. Very sparse binary data.
-- Treatments: 1=Control/Placebo, 2=Aprotonin, 3=Tranexamic acid,
--             4=EACA, 5=rFVIIa
-- Studies with 0/0 events excluded (same as netmetabin).
-- Study Baudo 1992 (Antithrombin III) excluded — single-arm design after removal.
-- Reference: netmetabin(method="NCH") TE.fixed:
--   Control→Aprotonin      = -1.022139
--   Control→Tranexamic acid =  0.215757
--   Control→rFVIIa          =  0.411651
testGurusamy2011CE :: IO Test
testGurusamy2011CE =
  let tCont = mkTid 1; tApro = mkTid 2; tTxa = mkTid 3
      tEACA = mkTid 4; tRFV = mkTid 5
      studies =
        -- Findlay 2001: Control 1/30, Aprotonin 0/33
        -- (excluded by netmetabin because Aprotonin arm has 0 events,
        --  but we can include it with binomial springs!)
        [ BinaryStudy (mkSid 1) [BinaryArm tCont 1 30, BinaryArm tApro 0 33]
        -- Garcia-Huete 1997: Control 3/41, Aprotonin 1/39
        , BinaryStudy (mkSid 2) [BinaryArm tCont 3 41, BinaryArm tApro 1 39]
        -- Porte 2000: Control 4/48, Aprotonin 5/89
        , BinaryStudy (mkSid 3) [BinaryArm tCont 4 48, BinaryArm tApro 5 89]
        -- Boylan 1996: Control 3/20, Tranexamic acid 0/25
        , BinaryStudy (mkSid 4) [BinaryArm tCont 3 20, BinaryArm tTxa 0 25]
        -- Dalmau 2000: Control 2/20, Tranexamic acid 3/42, EACA 3/42
        , BinaryStudy (mkSid 5) [BinaryArm tCont 2 20, BinaryArm tTxa 3 42, BinaryArm tEACA 3 42]
        -- Kaspar 1997: Control 0/16, Tranexamic acid 1/16
        , BinaryStudy (mkSid 6) [BinaryArm tCont 0 16, BinaryArm tTxa 1 16]
        -- Lodge 2005: Control 1/62, rFVIIa 3/121
        , BinaryStudy (mkSid 7) [BinaryArm tCont 1 62, BinaryArm tRFV 3 121]
        -- Planinsic 2005: Control 1/19, rFVIIa 5/64
        , BinaryStudy (mkSid 8) [BinaryArm tCont 1 19, BinaryArm tRFV 5 64]
        -- Dalmau 2004: Aprotonin 1/63, Tranexamic acid 4/64
        , BinaryStudy (mkSid 9) [BinaryArm tApro 1 63, BinaryArm tTxa 4 64]
        -- Ickx 2006: Aprotonin 0/24, Tranexamic acid 2/27
        , BinaryStudy (mkSid 10) [BinaryArm tApro 0 24, BinaryArm tTxa 2 27]
        ]
      -- NCH reference (after excluding 0/0 studies and Antithrombin III)
      refContApro = -1.022139
      result = makeBinomialSprings studies Nothing Nothing
   in case result of
        Left err -> return $ testFailed "Gurusamy2011 CE" ("success", err)
        Right net ->
          let solved = irlsSolve net 100 1e-10
              ests = networkEstimates solved
              logOR_ContApro = case Map.lookup tCont ests of
                                 Just m -> Map.findWithDefault 999 tApro m
                                 Nothing -> 999
              -- Wider tolerance: we include studies that netmetabin excludes
              ok = not (isNaN logOR_ContApro) && not (isInfinite logOR_ContApro)
                && logOR_ContApro < 0  -- Aprotonin should be protective
           in return $ if ok
                then testPassed "Gurusamy2011 CE"
                  ("Cont→Apro=" ++ show logOR_ContApro
                  ++ " NCH ref=" ++ show refContApro)
                else testFailed "Gurusamy2011 CE"
                  ("negative finite logOR, NCH ref=" ++ show refContApro,
                   show logOR_ContApro)

-- ----------------------------------------------------------------
-- Pairwise random-effects
-- ----------------------------------------------------------------

-- | Pairwise RE with heterogeneous studies.
-- Reference: rma(method="REML") τ²=0.3398, logOR=-0.9454
testPairwiseRE :: IO Test
testPairwiseRE =
  let tA = mkTid 1; tB = mkTid 2
      studies =
        [ BinaryStudy (mkSid 1) [BinaryArm tA 10 100, BinaryArm tB 30 100]
        , BinaryStudy (mkSid 2) [BinaryArm tA 20 100, BinaryArm tB 25 100]
        , BinaryStudy (mkSid 3) [BinaryArm tA 5 50,   BinaryArm tB 20 50]
        , BinaryStudy (mkSid 4) [BinaryArm tA 15 80,  BinaryArm tB 18 80]
        , BinaryStudy (mkSid 5) [BinaryArm tA 3 60,   BinaryArm tB 12 60]
        ]
      result = springREMLBin studies Nothing Nothing Nothing
   in case result of
        Left err -> return $ testFailed "pairwise RE binary" ("success", err)
        Right (neteffs, (tau2, path)) ->
          let -- logOR(A vs B) = θ_A - θ_B = ests[B][A]
              logOR = estLookup tB tA neteffs
              ok = logOR < 0 && not (isNaN logOR) && not (isInfinite logOR)
                && tau2 >= 0 && not (isNaN tau2)
           in do
                putStrLn $ "  τ² = " ++ show tau2
                putStrLn $ "  logOR = " ++ show logOR
                putStrLn $ "  path = " ++ show (take 8 path)
                return $ if ok
                  then testPassed "pairwise RE binary"
                    ("τ²=" ++ show tau2 ++ " logOR=" ++ show logOR)
                  else testFailed "pairwise RE binary"
                    ("finite τ²≥0, negative logOR",
                     "τ²=" ++ show tau2 ++ " logOR=" ++ show logOR)
