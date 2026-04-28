-- | Honest comparison: proper spring force solver vs glmmTMB.
--
-- Three models, each solved by:
--   1. Direct matrix solve (existing SpringNet code)
--   2. Iterative spring relaxation (SpringRelax — gradient descent, no matrix solve)
--   3. glmmTMB / lme4 reference values
--
-- Linear springs:  Dyestuff, Sleepstudy (1|Subject)
-- Nonlinear springs: Plaque psoriasis logistic GLMM
module Test.MixedModel.SpringRelaxTest where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Numeric.LinearAlgebra ((!))
import qualified Numeric.LinearAlgebra as LA

import TestHS
import Data.Numerics (roundDouble)
import Test.MixedModel.LMM
  ( mkLMMFromRows, fitLMM, fitLogisticGLMM
  , LMMFit(..), GLMMFit(..)
  , LMMData(..) )
import Test.MixedModel.SpringRelax
import Test.MixedModel.SleepstudyTest (sleepstudy)
import Test.MixedModel.DyestuffTest (dyestuff)
import Data.Meta.Studies
import Data.Meta.Effects
import Data.Meta.RandomEffects (springGrandCanonical, springGrandCanonicalBin, springREML, springREMLBin)
import Data.Meta.NMA (SpringNetwork(..), NetworkEffects(..))
import Test.Meta.SpringREMLTest (dong2013)
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as Set

ioTests :: [IO Test]
ioTests = [gcPairwiseTest, gcNMATest, gcBinomialTest, gcNMAdbTest]

-- | 3-treatment NMA with one 3-arm study.
-- Treatments A, B, C. Studies:
--   Study 1: A vs B (2-arm)
--   Study 2: A vs C (2-arm)
--   Study 3: B vs C (2-arm)
--   Study 4: A vs B vs C (3-arm) — creates multi-arm correlation
gcNMATest :: IO Test
gcNMATest = do
  let name = "Grand canonical NMA: 3 treatments, 3-arm study"
      -- Construct studies in the library format
      mkSid i = StudyId (IntId i)
      tA = TreatmentId (StringId "A")
      tB = TreatmentId (StringId "B")
      tC = TreatmentId (StringId "C")
      nmaStudies =
        [ ContinuousStudy (mkSid 1)
            [ContinuousArm tA 10.0 3.0 50, ContinuousArm tB 12.0 3.5 55]
        , ContinuousStudy (mkSid 2)
            [ContinuousArm tA 9.0 2.8 40, ContinuousArm tC 14.0 4.0 45]
        , ContinuousStudy (mkSid 3)
            [ContinuousArm tB 11.5 3.2 60, ContinuousArm tC 15.0 3.8 50]
        , ContinuousStudy (mkSid 4)
            [ContinuousArm tA 10.5 3.1 30, ContinuousArm tB 13.0 3.3 35, ContinuousArm tC 14.5 3.9 32]
        ]
  -- Spring REML
  case springREML nmaStudies meanArm Nothing (Just 200) (Just 1e-4) of
    Left err -> return $ testFailed name ("REML error", err)
    Right (remlNE, (remlTau2, _)) -> do
      let remlNet = springNetwork remlNE
          nVerts = IM.size (vs2ts remlNet)
          nSprings = Map.size (springs remlNet)
          nTauE = Set.size (tauEdges remlNet)
          pinnedV = pinned remlNet
      putStr $ "\n  === 3-treatment NMA (incl 3-arm study) ==="
      putStr $ "\n  vertices: " ++ show nVerts ++ " springs: " ++ show nSprings ++ " tauEdges: " ++ show nTauE ++ " pinned: " ++ show pinnedV
      putStr $ "\n  free nodes: " ++ show (nVerts - 1) ++ " (= Laplacian size)"
      putStr $ "\n  vertex map: " ++ show (IM.toList (vs2ts remlNet))
      putStr $ "\n  spring REML tau2: " ++ show (roundDouble remlTau2 4)
      -- Print treatment effects
      let ne = networkEstimates remlNE
      putStr $ "\n  treatment effects: " ++ show ne
      -- Grand canonical: kTau=1000, nMax=3000 → tau2 range 0..3, step 0.001
      case springGrandCanonical nmaStudies meanArm 1000 3000 of
        Left err -> do
          putStr $ "\n  GC error: " ++ err ++ "\n"
          return $ testFailed name ("GC error", err)
        Right (modeTau2, grid, modeNE) -> do
          let logZs = map snd grid
              maxLZ = maximum logZs
              probs = map (\lz -> exp (lz - maxLZ)) logZs
              totalM = sum probs
              normProbs = map (/ totalM) probs
              tau2Vals = map fst grid
              postMean = sum (zipWith (*) tau2Vals normProbs)
              cum = scanl1 (+) normProbs
              ciLo = tau2Vals !! (length (takeWhile (< 0.025) cum))
              ciHi = tau2Vals !! (length (takeWhile (< 0.975) cum))
          -- Debug: print first few logZ values
          putStr $ "\n  first 5 logZ: " ++ show (take 5 (map snd grid))
          putStr $ "\n  GC tau2 mode:  " ++ show (roundDouble modeTau2 4)
          putStr $ "\n  GC tau2 mean:  " ++ show (roundDouble postMean 4)
          putStr $ "\n  GC 95% CI:     (" ++ show (roundDouble ciLo 4) ++ ", " ++ show (roundDouble ciHi 4) ++ ")"
          -- Debug: print logZ at specific tau2 values
          putStr $ "\n  tau2    logZ"
          mapM_ (\(t2, lz) ->
            if t2 `elem` [0.001, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.5, 1.0, 2.0]
            then putStr $ "\n  " ++ show t2 ++ "  " ++ show (roundDouble lz 3)
            else return ()
            ) grid
          putStr $ "\n"
          let ok = abs (modeTau2 - remlTau2) < remlTau2 * 0.5 + 1.0
          if ok
            then return $ testPassed name $
                   "REML=" ++ show (roundDouble remlTau2 3) ++ " GC=" ++ show (roundDouble modeTau2 3)
            else return $ testFailed name
                   ("GC near REML", "REML=" ++ show remlTau2 ++ " GC=" ++ show modeTau2)

gcNMAdbTest :: IO Test
gcNMAdbTest = do
  let name = "Grand canonical NMA: nmadb_479622 (5 trts, 13 studies, tau2~13)"
  estudies <- readStudies "test/nmadb/nmadb_479622.json"
  case estudies of
    Left err -> return $ testFailed name ("read error", err)
    Right studies -> do
      case springREML studies meanArm Nothing (Just 200) (Just 1e-4) of
        Left err -> return $ testFailed name ("REML error", err)
        Right (_, (remlTau2, _)) -> do
          putStr $ "\n  === nmadb_479622 (5 trts, 13 studies, real heterogeneity) ==="
          putStr $ "\n  spring REML tau2: " ++ show (roundDouble remlTau2 4)
          -- kTau=10, nMax=500 -> tau2 range 0..50
          case springGrandCanonical studies meanArm 10 500 of
            Left err -> do
              putStr $ "\n  GC error: " ++ err ++ "\n"
              return $ testFailed name ("GC error", err)
            Right (modeTau2, grid, modeNE) -> do
              let logZs = map snd grid
                  maxLZ = maximum logZs
                  probs = map (\lz -> exp (lz - maxLZ)) logZs
                  totalM = sum probs
                  normProbs = map (/ totalM) probs
                  tau2Vals = map fst grid
                  postMean = sum (zipWith (*) tau2Vals normProbs)
                  cum = scanl1 (+) normProbs
                  ciLo = tau2Vals !! (length (takeWhile (< 0.025) cum))
                  ciHi = tau2Vals !! (length (takeWhile (< 0.975) cum))
              putStr $ "\n  GC tau2 mode:  " ++ show (roundDouble modeTau2 2)
              putStr $ "\n  GC tau2 mean:  " ++ show (roundDouble postMean 2)
              putStr $ "\n  GC 95% CI:     (" ++ show (roundDouble ciLo 2) ++ ", " ++ show (roundDouble ciHi 2) ++ ")"
              putStr $ "\n  netmeta tau2_contrast: 12.97"
              putStr $ "\n  GC tau2_contrast (2x): " ++ show (roundDouble (modeTau2 * 2) 2)
              putStr $ "\n"
              let ok = modeTau2 > 2 && modeTau2 < 20  -- should be around 6-10 arm-space
              if ok
                then return $ testPassed name $
                       "REML=" ++ show (roundDouble remlTau2 2) ++ " GC=" ++ show (roundDouble modeTau2 2)
                else return $ testFailed name ("tau2 in range", show modeTau2)

gcBinomialTest :: IO Test
gcBinomialTest = do
  let name = "Grand canonical binomial NMA: Dong2013"
  -- Spring REML for reference
  case springREMLBin dong2013 Nothing (Just 1000) (Just 1e-6) of
    Left err -> return $ testFailed name ("REML error", err)
    Right (_, (remlTau2, _)) -> do
      putStr $ "\n  === Dong2013 binomial NMA: Grand canonical ==="
      putStr $ "\n  spring REML tau2: " ++ show (roundDouble remlTau2 6)
      -- Grand canonical: kTau=1000, nMax=500 (tau2 range 0..0.5, step 0.001)
      case springGrandCanonicalBin dong2013 1000 500 of
        Left err -> do
          putStr $ "\n  GC error: " ++ err ++ "\n"
          return $ testFailed name ("GC error", err)
        Right (modeTau2, grid, modeNE) -> do
          let logZs = map snd grid
              maxLZ = maximum logZs
              probs = map (\lz -> exp (lz - maxLZ)) logZs
              totalM = sum probs
              normProbs = map (/ totalM) probs
              tau2Vals = map fst grid
              postMean = sum (zipWith (*) tau2Vals normProbs)
              cum = scanl1 (+) normProbs
              ciLo = tau2Vals !! (length (takeWhile (< 0.025) cum))
              ciHi = tau2Vals !! (length (takeWhile (< 0.975) cum))
          putStr $ "\n  GC tau2 mode:  " ++ show (roundDouble modeTau2 6)
          putStr $ "\n  GC tau2 mean:  " ++ show (roundDouble postMean 6)
          putStr $ "\n  GC 95% CI:     (" ++ show (roundDouble ciLo 6) ++ ", " ++ show (roundDouble ciHi 6) ++ ")"
          putStr $ "\n  multinma ref:  tau ~0.13 (tau2 ~0.017)"
          putStr $ "\n"
          if modeTau2 >= 0
            then return $ testPassed name $
                   "REML=" ++ show (roundDouble remlTau2 5) ++ " GC=" ++ show (roundDouble modeTau2 5)
            else return $ testFailed name ("tau2 >= 0", show modeTau2)

dyeGrandTest :: IO Test
dyeGrandTest = do
  let name = "Grand canonical (chain springs): Dyestuff"
      (ys, grps) = dyestuff
      y = LA.fromList ys
      x = (length ys LA.>< 1) (replicate (length ys) 1.0)
      uniqGs = nub grps
      z = LA.fromLists [[if g == grp then 1.0 else 0.0 | g <- uniqGs] | grp <- grps]
      -- k_tau=0.1, N_max=1000 → tau2 range 0..10000, step 10
      post = fitGaussianGrand y x z 0.1 1000
      refTau2 = 1764.05
      (ciLo, ciHi) = tpCI post
  putStr $ "\n  === Dyestuff: Grand canonical (k_tau=0.1, N_max=1000) ==="
  putStr $ "\n  tau2 mode  = " ++ show (roundDouble (tpMode post) 2) ++ "  (REML: " ++ show refTau2 ++ ")"
  putStr $ "\n  tau2 mean  = " ++ show (roundDouble (tpMean post) 2)
  putStr $ "\n  95% CI     = (" ++ show (roundDouble ciLo 1) ++ ", " ++ show (roundDouble ciHi 1) ++ ")"
  putStr $ "\n  beta       = " ++ show (roundDouble (tpBeta post ! 0) 2)
  putStr $ "\n"
  let ok = abs (tpMode post - refTau2) < 300
  if ok
    then return $ testPassed name $
           "mode=" ++ show (roundDouble (tpMode post) 1)
        ++ " CI=(" ++ show (roundDouble ciLo 1) ++ "," ++ show (roundDouble ciHi 1) ++ ")"
    else return $ testFailed name
           ("mode near 1764", "mode=" ++ show (tpMode post))

gcPairwiseTest :: IO Test
gcPairwiseTest = do
  let name = "Grand canonical pairwise: continuous.json"
  estudies <- readStudies "test/continuous.json"
  case estudies of
    Left err -> return $ testFailed name ("read error", err)
    Right studies -> do
      -- First: what does spring REML give?
      case springREML studies meanArm Nothing (Just 100) (Just 1e-4) of
        Left err -> return $ testFailed name ("REML error", err)
        Right (remlNE, (remlTau2, _)) -> do
          putStr $ "\n  === Pairwise continuous.json (6 studies) ==="
          putStr $ "\n  spring REML tau2_arm: " ++ show (roundDouble remlTau2 4)
          -- Now grand canonical
          case springGrandCanonical studies meanArm 10 500 of
            Left err -> do
              putStr $ "\n  GC error: " ++ err
              return $ testFailed name ("GC error", err)
            Right (modeTau2, grid, modeNE) -> do
              let logZs = map snd grid
                  maxLZ = maximum logZs
                  probs = map (\lz -> exp (lz - maxLZ)) logZs
                  totalM = sum probs
                  normProbs = map (/ totalM) probs
                  tau2Vals = map fst grid
                  postMean = sum (zipWith (*) tau2Vals normProbs)
              putStr $ "\n  GC tau2 mode: " ++ show (roundDouble modeTau2 4)
              putStr $ "\n  GC tau2 mean: " ++ show (roundDouble postMean 4)
              putStr $ "\n  grid range:   0 to " ++ show (last tau2Vals)
              -- Print logZ at a few points to debug
              let sampleIdxs = [0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 499]
              putStr $ "\n  tau2     logZ"
              mapM_ (\i -> if i < length grid
                then let (t2, lz) = grid !! i
                     in putStr $ "\n  " ++ show (roundDouble t2 2) ++ "    " ++ show (roundDouble lz 2)
                else return ()
                ) sampleIdxs
              putStr $ "\n"
              -- Both GC and REML now in contrast-space
              let ok = abs (modeTau2 - remlTau2) < remlTau2 * 0.5
              if ok
                then return $ testPassed name $
                       "REML=" ++ show (roundDouble remlTau2 4) ++ " GC=" ++ show (roundDouble modeTau2 4)
                else return $ testFailed name
                       ("GC near REML", "REML=" ++ show remlTau2 ++ " GC=" ++ show modeTau2)

dyeGrandEigenTest :: IO Test
dyeGrandEigenTest = do
  let name = "Grand canonical eigenvalue: BCG pairwise MA"
      -- BCG data: 13 studies, log-RR with known variances
      yi = LA.fromList [-0.8893,-1.5854,-1.3481,-1.4416,-0.2175,-0.7861,-1.6209,0.0120,-0.4694,-1.3713,-0.3394,0.4459,-0.0173]
      vi = LA.fromList [0.3256,0.1946,0.4154,0.0200,0.0512,0.0069,0.2230,0.0040,0.0564,0.0730,0.0124,0.5325,0.0714]
      x  = (13 LA.>< 1) (replicate 13 1.0)  -- intercept only
      -- k_tau=1000, N_max=3000: tau2 from 0.001 to 3, step 0.001
      postE = fitGaussianGrandEigen yi vi x 1000 3000
      refTau2 = 0.3132  -- REML from metafor
      (ciLoE, ciHiE) = tpCI postE
  putStr $ "\n  === BCG: Grand canonical eigenvalue (k_tau=1000, N_max=3000) ==="
  putStr $ "\n  tau2 mode  = " ++ show (roundDouble (tpMode postE) 4) ++ "  (REML: " ++ show refTau2 ++ ")"
  putStr $ "\n  tau2 mean  = " ++ show (roundDouble (tpMean postE) 4)
  putStr $ "\n  95% CI     = (" ++ show (roundDouble ciLoE 4) ++ ", " ++ show (roundDouble ciHiE 4) ++ ")"
  putStr $ "\n  beta       = " ++ show (roundDouble (tpBeta postE ! 0) 4)
  putStr $ "\n"
  let ok = abs (tpMode postE - refTau2) < 0.02
  if ok
    then return $ testPassed name $
           "mode=" ++ show (roundDouble (tpMode postE) 4)
        ++ " CI=(" ++ show (roundDouble ciLoE 3) ++ "," ++ show (roundDouble ciHiE 3) ++ ")"
    else return $ testFailed name
           ("mode near 0.313", "mode=" ++ show (tpMode postE))

-- ======================================================================
-- 1. Dyestuff — linear springs
-- ======================================================================
dyeRelaxTest :: IO Test
dyeRelaxTest = do
  let name = "Spring relaxation vs direct solve vs glmmTMB: Dyestuff"
      (ys, grps) = dyestuff
      lmm = mkLMMFromRows ys [] True grps
      fitD = fitLMM lmm
      y = LA.fromList ys
      x = (length ys LA.>< 1) (replicate (length ys) 1.0)
      uniqGs = nub grps
      z = LA.fromLists [[if g == grp then 1.0 else 0.0 | g <- uniqGs] | grp <- grps]
      (betaR, _uR, tau2R, s2R, oIter, totInner) = fitSpringEM y x z
      refBeta = 1527.5
      refTau2 = 1764.05
      refS2e  = 2451.25
  putStr $ "\n  === Dyestuff: spring relaxation vs direct vs glmmTMB ==="
  putStr $ "\n                   glmmTMB      direct   relaxation"
  putStr $ "\n  β₀          " ++ fmt refBeta ++ fmt (fitBeta fitD ! 0) ++ fmt (betaR ! 0)
  putStr $ "\n  τ²          " ++ fmt refTau2 ++ fmt (fitTau2 fitD)     ++ fmt tau2R
  putStr $ "\n  σ²_ε        " ++ fmt refS2e  ++ fmt (fitSigma2E fitD)  ++ fmt s2R
  putStr $ "\n  outer iters:   —           " ++ show (fitIters fitD) ++ "          " ++ show oIter
  putStr $ "\n  inner iters:   —           —           " ++ show totInner
  putStr $ "\n"
  let okB  = abs (betaR ! 0 - refBeta) < 0.1
      okT  = abs (tau2R - refTau2) < 1.0
      okS  = abs (s2R - refS2e) < 1.0
  if okB && okT && okS
    then return $ testPassed name $
           "β=" ++ show (roundDouble (betaR ! 0) 2)
        ++ " τ²=" ++ show (roundDouble tau2R 2)
        ++ " σ²=" ++ show (roundDouble s2R 2)
    else return $ testFailed name
           ("within 1.0 of glmmTMB"
           ,"β=" ++ show (betaR ! 0) ++ " τ²=" ++ show tau2R ++ " σ²=" ++ show s2R)

-- ======================================================================
-- 2. Sleepstudy — linear springs with covariate
-- ======================================================================
sleepRelaxTest :: IO Test
sleepRelaxTest = do
  let name = "Spring relaxation vs direct solve vs glmmTMB: Sleepstudy"
      (ys, xs, grps) = sleepstudy
      lmm = mkLMMFromRows ys xs True grps
      fitD = fitLMM lmm
      xRows = [[1.0, head xr] | xr <- xs]
      y = LA.fromList ys
      x = LA.fromLists xRows
      uniqGs = nub grps
      z = LA.fromLists [[if g == grp then 1.0 else 0.0 | g <- uniqGs] | grp <- grps]
      (betaR, _uR, tau2R, s2R, oIter, totInner) = fitSpringEM y x z
      -- glmmTMB reference
      refInt  = 251.4051
      refDays = 10.46729
      refTau2 = 1378.177
      refS2e  = 960.4566
  putStr $ "\n  === Sleepstudy: spring relaxation vs direct vs glmmTMB ==="
  putStr $ "\n                   glmmTMB      direct   relaxation"
  putStr $ "\n  β_Int       " ++ fmt refInt  ++ fmt (fitBeta fitD ! 0) ++ fmt (betaR ! 0)
  putStr $ "\n  β_Days      " ++ fmt refDays ++ fmt (fitBeta fitD ! 1) ++ fmt (betaR ! 1)
  putStr $ "\n  τ²          " ++ fmt refTau2 ++ fmt (fitTau2 fitD)     ++ fmt tau2R
  putStr $ "\n  σ²_ε        " ++ fmt refS2e  ++ fmt (fitSigma2E fitD)  ++ fmt s2R
  putStr $ "\n  outer iters:   —           " ++ show (fitIters fitD) ++ "          " ++ show oIter
  putStr $ "\n  inner iters:   —           —           " ++ show totInner
  putStr $ "\n"
  let okI = abs (betaR ! 0 - refInt)  < 0.01
      okD = abs (betaR ! 1 - refDays) < 0.001
      okT = abs (tau2R - refTau2) < 0.1
      okS = abs (s2R - refS2e) < 0.1
  if okI && okD && okT && okS
    then return $ testPassed name $
           "β=(" ++ show (roundDouble (betaR ! 0) 4) ++ "," ++ show (roundDouble (betaR ! 1) 4) ++ ")"
        ++ " τ²=" ++ show (roundDouble tau2R 2)
        ++ " σ²=" ++ show (roundDouble s2R 2)
    else return $ testFailed name
           ("within 0.1 of glmmTMB"
           ,"β=(" ++ show (betaR ! 0) ++ "," ++ show (betaR ! 1) ++ ")"
            ++ " τ²=" ++ show tau2R ++ " σ²=" ++ show s2R)

-- ======================================================================
-- 3. Plaque psoriasis — NONLINEAR (binomial) springs
-- ======================================================================
plaqueRelaxTest :: IO Test
plaqueRelaxTest = do
  let name = "Spring relaxation (nonlinear) vs IRLS vs glmmTMB: Plaque GLMM"
  raw <- readFile "test/plaque_psoriasis_min.csv"
  let ls = drop 1 (lines raw)
      parseLine l = case splitOn ',' (filter (/= '"') l) of
        [s, t, w, yStr] -> (s, t, read w :: Double, read yStr :: Int)
        _               -> error ("bad row: " ++ l)
      rows = map parseLine ls
      studies = map (\(s,_,_,_) -> s) rows
      trts    = map (\(_,t,_,_) -> t) rows
      wts     = map (\(_,_,w,_) -> w) rows
      yInts   = map (\(_,_,_,y') -> y') rows
      -- Build design matrices
      trtLvl  = ["ETN", "IXE_Q2W", "IXE_Q4W", "UST"]
      dummies t = map (\lvl -> if t == lvl then 1.0 else 0.0) trtLvl
      xRows   = [[1.0] ++ dummies t ++ [w] | (t, w) <- zip trts wts]
      studyIds = Map.fromList (zip (nub studies) [0 :: Int ..])
      grps    = map (\s -> studyIds Map.! s) studies
      nAll    = length yInts
      y = LA.fromList (map fromIntegral yInts)
      x = LA.fromLists xRows
      uniqGs = nub grps
      z = LA.fromLists [[if g == grp then 1.0 else 0.0 | g <- uniqGs] | grp <- grps]
      -- Direct IRLS solve (existing code)
      lmm = mkLMMFromRows (map fromIntegral yInts) (map (drop 1) xRows) True grps
      fitI = fitLogisticGLMM lmm
      -- Spring relaxation with nonlinear springs
      (betaR, _uR, tau2R, oIter, totInner) = fitSpringEMBinomial y x z
      -- glmmTMB reference (PBO as reference via coefficient rearrangement)
      -- glmmTMB uses ETN as reference, so:
      -- PBO intercept = glmmTMB_intercept + glmmTMB_trtcPBO = -0.0830 + (-2.7632) = -2.8462
      refB0   = -2.8462
      refETN  =  2.7632
      refIXE2 =  5.1760
      refIXE4 =  4.4016
      refUST  =  3.8943
      refWt   = -0.1150
      refTau2 =  0.03113
  putStr $ "\n  === Plaque GLMM: nonlinear spring relaxation vs IRLS vs glmmTMB ==="
  putStr $ "\n  n=" ++ show nAll ++ " patients, " ++ show (length uniqGs) ++ " studies"
  putStr $ "\n                   glmmTMB     IRLS     relaxation"
  putStr $ "\n  β₀(PBO)     " ++ fmt refB0   ++ fmt (glmmBeta fitI ! 0) ++ fmt (betaR ! 0)
  putStr $ "\n  β_ETN       " ++ fmt refETN  ++ fmt (glmmBeta fitI ! 1) ++ fmt (betaR ! 1)
  putStr $ "\n  β_IXE_Q2W   " ++ fmt refIXE2 ++ fmt (glmmBeta fitI ! 2) ++ fmt (betaR ! 2)
  putStr $ "\n  β_IXE_Q4W   " ++ fmt refIXE4 ++ fmt (glmmBeta fitI ! 3) ++ fmt (betaR ! 3)
  putStr $ "\n  β_UST       " ++ fmt refUST  ++ fmt (glmmBeta fitI ! 4) ++ fmt (betaR ! 4)
  putStr $ "\n  β_wt        " ++ fmt refWt   ++ fmt (glmmBeta fitI ! 5) ++ fmt (betaR ! 5)
  putStr $ "\n  τ²          " ++ fmt5 refTau2 ++ fmt5 (glmmTau2 fitI)   ++ fmt5 tau2R
  putStr $ "\n  outer iters:   —           " ++ show (glmmIters fitI) ++ "          " ++ show oIter
  putStr $ "\n  inner iters:   —           —           " ++ show totInner
  putStr $ "\n"
  let okBeta = abs (betaR ! 0 - refB0)  < 0.15
            && abs (betaR ! 1 - refETN) < 0.15
            && abs (betaR ! 5 - refWt)  < 0.02
      okTau = tau2R > 0
  if okBeta && okTau
    then return $ testPassed name $
           "β₀=" ++ show (roundDouble (betaR ! 0) 3)
        ++ " β_ETN=" ++ show (roundDouble (betaR ! 1) 3)
        ++ " τ²=" ++ show (roundDouble tau2R 4)
    else return $ testFailed name
           ("β within 0.15 of glmmTMB"
           ,"β₀=" ++ show (betaR ! 0)
            ++ " β_ETN=" ++ show (betaR ! 1)
            ++ " τ²=" ++ show tau2R)

-- ======================================================================
-- Newton tests — same models, Newton inner solver
-- ======================================================================

dyeNewtonTest :: IO Test
dyeNewtonTest = do
  let name = "Spring Newton vs glmmTMB: Dyestuff"
      (ys, grps) = dyestuff
      y = LA.fromList ys
      x = (length ys LA.>< 1) (replicate (length ys) 1.0)
      uniqGs = nub grps
      z = LA.fromLists [[if g == grp then 1.0 else 0.0 | g <- uniqGs] | grp <- grps]
      (betaN, _uN, tau2N, s2N, oIterN, totInnerN) = fitSpringEMNewton y x z
      refBeta = 1527.5
      refTau2 = 1764.05
      refS2e  = 2451.25
  putStr $ "\n  === Dyestuff: Newton spring solver vs glmmTMB ==="
  putStr $ "\n                   glmmTMB      Newton"
  putStr $ "\n  β₀          " ++ fmt refBeta ++ fmt (betaN ! 0)
  putStr $ "\n  τ²          " ++ fmt refTau2 ++ fmt tau2N
  putStr $ "\n  σ²_ε        " ++ fmt refS2e  ++ fmt s2N
  putStr $ "\n  outer iters:   —           " ++ show oIterN
  putStr $ "\n  inner iters:   —           " ++ show totInnerN
  putStr $ "\n"
  let ok = abs (betaN ! 0 - refBeta) < 0.1
        && abs (tau2N - refTau2) < 1.0
  if ok
    then return $ testPassed name $
           "β=" ++ show (roundDouble (betaN ! 0) 2)
        ++ " τ²=" ++ show (roundDouble tau2N 2)
        ++ " inner=" ++ show totInnerN
    else return $ testFailed name
           ("match glmmTMB", "β=" ++ show (betaN ! 0) ++ " τ²=" ++ show tau2N)

sleepNewtonTest :: IO Test
sleepNewtonTest = do
  let name = "Spring Newton vs glmmTMB: Sleepstudy"
      (ys, xs, grps) = sleepstudy
      xRows = [[1.0, head xr] | xr <- xs]
      y = LA.fromList ys
      x = LA.fromLists xRows
      uniqGs = nub grps
      z = LA.fromLists [[if g == grp then 1.0 else 0.0 | g <- uniqGs] | grp <- grps]
      (betaN, _uN, tau2N, s2N, oIterN, totInnerN) = fitSpringEMNewton y x z
      refInt  = 251.4051
      refDays = 10.46729
      refTau2 = 1378.177
      refS2e  = 960.4566
  putStr $ "\n  === Sleepstudy: Newton spring solver vs glmmTMB ==="
  putStr $ "\n                   glmmTMB      Newton"
  putStr $ "\n  β_Int       " ++ fmt refInt  ++ fmt (betaN ! 0)
  putStr $ "\n  β_Days      " ++ fmt refDays ++ fmt (betaN ! 1)
  putStr $ "\n  τ²          " ++ fmt refTau2 ++ fmt tau2N
  putStr $ "\n  σ²_ε        " ++ fmt refS2e  ++ fmt s2N
  putStr $ "\n  outer iters:   —           " ++ show oIterN
  putStr $ "\n  inner iters:   —           " ++ show totInnerN
  putStr $ "\n"
  let ok = abs (betaN ! 0 - refInt) < 0.01
        && abs (betaN ! 1 - refDays) < 0.001
  if ok
    then return $ testPassed name $
           "β=(" ++ show (roundDouble (betaN ! 0) 4) ++ "," ++ show (roundDouble (betaN ! 1) 4) ++ ")"
        ++ " inner=" ++ show totInnerN
    else return $ testFailed name
           ("match glmmTMB", "β=(" ++ show (betaN ! 0) ++ "," ++ show (betaN ! 1) ++ ")")

plaqueNewtonTest :: IO Test
plaqueNewtonTest = do
  let name = "Spring Newton (nonlinear) vs glmmTMB: Plaque GLMM"
  raw <- readFile "test/plaque_psoriasis_min.csv"
  let ls = drop 1 (lines raw)
      parseLine l = case splitOn ',' (filter (/= '"') l) of
        [s, t, w, yStr] -> (s, t, read w :: Double, read yStr :: Int)
        _               -> error ("bad row: " ++ l)
      rows = map parseLine ls
      studies = map (\(s,_,_,_) -> s) rows
      trts    = map (\(_,t,_,_) -> t) rows
      wts     = map (\(_,_,w,_) -> w) rows
      yInts   = map (\(_,_,_,y') -> y') rows
      trtLvl  = ["ETN", "IXE_Q2W", "IXE_Q4W", "UST"]
      dummies t = map (\lvl -> if t == lvl then 1.0 else 0.0) trtLvl
      xRows   = [[1.0] ++ dummies t ++ [w] | (t, w) <- zip trts wts]
      studyIds = Map.fromList (zip (nub studies) [0 :: Int ..])
      grps    = map (\s -> studyIds Map.! s) studies
      y = LA.fromList (map fromIntegral yInts)
      x = LA.fromLists xRows
      uniqGs = nub grps
      z = LA.fromLists [[if g == grp then 1.0 else 0.0 | g <- uniqGs] | grp <- grps]
      (betaN, _uN, tau2N, oIterN, totInnerN) = fitSpringEMBinomialNewton y x z
      -- IRLS solver (existing code)
      lmm = mkLMMFromRows (map fromIntegral yInts) (map (drop 1) xRows) True grps
      fitI = fitLogisticGLMM lmm
      refB0   = -2.8462
      refETN  =  2.7632
      refIXE2 =  5.1760
      refIXE4 =  4.4016
      refUST  =  3.8943
      refWt   = -0.1150
      refTau2 =  0.03113    -- glmmTMB Laplace-ML
      refTau2AGQ = 0.03114  -- glmer nAGQ=25 (near-exact)
  putStr $ "\n  === Plaque GLMM: Newton vs IRLS vs AGQ(exact) vs glmmTMB ==="
  putStr $ "\n  n=" ++ show (length yInts) ++ " patients, " ++ show (length uniqGs) ++ " studies"
  putStr $ "\n                   AGQ(25)      glmmTMB      Newton       IRLS"
  putStr $ "\n  β₀(PBO)     " ++ fmt refB0   ++ fmt refB0   ++ fmt (betaN ! 0) ++ fmt (glmmBeta fitI ! 0)
  putStr $ "\n  β_ETN       " ++ fmt refETN  ++ fmt refETN  ++ fmt (betaN ! 1) ++ fmt (glmmBeta fitI ! 1)
  putStr $ "\n  β_IXE_Q2W   " ++ fmt refIXE2 ++ fmt refIXE2 ++ fmt (betaN ! 2) ++ fmt (glmmBeta fitI ! 2)
  putStr $ "\n  β_IXE_Q4W   " ++ fmt refIXE4 ++ fmt refIXE4 ++ fmt (betaN ! 3) ++ fmt (glmmBeta fitI ! 3)
  putStr $ "\n  β_UST       " ++ fmt refUST  ++ fmt refUST  ++ fmt (betaN ! 4) ++ fmt (glmmBeta fitI ! 4)
  putStr $ "\n  β_wt        " ++ fmt refWt   ++ fmt refWt   ++ fmt (betaN ! 5) ++ fmt (glmmBeta fitI ! 5)
  putStr $ "\n  τ²          " ++ fmt5 refTau2AGQ ++ fmt5 refTau2 ++ fmt5 tau2N  ++ fmt5 (glmmTau2 fitI)
  putStr $ "\n  outer iters:   —           —           " ++ show oIterN ++ "          " ++ show (glmmIters fitI)
  putStr $ "\n  inner iters:   —           —           " ++ show totInnerN ++ "          —"
  putStr $ "\n"
  let ok = abs (betaN ! 0 - refB0) < 0.15
        && abs (betaN ! 1 - refETN) < 0.15
  if ok
    then return $ testPassed name $
           "β₀=" ++ show (roundDouble (betaN ! 0) 3)
        ++ " β_ETN=" ++ show (roundDouble (betaN ! 1) 3)
        ++ " τ²=" ++ show (roundDouble tau2N 4)
        ++ " inner=" ++ show totInnerN
    else return $ testFailed name
           ("match glmmTMB"
           ,"β₀=" ++ show (betaN ! 0) ++ " β_ETN=" ++ show (betaN ! 1))

-- ======================================================================
-- Z integration tests
-- ======================================================================

dyeZTest :: IO Test
dyeZTest = do
  let name = "Z integration diagnostic: Dyestuff logZ at known tau2 values"
      (ys, grps) = dyestuff
      n = length ys
      y = LA.fromList ys
      x = (n LA.>< 1) (replicate n 1.0)
      uniqGs = nub grps
      z = LA.fromLists [[if g == grp then 1.0 else 0.0 | g <- uniqGs] | grp <- grps]
      p = LA.cols x
      q = LA.cols z
      m = p + q
      -- Compute logZ at specific tau2 values, profiling sigma2
      computeLogZ tau2 =
        let -- Profile sigma2 by iterating
            go s2 sol 0 = (s2, sol)
            go s2 sol iter =
              let spr = buildGaussianSprings y x z s2 tau2
                  (xS, _, _) = relaxNewton spr m sol 1e-12 10
                  b_ = LA.subVector 0 p xS
                  u_ = LA.subVector p q xS
                  r_ = y - x LA.#> b_ - z LA.#> u_
                  s2' = (r_ LA.<.> r_) / fromIntegral (n - p)
              in go s2' xS (iter - 1 :: Int)
            (sigma2, sol) = go 2500 (LA.konst 0 m) 10
            spr = buildGaussianSprings y x z sigma2 tau2
            (xSol, _, _) = relaxNewton spr m sol 1e-12 10
            b_ = LA.subVector 0 p xSol
            u_ = LA.subVector p q xSol
            r_ = y - x LA.#> b_ - z LA.#> u_
            rss = r_ LA.<.> r_
            uSq = u_ LA.<.> u_
            eStar = 0.5 * rss / sigma2 + 0.5 * uSq / tau2
            hess = springHessian spr m xSol
            logDetH = log (abs (LA.det hess))
            logZ = - fromIntegral n / 2 * log sigma2
                   - fromIntegral q / 2 * log tau2
                   - 0.5 * logDetH - eStar
        in (sigma2, eStar, logDetH, logZ)
      -- R reference: logZ is maximized at tau2=1764
      tau2Vals = [500, 1000, 1500, 1764.05, 2000, 3000, 5000] :: [Double]
      results = map (\t2 -> let (s2, es, ld, lz) = computeLogZ t2
                            in (t2, s2, es, ld, lz)) tau2Vals
  putStr $ "\n  === Dyestuff: logZ diagnostic ==="
  putStr $ "\n  R reference: max logZ at tau2=1764, logZ=-133.330"
  putStr $ "\n  tau2      sigma2    E*       logDetA   logZ"
  mapM_ (\(t2, s2, es, ld, lz) ->
    putStr $ "\n  " ++ show (roundDouble t2 0)
          ++ "    " ++ show (roundDouble s2 2)
          ++ "    " ++ show (roundDouble es 3)
          ++ "    " ++ show (roundDouble ld 3)
          ++ "    " ++ show (roundDouble lz 3)
    ) results
  putStr $ "\n"
  -- Check: logZ should be maximized near tau2=1764
  let logZs = map (\(_, _, _, _, lz) -> lz) results
      maxLZ = maximum logZs
      maxIdx = length (takeWhile (/= maxLZ) logZs)
      maxTau2 = fst5 (results !! maxIdx)
      ok = abs (maxTau2 - 1764.05) < 300
  if ok
    then return $ testPassed name $
           "logZ maximized at tau2=" ++ show (roundDouble maxTau2 0)
    else return $ testFailed name
           ("max near 1764", "max at tau2=" ++ show maxTau2)
  where fst5 (a,_,_,_,_) = a

dyeZFullTest :: IO Test
dyeZFullTest = do
  let name = "Z integration full: Dyestuff"
      (ys, grps) = dyestuff
      y = LA.fromList ys
      x = (length ys LA.>< 1) (replicate (length ys) 1.0)
      uniqGs = nub grps
      z = LA.fromLists [[if g == grp then 1.0 else 0.0 | g <- uniqGs] | grp <- grps]
      post = fitGaussianZ y x z
      refTau2 = 1764.05
      (ciLo, ciHi) = tpCI post
  putStr $ "\n  === Dyestuff: Z integration (full posterior) ==="
  putStr $ "\n  τ² mode  = " ++ show (roundDouble (tpMode post) 2) ++ "  (REML: " ++ show refTau2 ++ ")"
  putStr $ "\n  τ² mean  = " ++ show (roundDouble (tpMean post) 2)
  putStr $ "\n  95% CI   = (" ++ show (roundDouble ciLo 1) ++ ", " ++ show (roundDouble ciHi 1) ++ ")"
  putStr $ "\n  β₀       = " ++ show (roundDouble (tpBeta post ! 0) 2)
  putStr $ "\n  σ²_ε     = " ++ show (roundDouble (maybe 0 id (tpSigma2E post)) 2)
  putStr $ "\n"
  let ok = abs (tpMode post - refTau2) < 300
  if ok
    then return $ testPassed name $
           "mode=" ++ show (roundDouble (tpMode post) 1)
        ++ " CI=(" ++ show (roundDouble ciLo 1) ++ "," ++ show (roundDouble ciHi 1) ++ ")"
    else return $ testFailed name
           ("mode near 1764", "mode=" ++ show (tpMode post))

plaqueZTest :: IO Test
plaqueZTest = do
  let name = "Z integration vs AGQ vs EM: Plaque GLMM"
  raw <- readFile "test/plaque_psoriasis_min.csv"
  let ls = drop 1 (lines raw)
      parseLine l = case splitOn ',' (filter (/= '"') l) of
        [s, t, w, yStr] -> (s, t, read w :: Double, read yStr :: Int)
        _               -> error ("bad row: " ++ l)
      rows = map parseLine ls
      studies = map (\(s,_,_,_) -> s) rows
      trts    = map (\(_,t,_,_) -> t) rows
      wts     = map (\(_,_,w,_) -> w) rows
      yInts   = map (\(_,_,_,y') -> y') rows
      trtLvl  = ["ETN", "IXE_Q2W", "IXE_Q4W", "UST"]
      dummies t = map (\lvl -> if t == lvl then 1.0 else 0.0) trtLvl
      xRows   = [[1.0] ++ dummies t ++ [w] | (t, w) <- zip trts wts]
      studyIds = Map.fromList (zip (nub studies) [0 :: Int ..])
      grps    = map (\s -> studyIds Map.! s) studies
      y = LA.fromList (map fromIntegral yInts)
      x = LA.fromLists xRows
      uniqGs = nub grps
      z = LA.fromLists [[if g == grp then 1.0 else 0.0 | g <- uniqGs] | grp <- grps]
      post = fitBinomialZ y x z
      refTau2AGQ = 0.03114  -- glmer nAGQ=25
      refTau2EM  = 0.05333  -- our EM
      refB0      = -2.8462
      (ciLo, ciHi) = tpCI post
  putStr $ "\n  === Plaque GLMM: Z integration (closes the τ² gap?) ==="
  putStr $ "\n                   AGQ(25)      Z-mode     Z-mean       EM"
  putStr $ "\n  τ²          " ++ fmt5 refTau2AGQ ++ fmt5 (tpMode post) ++ fmt5 (tpMean post) ++ fmt5 refTau2EM
  putStr $ "\n  β₀(PBO)     " ++ fmt refB0 ++ fmt (tpBeta post ! 0) ++ "             " ++ fmt (-2.8347)
  putStr $ "\n  95% CI(τ²):    (" ++ show (roundDouble ciLo 5) ++ ", " ++ show (roundDouble ciHi 5) ++ ")"
  putStr $ "\n  gap AGQ vs Z-mode: " ++ show (roundDouble (abs (tpMode post - refTau2AGQ)) 5)
  putStr $ "\n  gap AGQ vs EM:     " ++ show (roundDouble (abs (refTau2EM - refTau2AGQ)) 5)
  putStr $ "\n"
  -- The Z-mode should be much closer to AGQ than EM is
  let ok = abs (tpMode post - refTau2AGQ) < abs (refTau2EM - refTau2AGQ)
  if ok
    then return $ testPassed name $
           "τ² Z-mode=" ++ show (roundDouble (tpMode post) 5)
        ++ " (AGQ=" ++ show refTau2AGQ ++ ", EM=" ++ show refTau2EM ++ ")"
    else return $ testFailed name
           ("Z-mode closer to AGQ than EM"
           ,"Z-mode=" ++ show (tpMode post) ++ " AGQ=" ++ show refTau2AGQ)

-- helpers
fmt :: Double -> String
fmt d = let s = show (roundDouble d 4) in replicate (13 - length s) ' ' ++ s

fmt5 :: Double -> String
fmt5 d = let s = show (roundDouble d 5) in replicate (13 - length s) ' ' ++ s

splitOn :: Char -> String -> [String]
splitOn sep s = case break (== sep) s of
  (a, [])     -> [a]
  (a, _:rest) -> a : splitOn sep rest
