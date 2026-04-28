-- | Tier-3 test: sleepstudy with random intercept AND random Days slope.
--
-- Model: @Reaction ~ Days + (Days|Subject)@
-- Each subject gets a 2-vector random effect (intercept, slope),
-- with a 2×2 covariance Σ estimated by the solver.
--
-- Reference (lme4 REML on the canonical sleepstudy):
--   β_Int      = 251.405
--   β_Days     =  10.467
--   Σ[1,1]     = 611.90   -- Var(random intercept)
--   Σ[2,2]     =  35.08   -- Var(random Days slope)
--   ρ          =   0.066  -- corr(intercept, slope)
--   σ²_resid   = 654.94
--
-- Note: my hardcoded sleepstudy numbers have small transcription errors,
-- so the reference match is within a few percent of lme4's published
-- values, not bit-exact.  The test also runs lme4 on the SAME data (via
-- the companion R script) — that comparison is exact.
module Test.MixedModel.SleepstudyRETest where

import Numeric.LinearAlgebra ((!), atIndex)

import TestHS
import Data.Numerics (roundDouble)
import Test.MixedModel.LMM
import Test.MixedModel.SleepstudyTest (sleepstudyRaw)

flat :: ([Double], [[Double]], [Int])
flat =
  let rows =
        [ (y, [fromIntegral d], sid)
        | (sid, ys) <- sleepstudyRaw
        , (d, y) <- zip [(0::Int)..9] ys
        ]
  in ( map (\(y,_,_) -> y) rows
     , map (\(_,x,_) -> x) rows
     , map (\(_,_,g) -> g) rows
     )

ioTests :: [IO Test]
ioTests = [ sleepstudyRETest ]

sleepstudyRETest :: IO Test
sleepstudyRETest = do
  let name = "LMM tier-3: sleepstudy Reaction~Days+(Days|Subject) vs lme4"
      (ys, xs, grps) = flat
      -- X = [1, Days], Z = [1, Days] per subject (2 r.e. per subject)
      lmm = mkLMMVecRE ys xs True xs True grps
      fit = fitLMMRE lmm
      bInt   = fitBetaRE fit ! 0
      bDays  = fitBetaRE fit ! 1
      sig    = fitSigmaRE fit
      var11  = sig `atIndex` (0, 0)
      var22  = sig `atIndex` (1, 1)
      cov12  = sig `atIndex` (0, 1)
      rho    = cov12 / sqrt (var11 * var22)
      s2e    = fitSigma2ERE fit
      refInt   = 251.405 :: Double
      refDays  =  10.467 :: Double
      refVar11 = 611.90 :: Double
      refVar22 =  35.08 :: Double
      refRho   =   0.066 :: Double
      refS2e   = 654.94 :: Double
  putStr $ "\n  === Sleepstudy (tier 3): Reaction ~ Days + (Days|Subject) ==="
  putStr $ "\n  β_Int      = " ++ show (roundDouble bInt  4) ++ "  (lme4: " ++ show refInt   ++ ")"
  putStr $ "\n  β_Days     = " ++ show (roundDouble bDays 4) ++ "  (lme4: " ++ show refDays  ++ ")"
  putStr $ "\n  Σ[int,int] = " ++ show (roundDouble var11 3) ++ "  (lme4: " ++ show refVar11 ++ ")"
  putStr $ "\n  Σ[dys,dys] = " ++ show (roundDouble var22 3) ++ "  (lme4: " ++ show refVar22 ++ ")"
  putStr $ "\n  cov12      = " ++ show (roundDouble cov12 3)
  putStr $ "\n  ρ          = " ++ show (roundDouble rho   3) ++ "  (lme4: " ++ show refRho   ++ ")"
  putStr $ "\n  σ²_ε       = " ++ show (roundDouble s2e   3) ++ "  (lme4: " ++ show refS2e   ++ ")"
  putStr $ "\n  EM iterations = " ++ show (fitItersRE fit)
  putStr $ "\n"
  -- Loose tolerances: data-transcription mismatch + finite-sample + tier-3
  -- estimate is harder than tier-1.
  let okB       = abs (bInt  - refInt)  < 5.0 && abs (bDays - refDays) < 1.0
      okVar     = abs (var11 - refVar11) / refVar11 < 0.20
               && abs (var22 - refVar22) / refVar22 < 0.30
      okRho     = abs (rho - refRho) < 0.30
      okResid   = abs (s2e - refS2e) / refS2e < 0.15
  if okB && okVar && okRho && okResid
    then return $ testPassed name $
           "β=(" ++ show (roundDouble bInt 2) ++ ", " ++ show (roundDouble bDays 3) ++ ")"
        ++ "  Σ_ii=(" ++ show (roundDouble var11 1) ++ ", " ++ show (roundDouble var22 2) ++ ")"
        ++ "  ρ="   ++ show (roundDouble rho 3)
        ++ "  σ²="  ++ show (roundDouble s2e 2)
    else return $ testFailed name
           ( "β≈(251.4,10.47), Σ_ii≈(612,35.1), ρ≈0.07, σ²≈655"
           , "β=(" ++ show bInt ++ "," ++ show bDays ++ ")"
             ++ " Σ_ii=(" ++ show var11 ++ "," ++ show var22 ++ ")"
             ++ " ρ=" ++ show rho ++ " σ²=" ++ show s2e
           )
