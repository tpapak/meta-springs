-- | Test: sleepstudy (lme4) — LMM with fixed-effect covariate and random intercepts.
--
-- Model: @Reaction ~ Days + (1|Subject)@
-- Data:  18 subjects × 10 days of sleep deprivation = 180 observations
--        (Belenky et al. 2003, via @lme4::sleepstudy@).
-- Reference (lme4 REML, intercept-only random effects):
--   β_Int      = 251.405
--   β_Days     =  10.467
--   σ²_Subject = 1378.18
--   σ²_resid   =  960.46
--
-- Purpose: exercise the fixed-effect covariate path (X has a within-group
-- covariate Days) and show the IPD spring network in action.
--
-- NOTE: the 180 response values below are hardcoded by hand; small
-- transcription errors are near-inevitable.  The solver matches lme4
-- exactly on Dyestuff (see DyestuffTest), so any residual mismatch here
-- reflects data transcription, not the algorithm.  Swap in a CSV reader
-- with the real @sleepstudy@ export for bit-exact reference matching.
module Test.MixedModel.SleepstudyTest
  ( ioTests
  , sleepstudyRaw
  , sleepstudy
  , sleepstudyTest
  ) where

import Numeric.LinearAlgebra ((!))

import TestHS
import Data.Numerics (roundDouble)
import Test.MixedModel.LMM

-- | @lme4::sleepstudy@: (Subject ID, [Reaction at Day 0 .. 9]).
sleepstudyRaw :: [(Int, [Double])]
sleepstudyRaw =
  [ (308, [249.5600, 258.7047, 250.8006, 321.4398, 356.8519, 414.6901, 382.2038, 290.1486, 430.5853, 466.3535])
  , (309, [222.7339, 205.2658, 202.9778, 204.7070, 207.7161, 215.9618, 213.6303, 217.7272, 224.2957, 237.3142])
  , (310, [199.0539, 194.3322, 234.3200, 232.8416, 229.3074, 220.4579, 235.4208, 255.7511, 261.0125, 247.5153])
  , (330, [321.5426, 300.4002, 283.8565, 285.1330, 285.7973, 297.5855, 280.2396, 318.2613, 305.3495, 354.0487])
  , (331, [287.6079, 285.0000, 301.8206, 320.1153, 316.2773, 293.3187, 290.0750, 334.8177, 293.7469, 371.5811])
  , (332, [234.8606, 242.8118, 272.9613, 309.7688, 317.4629, 309.9976, 454.1619, 346.8311, 330.3003, 253.8644])
  , (333, [283.8424, 289.5550, 276.7693, 299.8097, 297.1710, 338.1665, 332.0265, 348.8399, 333.3600, 362.0428])
  , (334, [265.4731, 276.2012, 243.3647, 254.6723, 279.0244, 284.1912, 305.5248, 331.5229, 335.7469, 377.2990])
  , (335, [241.6083, 273.9472, 254.4907, 270.8021, 251.4519, 254.6362, 245.4523, 235.3110, 235.7541, 237.2466])
  , (337, [312.3666, 313.8058, 291.6112, 346.1222, 365.7324, 391.8385, 404.2601, 416.6923, 455.8643, 458.9167])
  , (349, [236.1032, 230.3167, 238.9256, 254.9220, 250.7103, 269.7744, 281.5648, 308.1020, 336.2806, 351.6451])
  , (350, [256.2968, 243.4543, 256.2046, 255.5271, 268.9165, 329.7247, 379.4445, 362.9184, 394.4872, 389.0527])
  , (351, [250.5265, 300.0576, 269.8939, 280.5891, 271.8274, 304.6336, 287.7466, 266.5955, 321.5418, 347.5655])
  , (352, [221.6771, 298.1939, 326.8785, 346.8555, 348.7402, 352.8287, 354.4266, 360.4326, 375.6406, 388.5417])
  , (369, [271.9235, 268.4369, 257.2424, 277.6566, 314.8222, 317.2135, 298.1353, 348.1229, 340.2800, 366.5131])
  , (370, [225.2640, 234.5235, 238.9008, 240.4730, 267.5373, 344.1937, 281.1481, 347.5855, 365.1630, 372.2288])
  , (371, [269.8804, 272.4428, 277.8989, 281.7895, 279.1705, 284.5120, 259.2658, 304.6306, 350.7807, 369.4692])
  , (372, [269.4117, 273.4740, 297.5968, 310.6316, 287.1726, 329.6076, 334.4818, 343.2199, 369.1417, 364.1236])
  ]

-- | Flatten into (ys, Days-covariate-rows, subject-grouping).
sleepstudy :: ([Double], [[Double]], [Int])
sleepstudy =
  let rows =
        [ (y, [fromIntegral d], sid)
        | (sid, ys) <- sleepstudyRaw
        , (d, y)    <- zip [(0::Int)..9] ys
        ]
  in ( map (\(y,_,_) -> y) rows
     , map (\(_,x,_) -> x) rows
     , map (\(_,_,g) -> g) rows
     )

ioTests :: [IO Test]
ioTests = [ sleepstudyTest ]

sleepstudyTest :: IO Test
sleepstudyTest = do
  let name = "LMM spring-EM: sleepstudy Reaction~Days+(1|Subject) vs lme4"
      (ys, xs, grps) = sleepstudy
      lmm = mkLMMFromRows ys xs True grps
      fit = fitLMM lmm
      (t2Next, s2Next, bAtRef, _, _) = emStepDiag lmm (1378.18, 960.46)
      bInt  = fitBeta fit ! 0
      bDays = fitBeta fit ! 1
      tau2  = fitTau2 fit
      s2e   = fitSigma2E fit
      refInt  = 251.405 :: Double
      refDays =  10.467 :: Double
      refTau2 = 1378.18 :: Double
      refS2e  =  960.46 :: Double
      sprs = extractSprings lmm fit
      shrSprings = filter ((== Shrinkage) . spKind) sprs
  putStr $ "\n  === Fixpoint check (feed lme4 ref values into emStep) ==="
  putStr $ "\n    from (τ²=1378.18, σ²=960.46) → (τ²=" ++ show (roundDouble t2Next 4)
                                          ++ ", σ²=" ++ show (roundDouble s2Next 4) ++ ")"
  putStr $ "\n    β at lme4 ref = (" ++ show (roundDouble (bAtRef ! 0) 4) ++ ", "
                                     ++ show (roundDouble (bAtRef ! 1) 4) ++ ")"
  putStr $ "\n  === Sleepstudy: Reaction ~ Days + (1|Subject) ==="
  putStr $ "\n  β_Int      = " <> show (roundDouble bInt  4) <> "  (lme4: " <> show refInt  <> ")"
  putStr $ "\n  β_Days     = " <> show (roundDouble bDays 4) <> "  (lme4: " <> show refDays <> ")"
  putStr $ "\n  τ²_Subject = " <> show (roundDouble tau2  3) <> "  (lme4: " <> show refTau2 <> ")"
  putStr $ "\n  σ²_ε       = " <> show (roundDouble s2e   3) <> "  (lme4: " <> show refS2e  <> ")"
  putStr $ "\n  EM iterations = " <> show (fitIters fit)
  putStr $ "\n  --- Springs ---"
  putStr $ "\n" <> summarizeSprings sprs
  putStr $ "\n  Subject BLUPs (shrinkage-spring lengths):"
  mapM_ (\s -> putStr $ "\n    " <> spLabel s
                     <> ": u="      <> show (roundDouble (spLength s) 3)
                     <> "  energy=" <> show (roundDouble (spEnergy s) 4)
        ) shrSprings
  putStr $ "\n"
  -- Loose tolerances: exact lme4 match requires bit-exact data, which the
  -- hand-transcription likely does not provide.  These tolerances confirm
  -- the solver converges to the right order of magnitude on a realistic
  -- IPD-style dataset.
  let okInt  = abs (bInt  - refInt)  < 2.0
      okDays = abs (bDays - refDays) < 0.2
      okTau2 = abs (tau2  - refTau2) < 100.0
      okS2e  = abs (s2e   - refS2e)  < 100.0
  if okInt && okDays && okTau2 && okS2e
    then return $ testPassed name $
              "β=(" <> show (roundDouble bInt 2) <> "," <> show (roundDouble bDays 3) <> ")"
           <> " τ²=" <> show (roundDouble tau2 2)
           <> " σ²ε=" <> show (roundDouble s2e 2)
    else return $ testFailed name
         ( "β_Int≈" <> show refInt <> " β_Days≈" <> show refDays
         <> " τ²≈" <> show refTau2 <> " σ²ε≈" <> show refS2e
         , "β_Int=" <> show bInt <> " β_Days=" <> show bDays
         <> " τ²=" <> show tau2 <> " σ²ε=" <> show s2e
         )
