-- | Simple synthetic IPD meta-analysis example.
--
-- Three trials, four patients each (12 rows total).  Every patient has:
--   * a continuous covariate (age)
--   * a binary treatment indicator (0 = control, 1 = active)
--   * a continuous outcome y
--
-- Data is generated exactly from the model
--
--     y  =  2  +  0.2 · age  +  6 · trt  +  u_study  +  ε
--
-- with u = (0, −5, 0) and small deterministic residuals ε that sum to zero
-- within each study.  The solver should recover
--
--     β_Intercept ≈ 2
--     β_age       ≈ 0.2
--     β_trt       ≈ 6
--     τ²          ≈ 8.33   -- variance of (0, −5, 0)
--     σ²_ε         small   -- from the tiny added residuals
--
-- and the BLUPs should recover (0, −5, 0) up to shrinkage.
module Test.MixedModel.IPDSimpleTest where

import Numeric.LinearAlgebra ((!))

import TestHS
import Data.Numerics (roundDouble)
import Test.MixedModel.LMM

-- | (study id, [(age, trt, y)] rows).
trials :: [(Int, [(Double, Double, Double)])]
trials =
  [ (1, [ (40, 0, 10.3),  (50, 0, 11.8),  (40, 1, 16.1),  (50, 1, 17.8) ])
  , (2, [ (30, 0,  3.3),  (50, 0,  6.8),  (30, 1,  9.1),  (50, 1, 12.8) ])
  , (3, [ (45, 0, 11.3),  (55, 0, 12.8),  (45, 1, 17.1),  (55, 1, 18.8) ])
  ]

flatten :: ([Double], [[Double]], [Int])
flatten =
  let rows =
        [ (y, [age, trt], sid)
        | (sid, pts) <- trials
        , (age, trt, y) <- pts
        ]
  in ( map (\(y,_,_) -> y) rows
     , map (\(_,x,_) -> x) rows
     , map (\(_,_,g) -> g) rows
     )

ioTests :: [IO Test]
ioTests = [ ipdSimpleTest ]

ipdSimpleTest :: IO Test
ipdSimpleTest = do
  let name = "LMM spring-EM: synthetic IPD meta-analysis y ~ age + trt + (1|study)"
      (ys, xs, grps) = flatten
      lmm = mkLMMFromRows ys xs True grps
      fit = fitLMM lmm
      bInt = fitBeta fit ! 0
      bAge = fitBeta fit ! 1
      bTrt = fitBeta fit ! 2
      tau2 = fitTau2 fit
      s2e  = fitSigma2E fit
      sprs = extractSprings lmm fit
      shr  = filter ((== Shrinkage) . spKind) sprs
  putStr $ "\n  === Synthetic IPD meta-analysis (3 trials × 4 patients) ==="
  putStr $ "\n  model:  y  ~  age  +  trt  +  (1|study)"
  putStr $ "\n  truth:  β = (2, 0.2, 6),  u = (0, -5, 0)"
  putStr $ "\n  fit:"
  putStr $ "\n    β_Int = " ++ show (roundDouble bInt 4)  ++ "  (truth: 2)"
  putStr $ "\n    β_age = " ++ show (roundDouble bAge 4)  ++ "  (truth: 0.2)"
  putStr $ "\n    β_trt = " ++ show (roundDouble bTrt 4)  ++ "  (truth: 6)"
  putStr $ "\n    τ²    = " ++ show (roundDouble tau2 4)  ++ "  (pop var of u = 8.333)"
  putStr $ "\n    σ²_ε  = " ++ show (roundDouble s2e  4)
  putStr $ "\n  EM iterations = " ++ show (fitIters fit)
  putStr $ "\n  --- Springs ---"
  putStr $ "\n" ++ summarizeSprings sprs
  putStr $ "\n  Study BLUPs (shrinkage-spring lengths):"
  mapM_ (\s -> putStr $ "\n    " ++ spLabel s
                     ++ ": u="      ++ show (roundDouble (spLength s) 3)
                     ++ "  energy=" ++ show (roundDouble (spEnergy s) 4)
        ) shr
  putStr $ "\n"
  let okB = abs (bInt - 2.0) < 0.5
         && abs (bAge - 0.2) < 0.05
         && abs (bTrt - 6.0) < 0.5
      okTau = tau2 > 0 && tau2 < 50
      okS2e = s2e  >= 0 && s2e  < 5
  if okB && okTau && okS2e
    then return $ testPassed name $
           "β≈(" ++ show (roundDouble bInt 2)
        ++ ", "  ++ show (roundDouble bAge 3)
        ++ ", "  ++ show (roundDouble bTrt 2) ++ ")"
        ++ "  τ²=" ++ show (roundDouble tau2 2)
        ++ "  σ²=" ++ show (roundDouble s2e 3)
    else return $ testFailed name
           ( "β≈(2, 0.2, 6), τ²>0, σ²≥0"
           , "β=(" ++ show bInt ++ "," ++ show bAge ++ "," ++ show bTrt ++ ")"
             ++ " τ²=" ++ show tau2 ++ " σ²=" ++ show s2e
           )
