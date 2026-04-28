-- | Test: Dyestuff (lme4) — spring-EM on a one-way variance-components LMM.
--
-- Model: @Yield ~ 1 + (1|Batch)@
-- Data:  6 batches × 5 yields = 30 observations (Davies 1967, @lme4::Dyestuff@).
-- Reference (lme4 REML):
--   β₀        = 1527.5
--   σ²_Batch  = 1764.05
--   σ²_resid  = 2451.25
module Test.MixedModel.DyestuffTest where

import Numeric.LinearAlgebra ((!))

import TestHS
import Data.Numerics (roundDouble)
import Test.MixedModel.LMM

dyestuff :: ([Double], [Int])
dyestuff =
  ( concat batches
  , concatMap (replicate 5) [1..6 :: Int]
  )
  where
    batches =
      [ [1545, 1440, 1440, 1520, 1580]  -- A
      , [1540, 1555, 1490, 1560, 1495]  -- B
      , [1595, 1550, 1605, 1510, 1560]  -- C
      , [1445, 1440, 1595, 1465, 1545]  -- D
      , [1595, 1630, 1515, 1635, 1625]  -- E
      , [1520, 1455, 1450, 1480, 1445]  -- F
      ]

ioTests :: [IO Test]
ioTests = [ dyestuffTest ]

dyestuffTest :: IO Test
dyestuffTest = do
  let name = "LMM spring-EM: Dyestuff (1|Batch) vs lme4"
      (ys, grps) = dyestuff
      lmm = mkLMMFromRows ys [] True grps
      fit = fitLMM lmm
      b0   = fitBeta fit ! 0
      tau2 = fitTau2 fit
      s2e  = fitSigma2E fit
      refB    = 1527.5  :: Double
      refTau2 = 1764.05 :: Double
      refS2e  = 2451.25 :: Double
      sprs = extractSprings lmm fit
      shrSprings = filter ((== Shrinkage) . spKind) sprs
  putStr $ "\n  === Dyestuff ==="
  putStr $ "\n  β₀     = " <> show (roundDouble b0   4) <> "  (lme4: " <> show refB    <> ")"
  putStr $ "\n  τ²     = " <> show (roundDouble tau2 4) <> "  (lme4: " <> show refTau2 <> ")"
  putStr $ "\n  σ²_ε   = " <> show (roundDouble s2e  4) <> "  (lme4: " <> show refS2e  <> ")"
  putStr $ "\n  EM iterations = " <> show (fitIters fit)
  putStr $ "\n  --- Springs ---"
  putStr $ "\n" <> summarizeSprings sprs
  putStr $ "\n  BLUPs per batch (shrinkage-spring lengths):"
  mapM_ (\s -> putStr $ "\n    " <> spLabel s
                     <> ": u="      <> show (roundDouble (spLength s) 3)
                     <> "  energy=" <> show (roundDouble (spEnergy s) 4)
        ) shrSprings
  putStr $ "\n"
  let okB    = abs (b0   - refB)    < 0.5
      okTau2 = abs (tau2 - refTau2) < 20.0
      okS2e  = abs (s2e  - refS2e)  < 20.0
  if okB && okTau2 && okS2e
    then return $ testPassed name $
           "β="    <> show (roundDouble b0   2)
        <> " τ²="  <> show (roundDouble tau2 2)
        <> " σ²ε=" <> show (roundDouble s2e  2)
    else return $ testFailed name
           ( "β≈" <> show refB <> " τ²≈" <> show refTau2 <> " σ²ε≈" <> show refS2e
           , "β=" <> show b0   <> " τ²=" <> show tau2   <> " σ²ε=" <> show s2e
           )
