-- | Verify the explicit SpringNetwork against an **external** reference.
--
-- Since `fitLMM` now calls `solveSpringNetwork` internally, comparing them
-- would be tautological.  Instead we pin the variance components to
-- `lme4::lmer`'s REML values for Dyestuff, build the network ourselves,
-- solve it, and check that β̂ and the BLUPs match lme4's published
-- Dyestuff numbers.  If any detail of spring assembly or the A·x = b
-- bookkeeping were wrong, this would fail.
module Test.MixedModel.SpringNetTest where

import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra (cols)

import TestHS
import Data.Numerics (roundDouble)
import Test.MixedModel.LMM
import Test.MixedModel.SpringNet

-- Dyestuff data (Davies 1967) — same 30 numbers lme4 has.
dyestuff :: ([Double], [Int])
dyestuff =
  ( concat
      [ [1545, 1440, 1440, 1520, 1580]
      , [1540, 1555, 1490, 1560, 1495]
      , [1595, 1550, 1605, 1510, 1560]
      , [1445, 1440, 1595, 1465, 1545]
      , [1595, 1630, 1515, 1635, 1625]
      , [1520, 1455, 1450, 1480, 1445]
      ]
  , concatMap (replicate 5) [1..6 :: Int]
  )

ioTests :: [IO Test]
ioTests = [ networkMatchesLme4Reference ]

-- lme4 REML on Dyestuff, from the R fit we ran earlier:
--   β₀ = 1527.5, Var(Batch) = 1764.05, Var(Residual) = 2451.25.
-- At those variance components, lme4's BLUPs are:
--   u = (-17.607, +0.391, +28.562, -23.085, +56.733, -44.995)
networkMatchesLme4Reference :: IO Test
networkMatchesLme4Reference = do
  let name = "SpringNetwork on Dyestuff @ lme4 (τ²=1764.05, σ²=2451.25) → β=1527.5"
      (ys, grps)   = dyestuff
      lmm          = mkLMMFromRows ys [] True grps
      -- Pin variance components to lme4's REML values.
      tau2Ref      = 1764.05
      sigma2ERef   = 2451.25
      net          = buildSpringNetwork (lmmY lmm) (lmmX lmm) (lmmZ lmm)
                                        sigma2ERef tau2Ref
      (xSol, _)    = solveSpringNetworkWithVar net
      p            = cols (lmmX lmm)
      q            = cols (lmmZ lmm)
      betaSol      = LA.subVector 0 p xSol
      uSol         = LA.subVector p q xSol
      refBeta0     = 1527.5  :: Double
      refBLUPs     = [-17.607, 0.391, 28.562, -23.085, 56.733, -44.995]
      beta0        = LA.toList betaSol !! 0
      blups        = LA.toList uSol
      dBeta        = abs (beta0 - refBeta0)
      dBLUPs       = maximum (zipWith (\a b -> abs (a - b)) blups refBLUPs)
  putStr $ "\n  === SpringNetwork on Dyestuff at lme4 reference (τ², σ²_ε) ==="
  putStr $ "\n" ++ summarizeNetwork net
  putStr $ "\n  β from spring-network solve = "
                ++ show (roundDouble beta0 6)
                ++ "   (lme4: " ++ show refBeta0 ++ ")"
  putStr $ "\n  BLUPs from network solve = "
                ++ show (map (`roundDouble` 3) blups)
  putStr $ "\n  BLUPs from lme4          = " ++ show refBLUPs
  putStr $ "\n  |β diff|      = " ++ show (roundDouble dBeta 6)
  putStr $ "\n  max |u diff|  = " ++ show (roundDouble dBLUPs 4)
  putStr $ "\n"
  if dBeta < 1e-4 && dBLUPs < 1e-2
    then return $ testPassed name $
           "β=" ++ show (roundDouble beta0 4)
        ++ ", max |BLUP − lme4_BLUP| = " ++ show (roundDouble dBLUPs 4)
    else return $ testFailed name
           ( "β≈1527.5, BLUPs match lme4 to 1e-2"
           , "β=" ++ show beta0 ++ ", max BLUP diff = " ++ show dBLUPs )
