module Main where

import qualified Data.Map.Strict as Map
import Data.Meta.Effects
import Data.Meta.Studies
import Data.Meta.NMA
import Data.Meta.RandomEffects (springREML)

arm :: String -> Double -> Arm
arm name m = ContinuousArm (TreatmentId (StringId name)) m 2.0 100   -- sd=2,n=100 => v=0.04

mkStudy :: String -> [(String,Double)] -> Study
mkStudy sid as = ContinuousStudy (StudyId (StringId sid)) [ arm n m | (n,m) <- as ]

-- (1) network with no covariate -> should reduce to RE NMA (springREML / netmeta REML)
netStudies' :: [Study]
netStudies' =
  [ mkStudy "S1" [("A",0.0),("B",1.0)], mkStudy "S2" [("A",0.0),("B",1.6)]
  , mkStudy "S3" [("A",0.0),("C",2.0)], mkStudy "S4" [("A",0.0),("C",2.6)]
  , mkStudy "S5" [("B",0.0),("C",0.8)], mkStudy "S6" [("B",0.0),("C",1.4)] ]

cov0 :: Map.Map StudyId Double
cov0 = Map.fromList [ (getStudyId s, 0.0) | s <- netStudies' ]

-- (2) two-treatment meta-regression vs metafor::rma(REML): contrast = d + beta*x
regContrasts :: [(String,Double,Double)]   -- (study, B-arm mean = contrast, covariate x)
regContrasts =
  [ ("S1",0.60,0), ("S2",0.70,1), ("S3",1.25,2)
  , ("S4",1.25,3), ("S5",1.75,4), ("S6",1.95,5) ]

regStudies :: [Study]
regStudies = [ mkStudy s [("A",0.0),("B",c)] | (s,c,_) <- regContrasts ]

regCov :: Map.Map StudyId Double
regCov = Map.fromList [ (StudyId (StringId s), x) | (s,_,x) <- regContrasts ]

t :: String -> TreatmentId
t = TreatmentId . StringId

main :: IO ()
main = do
  putStrLn "=== (1) covariate = 0: reduce to RE NMA ==="
  case springREML netStudies' meanArm (Just 0.5) Nothing Nothing of
    Right (_, (tau2, _)) -> putStrLn ("springREML  tau2 = " ++ show tau2)
    Left e -> putStrLn e
  putStrLn "netmeta REML tau2 = 0.0566667 ; A->B=1.26667 A->C=2.33333  (external)"
  case nmaRegressionRE netStudies' cov0 (Just 0.5) meanArm of
    Right (beta, tau2, cs) -> do
      putStrLn ("nmaRegressionRE tau2 = " ++ show tau2 ++ "  beta = " ++ show beta)
      putStrLn ("  A->B = " ++ show (cs Map.! t "A" Map.! t "B")
                ++ "  A->C = " ++ show (cs Map.! t "A" Map.! t "C"))
    Left e -> putStrLn e

  putStrLn "\n=== (2) RE meta-regression on x  (compare to metafor REML) ==="
  case nmaRegressionRE regStudies regCov (Just 0.1) meanArm of
    Right (beta, tau2, cs) -> do
      putStrLn ("nmaRegressionRE  beta = " ++ show beta
                ++ "  intercept(A->B at x=0) = " ++ show (cs Map.! t "A" Map.! t "B")
                ++ "  tau2 = " ++ show tau2)
    Left e -> putStrLn e
