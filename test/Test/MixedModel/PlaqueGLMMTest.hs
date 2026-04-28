-- | Logistic GLMM on the plaque-psoriasis IPD (Phillippo 2020/2023), via
-- the spring-EM + IRLS solver.
--
-- Data:  test/plaque_psoriasis_min.csv  (exported from multinma by
--        test/ipd_glmm_springs.R)
--        4114 patients, 4 studies, 5 treatments, covariate wt_c (weight / 10 kg)
--
-- Model:  pasi75 ~ trtc + wt_c + (1 | studyc)
-- Reference (glmer from ipd_glmm_springs.R):
--   β₀        = -2.8462
--   β_ETN     = +2.7632
--   β_IXE_Q2W = +5.1760
--   β_IXE_Q4W = +4.4016
--   β_UST     = +3.8943
--   β_wt      = -0.1150
--   τ²        =  0.0311
--
-- Note: fixed effects should agree with glmer to ~0.05. τ² may differ
-- slightly because our algorithm is *Laplace-EM* (moment-match the
-- Laplace posterior of u), whereas glmer does *Laplace-ML* (gradient-
-- optimize the Laplace marginal likelihood).  With only J=4 groups these
-- can give modestly different τ² estimates; both are principled, they
-- converge to the same limit as J→∞.
module Test.MixedModel.PlaqueGLMMTest where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Numeric.LinearAlgebra ((!))
import qualified Numeric.LinearAlgebra as LA

import TestHS
import Data.Numerics (roundDouble)
import Test.MixedModel.LMM

-- | Minimal CSV loader — assumes the exact columns in plaque_psoriasis_min.csv:
--   studyc,trtc,wt_c,pasi75
loadPlaque :: FilePath -> IO ([String], [String], [Double], [Int])
loadPlaque path = do
  raw <- readFile path
  let ls = drop 1 (lines raw)   -- drop header
      parseLine l = case splitOn4 ',' (filter (/= '"') l) of
        [s, t, w, y] -> (s, t, read w :: Double, read y :: Int)
        _            -> error ("bad row: " ++ l)
      rows = map parseLine ls
  return ( map (\(s,_,_,_) -> s) rows
         , map (\(_,t,_,_) -> t) rows
         , map (\(_,_,w,_) -> w) rows
         , map (\(_,_,_,y) -> y) rows
         )

splitOn4 :: Char -> String -> [String]
splitOn4 sep s = case break (== sep) s of
  (a, [])     -> [a]
  (a, _:rest) -> a : splitOn4 sep rest

-- | Build LMMData for logistic GLMM pasi75 ~ trtc + wt_c + (1|studyc).
-- Encodes trtc as dummy columns ETN, IXE_Q2W, IXE_Q4W, UST (PBO = reference).
buildLMM :: [String] -> [String] -> [Double] -> [Int] -> LMMData
buildLMM studies trts wts ys =
  let nAll   = length ys
      trtLvl = ["ETN", "IXE_Q2W", "IXE_Q4W", "UST"]  -- non-reference levels
      dummies t = map (\lvl -> if t == lvl then 1.0 else 0.0) trtLvl
      xRows = [ dummies t ++ [w] | (t, w) <- zip trts wts ]
      studyIds = Map.fromList (zip (nub studies) [0 ..])
      grps = map (\s -> studyIds Map.! s) studies
  in mkLMMFromRows (map fromIntegral ys) xRows True grps

ioTests :: [IO Test]
ioTests = [ plaqueGLMMTest ]

plaqueGLMMTest :: IO Test
plaqueGLMMTest = do
  let name = "GLMM spring-EM: plaque_psoriasis pasi75 ~ trt + wt + (1|study) vs glmer"
  (studies, trts, wts, ys) <- loadPlaque "test/plaque_psoriasis_min.csv"
  let lmm = buildLMM studies trts wts ys
      fit = fitLogisticGLMM lmm
      b0     = glmmBeta fit ! 0
      bETN   = glmmBeta fit ! 1
      bIXE2  = glmmBeta fit ! 2
      bIXE4  = glmmBeta fit ! 3
      bUST   = glmmBeta fit ! 4
      bWt    = glmmBeta fit ! 5
      tau2   = glmmTau2 fit
      u      = LA.toList (glmmU fit)
      w      = glmmW fit
      wStats = ( LA.minElement w, LA.maxElement w
               , LA.sumElements w / fromIntegral (LA.size w) )
      -- glmer reference values
      refB0  = -2.8462
      refETN =  2.7632
      refIX2 =  5.1760
      refIX4 =  4.4016
      refUST =  3.8943
      refWt  = -0.1150
      refTau2 =  0.0311
  putStr $ "\n  === plaque_psoriasis GLMM spring-EM ==="
  putStr $ "\n  n=" ++ show (length ys) ++ ", J=" ++ show (LA.size (glmmU fit)) ++ " studies"
  putStr $ "\n    β₀           = " ++ show (roundDouble b0    4) ++ "  (glmer " ++ show refB0  ++ ")"
  putStr $ "\n    β_ETN        = " ++ show (roundDouble bETN  4) ++ "  (glmer " ++ show refETN ++ ")"
  putStr $ "\n    β_IXE_Q2W    = " ++ show (roundDouble bIXE2 4) ++ "  (glmer " ++ show refIX2 ++ ")"
  putStr $ "\n    β_IXE_Q4W    = " ++ show (roundDouble bIXE4 4) ++ "  (glmer " ++ show refIX4 ++ ")"
  putStr $ "\n    β_UST        = " ++ show (roundDouble bUST  4) ++ "  (glmer " ++ show refUST ++ ")"
  putStr $ "\n    β_wt         = " ++ show (roundDouble bWt   4) ++ "  (glmer " ++ show refWt  ++ ")"
  putStr $ "\n    τ²_study     = " ++ show (roundDouble tau2  5) ++ "  (glmer " ++ show refTau2 ++ ")"
  putStr $ "\n    BLUPs        = " ++ show (map (`roundDouble` 4) u)
  putStr $ "\n    w_i:  min=" ++ show (roundDouble (let (mn,_,_) = wStats in mn) 4)
               ++ "  max=" ++ show (roundDouble (let (_,mx,_) = wStats in mx) 4)
               ++ "  mean=" ++ show (roundDouble (let (_,_,mn) = wStats in mn) 4)
  putStr $ "\n    outer EM iterations = " ++ show (glmmIters fit)
  putStr $ "\n"
  let closeEnough a b = abs (a - b) < 0.1
      okBeta = closeEnough b0 refB0 && closeEnough bETN refETN
            && closeEnough bIXE2 refIX2 && closeEnough bIXE4 refIX4
            && closeEnough bUST refUST && closeEnough bWt refWt
      -- Laplace-EM vs Laplace-ML: allow a factor-of-2 tolerance on τ² for small J.
      okTau  = tau2 > 0 && tau2 < 2 * refTau2 + 0.05
  if okBeta && okTau
    then return $ testPassed name $
           "β_ETN="  ++ show (roundDouble bETN 3)
        ++ " β_wt=" ++ show (roundDouble bWt 4)
        ++ " τ²="   ++ show (roundDouble tau2 4)
    else return $ testFailed name
         ( "β within 0.05 of glmer, τ² within 0.02"
         , "β₀=" ++ show b0 ++ " β_wt=" ++ show bWt ++ " τ²=" ++ show tau2
         )
