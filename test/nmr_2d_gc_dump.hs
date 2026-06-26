{-# LANGUAGE OverloadedStrings #-}
-- Dump the EXACT linearised system for the 2-D grand-canonical exchangeable baseline-risk
-- model, so the HTML can do the real spring solve at every (tau2, sigmaB2): per-arm Jacobian
-- J, working response z, weight w; the coordinate layout; the tau2/sigmaB2 grids; and the
-- per-grid log-prior weights (matching nmaRegressionBaselineRiskBinExchGC exactly).
module Main where

import System.Environment (getArgs)
import Data.List (sort, nub, intercalate)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..), tidOfArm, Arm (..))
import Data.Meta.Studies (StudyId (..), getStudyId, getStudyArms, readStudies)
import Data.Meta.NMA (nmaRegressionBaselineRiskBinB, BaselineSlopeModel (..), BaselineRiskFit (..), sigmoid)

nmT :: TreatmentId -> String
nmT (TreatmentId (StringId s)) = s
nmT (TreatmentId (IntId i)) = show i
nmS :: StudyId -> String
nmS (StudyId (StringId s)) = s
nmS (StudyId (IntId i)) = show i

num :: Double -> String
num x | isNaN x || isInfinite x = "0"
      | otherwise = printf "%.6g" x
arr :: [String] -> String
arr xs = "[" ++ intercalate "," xs ++ "]"
obj :: [(String,String)] -> String
obj kvs = "{" ++ intercalate "," [ "\"" ++ k ++ "\":" ++ v | (k,v) <- kvs ] ++ "}"
str :: String -> String
str s = "\"" ++ s ++ "\""

-- half-normal log-prior on tau2/sigmaB2 (HalfNormalTauR 0.5), Lebesgue scale
lpHN :: Double -> Double -> Double
lpHN s t | t <= 0 = -1e300
         | otherwise = - t / (2*s*s) - 0.5 * log t

main :: IO ()
main = do
  args <- getArgs
  let (path, refName, out) = case args of
        (p:r:o:_) -> (p,r,o); (p:r:_) -> (p,r,"test/nmr_2d_gc_grid.json"); _ -> ("test/nmr_certolizumab.json","Placebo","test/nmr_2d_gc_grid.json")
      ref = TreatmentId (StringId refName)
  Right studies <- readStudies path
  case nmaRegressionBaselineRiskBinB studies ref BLExchangeable (Just 0.1) of
    Left e -> putStrLn ("fit failed: " ++ e)
    Right fit -> do
      let treatments = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
          nonref = filter (/= ref) treatments; nT = length nonref
          sids = map getStudyId studies; nS = length sids
          sidx = Map.fromList (zip sids [0..]); tidx = Map.fromList (zip nonref [0..])
          betaBase = nS + nT; dCol t = nS + tidx Map.! t; bCol t = betaBase + tidx Map.! t
          commonCol = betaBase + nT; nP = nS + 2*nT + 1
          mx = brMx fit
          theta i | i < nS        = brEta fit Map.! (sids !! i)
                  | i < nS + nT   = brD   fit Map.! (nonref !! (i - nS))
                  | i < nS + 2*nT = brBt  fit Map.! (nonref !! (i - nS - nT))
                  | otherwise     = brB   fit
          thetaHat = map theta [0..nP-1]
          setAt xs i v = take i xs ++ [v] ++ drop (i+1) xs
          armJSON study a = case a of
            BinaryArm t ev ns ->
              let si = sidx Map.! getStudyId study; isR = t == ref
                  etaI = thetaHat !! si; bT = if isR then 0 else thetaHat !! bCol t
                  ell = if isR then etaI else etaI + thetaHat !! dCol t + bT*(etaI - mx)
                  p = sigmoid ell; w = max 1e-9 (fromIntegral ns * p*(1-p))
                  jac | isR = setAt (replicate nP 0) si 1
                      | otherwise = setAt (setAt (setAt (replicate nP 0) si (1+bT)) (dCol t) 1) (bCol t) (etaI-mx)
                  z = sum (zipWith (*) jac thetaHat) + (fromIntegral ev - fromIntegral ns * p)/w
              in obj [ ("si", show si), ("ref", if isR then "true" else "false")
                     , ("t", if isR then "-1" else show (tidx Map.! t))
                     , ("J", arr (map num jac)), ("z", num z), ("w", num w) ]
            _ -> obj [("err",str "binary")]
          arms = [ armJSON s a | s <- studies, a <- getStudyArms s ]
          -- grids (display resolution); same shape as the library GC
          nTau = 26; nSig = 26
          tau2hi = max (brTau2 fit * 12) 0.02
          ts = [ tau2hi * (fromIntegral i - 0.5) / fromIntegral nTau | i <- [1..nTau] ]
          sigHi = 1.0; sLo = sigHi/1e4
          ss = [ sLo * (sigHi/sLo) ** ((fromIntegral j - 0.5)/fromIntegral nSig) | j <- [1..nSig] ]
          tauW = [ lpHN 0.5 t | t <- ts ]                                   -- prior on tau2
          sigW = [ -0.5*fromIntegral nT*log s + lpHN 0.5 s + log s | s <- ss ] -- -nT/2 log s + prior + log-jacobian
      writeFile out $ obj
        [ ("dataset", str (nmS (getStudyId (head studies)) ++ "…"))
        , ("nS", show nS), ("nT", show nT), ("nP", show nP), ("mx", num mx)
        , ("studies", arr (map (str . nmS) sids)), ("treats", arr (map (str . nmT) nonref))
        , ("etaCols", arr (map show [0..nS-1]))
        , ("dCols", arr (map (show . dCol) nonref))
        , ("slopeCols", arr (map (show . bCol) nonref))
        , ("commonCol", show commonCol)
        , ("arms", arr arms)
        , ("tauGrid", arr (map num ts)), ("sigGrid", arr (map num ss))
        , ("tauW", arr (map num tauW)), ("sigW", arr (map num sigW))
        , ("emB", num (brB fit)), ("emSig", num (maybe 0 id (brSigmaB2 fit)))
        ]
      putStrLn $ "Wrote " ++ out ++ "  (nP=" ++ show nP ++ ", arms=" ++ show (length arms)
                 ++ ", grid " ++ show nTau ++ "x" ++ show nSig ++ ")"
