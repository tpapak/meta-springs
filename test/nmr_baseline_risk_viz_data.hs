{-# LANGUAGE OverloadedStrings #-}
-- Dump the fitted baseline-risk spring network (certolizumab + simulation) to JSON for the
-- macas.lan visualization. Real fit from nmaRegressionBaselineRiskBin (incl. latent η_i).
module Main where

import Data.List (sort, nub, intercalate)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..), tidOfArm, Arm (..))
import Data.Meta.Studies (StudyId (..), getStudyId, getStudyArms, readStudies, Study (..))
import Data.Meta.NMA (nmaRegressionBaselineRiskBinB, BaselineSlopeModel (..), BaselineRiskFit (..))

nmT :: TreatmentId -> String
nmT (TreatmentId (StringId s)) = s
nmT (TreatmentId (IntId i)) = show i
nmS :: StudyId -> String
nmS (StudyId (StringId s)) = s
nmS (StudyId (IntId i)) = show i

num :: Double -> String
num x = printf "%.5f" x

obj :: [(String, String)] -> String
obj kvs = "{" ++ intercalate "," [ "\"" ++ k ++ "\":" ++ v | (k, v) <- kvs ] ++ "}"
str :: String -> String
str s = "\"" ++ s ++ "\""
arr :: [String] -> String
arr xs = "[" ++ intercalate "," xs ++ "]"

logodds :: Int -> Int -> Double
logodds e n = let rd = if e == 0 then 0.5 else fromIntegral e
                  nd = if e == 0 then fromIntegral n + 1 else fromIntegral n
                  p  = rd / nd
              in log (p / (1 - p))

dumpDataset :: String -> FilePath -> String -> IO String
dumpDataset name path refName = do
  Right studies <- readStudies path
  let ref = TreatmentId (StringId refName)
      treats = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      nonref = filter (/= ref) treats
  case nmaRegressionBaselineRiskBinB studies ref BLShared Nothing of
    Right fit -> do
      let mx = brMx fit
          -- per-study positions: baseline node η_i (shared fit) and the observed contrast
          studyJson s =
            let sid = getStudyId s; arms = getStudyArms s
                ctrl = head [ (ev, ns) | BinaryArm t ev ns <- arms, t == ref ]
                (at, ae, an) = head [ (t, ev, ns) | BinaryArm t ev ns <- arms, t /= ref ]
            in obj [ ("id", str (nmS sid)), ("active", str (nmT at))
                   , ("eta", num (brEta fit Map.! sid))
                   , ("ctrlLO", num (uncurry logodds ctrl)), ("actLO", num (logodds ae an))
                   , ("ctrlE", show (fst ctrl)), ("ctrlN", show (snd ctrl))
                   , ("actE", show ae), ("actN", show an) ]
          -- one fitted model (shared/unrelated/exchangeable): per-treatment d_t and slope b_t
          modelJson f = obj
            [ ("B", num (brB f)), ("Bse", num (brBse f))
            , ("sigmaB2", maybe "null" num (brSigmaB2 f))
            , ("d",  obj [ (nmT t, num (brD f  Map.! t)) | t <- nonref ])
            , ("bt", obj [ (nmT t, num (brBt f Map.! t)) | t <- nonref ])
            , ("btse", obj [ (nmT t, num (brBtSe f Map.! t)) | t <- nonref ]) ]
          fitFor m = either (const (obj [("err",str "fail")])) modelJson
                       (nmaRegressionBaselineRiskBinB studies ref m Nothing)
      return $ obj
        [ ("name", str name), ("ref", str refName), ("mx", num mx)
        , ("treats", arr (map (str . nmT) nonref))
        , ("models", obj [ ("shared", fitFor BLShared)
                         , ("unrelated", fitFor BLUnrelated)
                         , ("exchangeable", fitFor BLExchangeable) ])
        , ("studies", arr (map studyJson studies)) ]
    _ -> return (obj [("name", str name), ("error", str "fit failed")])

main :: IO ()
main = do
  cert <- dumpDataset "Certolizumab (real)" "test/nmr_certolizumab.json" "Placebo"
  sim  <- dumpDataset "Simulation (truth B=-0.60)" "test/nmr_br_sim.json" "A"
  let out = obj [ ("certolizumab", cert), ("sim", sim) ]
  writeFile "test/nmr_baseline_risk_viz.json" out
  putStrLn "Wrote test/nmr_baseline_risk_viz.json"
