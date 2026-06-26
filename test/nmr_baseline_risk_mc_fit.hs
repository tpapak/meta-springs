{-# LANGUAGE OverloadedStrings #-}
-- Fit FE shared-slope baseline-risk to every Monte-Carlo rep in /tmp/brmc, in one process.
-- Writes /tmp/brmc/haskell.json: [{rep,B,Bse,dB,dBse,dC,dCse}].
module Main where

import Text.Printf (printf)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Meta.Effects (TreatmentId (..), StringIntId (..))
import Data.Meta.Studies (readStudies)
import Data.Meta.NMA (nmaRegressionBaselineRiskBin, BaselineRiskFit (..))

t :: String -> TreatmentId
t s = TreatmentId (StringId s)

main :: IO ()
main = do
  Just truth <- (Just . words) <$> readFile "/tmp/brmc/nrep.txt"  -- single int
  let nrep = read (head truth) :: Int
  results <- mapM fitRep [1 .. nrep]
  writeFile "/tmp/brmc/haskell.json" $ "[" ++ intercalate "," (concat results) ++ "]"
  printf "fitted %d reps -> /tmp/brmc/haskell.json (%d converged)\n" nrep (length (concat results))
  where
    fitRep r = do
      let path = printf "/tmp/brmc/rep_%03d.json" r
      est <- readStudies path
      case est of
        Left _   -> return []
        Right st -> case nmaRegressionBaselineRiskBin st (t "A") of
          Left _    -> return []
          Right f   -> return [ obj
            [ ("rep", show r)
            , ("B",   num (brB f)),   ("Bse",  num (brBse f))
            , ("dB",  num (brD f Map.! t "B")), ("dBse", num (brDse f Map.! t "B"))
            , ("dC",  num (brD f Map.! t "C")), ("dCse", num (brDse f Map.! t "C"))
            , ("tau2", num (brTau2 f)) ] ]
    num x = printf "%.6f" x :: String
    obj kvs = "{" ++ intercalate "," [ "\"" ++ k ++ "\":" ++ v | (k,v) <- kvs ] ++ "}"
