{-# LANGUAGE OverloadedStrings #-}
-- Dump GC posterior grid + effect (μ) mean/var at mode for test/binary.json.
module Main where

import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map

import Data.Meta.Effects       (TreatmentId(..))
import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects(..))
import Data.Meta.RandomEffects (springGrandCanonicalBin)

main :: IO ()
main = do
  est <- readStudies "test/binary.json"
  case est of
    Left err      -> putStrLn $ "ERR: " ++ err
    Right studies ->
      case springGrandCanonicalBin studies 1000 4000 of
        Left err                      -> putStrLn $ "GC err: " ++ err
        Right (modeTau2, grid, modeNE) -> do
          let ests  = networkEstimates modeNE
              vars  = networkVariances modeNE
              -- pull μ for B vs A (treat "1" = A reference, "2" = B)
              show_ (TreatmentId t) = show t
              pairs = [ (show_ a, show_ b, m, Map.findWithDefault 0 b
                          (Map.findWithDefault Map.empty a vars))
                      | (a, row) <- Map.toList ests
                      , (b, m) <- Map.toList row
                      , a /= b ]
              muAB = case filter (\(a,b,_,_) -> a == "A" && b == "B") pairs of
                       ((_,_,m,v):_) -> (m, v)
                       _ -> case filter (\(a,b,_,_) -> a == "1" && b == "2") pairs of
                              ((_,_,m,v):_) -> (m, v)
                              _ -> (0, 1)
              (muMean, muVar) = muAB
              gridObjs = [ object ["tau2" .= t2, "logZ" .= lz]
                         | (t2, lz) <- grid ]
          let out = object
                [ "dataset"     .= ("test/binary.json" :: String)
                , "mode_tau2"   .= modeTau2
                , "grid"        .= gridObjs
                , "mu_at_mode"  .= object ["mean" .= muMean, "var" .= muVar]
                , "effect_pairs" .=
                    [ object ["from" .= a, "to" .= b, "mean" .= m, "var" .= v]
                    | (a, b, m, v) <- pairs ]
                ]
          BL.writeFile "test/gc_compare/pairwise_gc.json" (A.encode out)
          putStrLn $ "Wrote test/gc_compare/pairwise_gc.json  (mode τ² = "
                     ++ show modeTau2 ++ ", μ_mean = " ++ show muMean ++ ")"
