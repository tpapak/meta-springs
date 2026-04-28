{-# LANGUAGE OverloadedStrings #-}
-- Dump spring-GC τ² grid + full effects table at the posterior mode for
-- test/nmadb/nmadb_479650.json (27 studies, 6 treatments, real binary NMA).
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
  let path = "test/nmadb/nmadb_479650.json"
  est <- readStudies path
  case est of
    Left err      -> putStrLn $ "ERR: " ++ err
    Right studies -> do
      putStrLn $ show (length studies) ++ " studies loaded"
      -- kTau=500, nMax=5000 -> τ² grid 0..10.0, step 0.002
      case springGrandCanonicalBin studies 500 5000 of
        Left err                      -> putStrLn $ "GC err: " ++ err
        Right (modeTau2, grid, modeNE) -> do
          let ests = networkEstimates modeNE
              vars = networkVariances modeNE
              show_ (TreatmentId t) = show t
              pairs = [ (show_ a, show_ b, m
                        , Map.findWithDefault 0 b
                            (Map.findWithDefault Map.empty a vars))
                      | (a, row) <- Map.toList ests
                      , (b, m)   <- Map.toList row
                      , a /= b ]
              -- thin grid by every 5 to keep JSON small
              gridObjs =
                [ object ["tau2" .= t2, "logZ" .= lz]
                | (i, (t2, lz)) <- zip [0::Int ..] grid, i `mod` 5 == 0
                ]
          let out = object
                [ "dataset"      .= path
                , "mode_tau2"    .= modeTau2
                , "grid"         .= gridObjs
                , "grid_kTau"    .= (500 :: Int)
                , "grid_nMax"    .= (5000 :: Int)
                , "effect_pairs" .=
                    [ object ["from" .= a, "to" .= b
                             , "mean" .= m, "var" .= v]
                    | (a, b, m, v) <- pairs ]
                ]
          BL.writeFile "test/gc_compare/real_bin_gc.json" (A.encode out)
          putStrLn $ "Wrote test/gc_compare/real_bin_gc.json "
                     ++ "(mode τ² = " ++ show modeTau2 ++ ")"
