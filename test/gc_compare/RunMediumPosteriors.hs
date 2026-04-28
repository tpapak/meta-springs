{-# LANGUAGE OverloadedStrings #-}
-- Dump adaptive-grid GC posterior for nmadb_474842 (45 studies × 8 trts):
-- τ² grid with log Z and every pairwise effect (mean + var) per grid point,
-- so R can reconstruct full marginal posteriors.
module Main where

import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import           Data.Time.Clock      (getCurrentTime, diffUTCTime)
import           System.IO            (hFlush, stdout)

import Data.Meta.Effects       (TreatmentId(..))
import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects(..))
import Data.Meta.RandomEffects (springGrandCanonicalBinAdaptive)

show_ :: TreatmentId -> String
show_ (TreatmentId t) = show t

pairs :: NetworkEffects -> [(String, String, Double, Double)]
pairs ne =
  let ests = networkEstimates ne
      vars = networkVariances ne
  in  [ (show_ a, show_ b, m,
         Map.findWithDefault 0 b (Map.findWithDefault Map.empty a vars))
      | (a, row) <- Map.toList ests
      , (b, m)   <- Map.toList row
      , a /= b ]

main :: IO ()
main = do
  let path = "test/nmadb/nmadb_474842.json"
  est <- readStudies path
  case est of
    Left err -> putStrLn $ "ERR: " ++ err
    Right studies -> do
      putStrLn $ show (length studies) ++ " studies from " ++ path
      hFlush stdout
      t0 <- getCurrentTime
      case springGrandCanonicalBinAdaptive studies 100 of
        Left err -> putStrLn $ "GC err: " ++ err
        Right (modeTau2, gridRes) -> do
          let gridJson =
                [ object [ "tau2"    .= t2
                         , "logZ"    .= lz
                         , "effects" .=
                             [ object [ "from" .= a, "to" .= b
                                      , "mean" .= m,   "var" .= v ]
                             | (a, b, m, v) <- pairs ne ]
                         ]
                | (t2, lz, ne) <- gridRes ]
              preOut = object
                [ "dataset"    .= path
                , "n_studies"  .= length studies
                , "mode_tau2"  .= modeTau2
                , "grid_type"  .= ("adaptive" :: String)
                , "grid_nPts"  .= length gridRes
                , "grid"       .= gridJson
                ]
              encoded = A.encode preOut
          let nBytes = BL.length encoded
          t1 <- getCurrentTime
          let wallSec = realToFrac (diffUTCTime t1 t0) :: Double
          putStrLn $ "GC wall: " ++ show wallSec
                     ++ " s, mode τ² = " ++ show modeTau2
                     ++ ", JSON size = " ++ show nBytes ++ " bytes"
          BL.writeFile "test/gc_compare/medium_nmadb_gc.json" encoded
          putStrLn "Wrote test/gc_compare/medium_nmadb_gc.json"
