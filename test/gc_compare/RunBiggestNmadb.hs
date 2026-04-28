{-# LANGUAGE OverloadedStrings #-}
-- Spring GC on the biggest nmadb binary NMA (nmadb_501194, 99 studies, 17 trts).
-- Saves FULL posterior: τ² grid with log Z, plus effect mean+var for every
-- (from, to) treatment pair at every τ² grid point. Timer forced to measure
-- real wall-clock (Haskell-lazy evaluation is defeated by forcing the output
-- JSON to be written).
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

-- Extract pairwise (from, to, mean, var) from a NetworkEffects
pairs :: NetworkEffects -> [(String, String, Double, Double)]
pairs ne =
  let ests = networkEstimates ne
      vars = networkVariances ne
  in  [ (show_ a, show_ b, m
        , Map.findWithDefault 0 b (Map.findWithDefault Map.empty a vars))
      | (a, row) <- Map.toList ests
      , (b, m)   <- Map.toList row
      , a /= b ]

main :: IO ()
main = do
  let path = "test/nmadb/nmadb_501194.json"
  est <- readStudies path
  case est of
    Left err      -> putStrLn $ "ERR: " ++ err
    Right studies -> do
      putStrLn $ show (length studies) ++ " studies loaded from " ++ path
      hFlush stdout
      -- Adaptive grid: find REML mode, log-space 100 points around it.
      let nPts = 100 :: Int
      t0 <- getCurrentTime
      case springGrandCanonicalBinAdaptive studies nPts of
        Left err -> putStrLn $ "GC err: " ++ err
        Right (modeTau2, gridRes) -> do
          -- Build JSON first — that forces every thunk in gridRes.
          let gridJson =
                [ object [ "tau2"    .= t2
                         , "logZ"    .= lz
                         , "effects" .=
                             [ object [ "from" .= a, "to" .= b
                                      , "mean" .= m,   "var" .= v ]
                             | (a, b, m, v) <- pairs ne
                             ]
                         ]
                | (t2, lz, ne) <- gridRes
                ]
              preOut = object
                [ "dataset"    .= path
                , "n_studies"  .= length studies
                , "mode_tau2"  .= modeTau2
                , "grid_type" .= ("adaptive" :: String)
                , "grid_nPts" .= nPts
                , "grid"       .= gridJson
                ]
              encoded = A.encode preOut
          -- `encoded` is a lazy ByteString, but `BL.length` forces it.
          let nBytes = BL.length encoded
          t1 <- getCurrentTime
          let wallSec = realToFrac (diffUTCTime t1 t0) :: Double
          putStrLn $ "GC wall (forced via JSON encode): "
                     ++ show wallSec ++ " s, mode τ² = "
                     ++ show modeTau2 ++ ", JSON size = "
                     ++ show nBytes
          BL.writeFile "test/gc_compare/biggest_nmadb_gc.json" encoded
          putStrLn "Wrote test/gc_compare/biggest_nmadb_gc.json"
