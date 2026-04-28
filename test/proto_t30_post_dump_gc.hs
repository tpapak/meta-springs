{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- For one replicate, dump the FULL GC posterior under HalfNormal(τ;σ=1):
--   • the (τ², logZ, prior_weight) grid → reproducible τ² density
--   • per-contrast (mean, sd) at each grid point → contrast mixture density
-- Reads /tmp/t30_reps_<regime>/rep_NN.json, writes /tmp/t30_post_<regime>_gc.json.
-- Two arguments: <regime> (bin|cont) <repIdx>.
module Main where

import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import qualified Data.ByteString.Lazy as BL
import           Data.List            (sortOn)
import qualified Data.Map.Strict      as Map
import           System.Environment   (getArgs)
import           Text.Printf          (printf)

import Data.Meta.Effects       (TreatmentId (..), StringIntId (..), meanArm)
import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects (..))
import Data.Meta.RandomEffects (springGrandCanonicalBinAdaptive,
                                springGrandCanonicalAdaptive,
                                logTau2Prior, Tau2Prior (..))

prior :: Tau2Prior
prior = HalfNormalTau 1.0
priorLabel :: String
priorLabel = "HalfNormal(τ; σ=1)"

showTid :: TreatmentId -> String
showTid (TreatmentId (IntId n))    = show n
showTid (TreatmentId (StringId s)) = s

dumpGrid :: String -> [(Double, Double, NetworkEffects)] -> IO ()
dumpGrid outPath gridFull = do
  let keep = [ (t, lz, ne) | (t, lz, ne) <- gridFull, t > 0 ]
      ts   = [ t | (t, _, _) <- keep ]
      lzs  = [ lz | (_, lz, _) <- keep ]
      nes  = [ ne | (_, _, ne) <- keep ]
      lps  = zipWith (\t lz -> lz + logTau2Prior prior t + log (max t 1e-300))
                     ts lzs
      m    = maximum lps
      ws   = map (\lp -> exp (lp - m)) lps
      tot  = sum ws
      ws'  = map (/ tot) ws
      -- target list (excludes ref)
      ref0 = head $ sortOn show $
               case nes of (n:_) -> Map.keys (networkEstimates n); _ -> []
      tgts = filter (/= ref0) $ sortOn show $
               case nes of (n:_) -> Map.keys (networkEstimates n); _ -> []
      -- per-contrast posterior at each grid point
      contrastPoints tgt =
        [ object
            [ "tau2"    .= t
            , "weight"  .= w
            , "mean"    .= ((networkEstimates ne Map.! ref0) Map.! tgt)
            , "var"     .= ((networkVariances ne Map.! ref0) Map.! tgt)
            ]
        | (t, ne, w) <- zip3 ts nes ws' ]
      json = object
        [ "prior_label" .= priorLabel
        , "grid_n"      .= length ts
        , "tau2"        .= ts
        , "logZ"        .= lzs
        , "weight"      .= ws'
        , "ref"         .= showTid ref0
        , "contrasts"   .=
            [ object [ "to" .= showTid tgt
                     , "points" .= contrastPoints tgt ]
            | tgt <- tgts ]
        ]
  BL.writeFile outPath (A.encode json)
  printf "Wrote %s  (%d grid pts × %d contrasts)\n"
         outPath (length ts) (length tgts)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [regime, repIdxStr] -> do
      let rep = read repIdxStr :: Int
          dataPath = printf "/tmp/t30_reps_%s/rep_%02d.json" regime rep
          outPath  = printf "/tmp/t30_post_%s_gc.json" regime
      Right studies <- readStudies dataPath
      printf "GC posterior dump: regime=%s rep=%d (%d studies)\n"
             regime rep (length studies)
      case regime of
        "bin" ->
          case springGrandCanonicalBinAdaptive studies 100 of
            Left e -> putStrLn ("err: " ++ e)
            Right (_, gridFull) -> dumpGrid outPath gridFull
        "cont" ->
          case springGrandCanonicalAdaptive studies meanArm 100 of
            Left e -> putStrLn ("err: " ++ e)
            Right (_, gridFull) -> dumpGrid outPath gridFull
        _ -> putStrLn $ "unknown regime: " ++ regime
    _ -> putStrLn "usage: proto_t30_post_dump_gc <bin|cont> <repIdx>"
