{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- For one T=50 rare-events replicate, dump the FULL Haskell GC posterior
-- under HalfNormal(τ; σ=1):
--   • the (τ², logZ, prior_weight) grid
--   • per-contrast (mean, sd) at each grid point
-- Argv: <repIdx>.  Output /tmp/t50_post_gc_haskell.json.
module Main where

import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import qualified Data.ByteString.Lazy as BL
import           Data.List            (sortOn)
import qualified Data.Map.Strict      as Map
import           System.Environment   (getArgs)
import           Text.Printf          (printf)

import Data.Meta.Effects       (TreatmentId (..), StringIntId (..))
import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects (..))
import Data.Meta.RandomEffects (springGrandCanonicalBinAdaptive,
                                logTau2Prior, Tau2Prior (..))

prior :: Tau2Prior
prior = HalfNormalTau 1.0
priorLabel :: String
priorLabel = "HalfNormal(τ; σ=1)"

showTid :: TreatmentId -> String
showTid (TreatmentId (IntId n))    = show n
showTid (TreatmentId (StringId s)) = s

main :: IO ()
main = do
  args <- getArgs
  let rep = case args of [s] -> read s :: Int; _ -> 4
      path = printf "/tmp/t50_rare_bin/rep_%02d.json" rep
  Right studies <- readStudies path
  printf "T=50 rare GC dump: rep %d (%d studies)\n" rep (length studies)
  case springGrandCanonicalBinAdaptive studies 100 of
    Left e -> putStrLn ("err: " ++ e)
    Right (_, gridFull) -> do
      let keep = [ (t, lz, ne) | (t, lz, ne) <- gridFull, t > 0 ]
          ts   = [t  | (t,_,_) <- keep]
          lzs  = [lz | (_,lz,_) <- keep]
          nes  = [ne | (_,_,ne) <- keep]
          lps  = zipWith (\t lz -> lz + logTau2Prior prior t + log (max t 1e-300))
                         ts lzs
          m    = maximum lps
          ws   = map (\lp -> exp (lp - m)) lps
          tot  = sum ws
          ws'  = map (/ tot) ws
          ref0 = head $ sortOn show $
                   case nes of (n:_) -> Map.keys (networkEstimates n); _ -> []
          tgts = filter (/= ref0) $ sortOn show $
                   case nes of (n:_) -> Map.keys (networkEstimates n); _ -> []
          contrastPoints tgt =
            [ object [ "tau2" .= t, "weight" .= w
                     , "mean" .= ((networkEstimates ne Map.! ref0) Map.! tgt)
                     , "var"  .= ((networkVariances ne Map.! ref0) Map.! tgt) ]
            | (t, ne, w) <- zip3 ts nes ws' ]
          json = object
            [ "rep"         .= rep
            , "engine"      .= ("haskell" :: String)
            , "prior_label" .= priorLabel
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
      BL.writeFile "/tmp/t50_post_gc_haskell.json" (A.encode json)
      printf "Wrote /tmp/t50_post_gc_haskell.json (%d grid pts × %d contrasts)\n"
             (length ts) (length tgts)
