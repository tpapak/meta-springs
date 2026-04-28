{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- GC sweep over /tmp/t50_rare_bin/rep_NN.json (5 reps).  Three prior reweights
-- per rep (HN(0.5), HN(1), HC(0.5)).  Same output schema as proto_t30_reps_gc.hs.
module Main where

import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import qualified Data.ByteString.Lazy as BL
import           Data.List            (sortOn)
import qualified Data.Map.Strict      as Map
import           Data.Time.Clock      (getCurrentTime, diffUTCTime)
import           Text.Printf          (printf)

import Data.Meta.Effects       (TreatmentId (..), StringIntId (..))
import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects (..))
import Data.Meta.RandomEffects (springGrandCanonicalBinAdaptive,
                                Tau2Prior (..), posteriorUnder,
                                PosteriorSummary (..),
                                logTau2Prior)

priors :: [(String, Tau2Prior)]
priors =
  [ ("HalfNormal(τ; σ=0.5)",   HalfNormalTau 0.5)
  , ("HalfNormal(τ; σ=1)",     HalfNormalTau 1.0)
  , ("HalfCauchy(τ; σ=0.5)",   HalfCauchyTau 0.5)
  ]

gridWeights :: [Double] -> [Double] -> Tau2Prior -> [Double]
gridWeights ts lzs prior =
  let lps = zipWith (\t lz -> lz + logTau2Prior prior t + log (max t 1e-300))
                    ts lzs
      m   = maximum lps
      ws  = map (\lp -> exp (lp - m)) lps
      tot = sum ws
  in map (/ tot) ws

showTid :: TreatmentId -> String
showTid (TreatmentId (IntId n))    = show n
showTid (TreatmentId (StringId s)) = s

oneRep :: Int -> IO A.Value
oneRep rep = do
  let path = printf "/tmp/t50_rare_bin/rep_%02d.json" rep
  est <- readStudies path
  case est of
    Left err -> do
      printf "rep %d: read err: %s\n" rep err
      return A.Null
    Right studies -> do
      t0 <- getCurrentTime
      case springGrandCanonicalBinAdaptive studies 100 of
        Left err -> do
          printf "rep %d: GC err: %s\n" rep err
          return A.Null
        Right (_, gridFull) -> do
          let keep  = [ (t, lz, ne) | (t, lz, ne) <- gridFull, t > 0 ]
              ts    = [t  | (t,_,_) <- keep]
              lzs   = [lz | (_,lz,_) <- keep]
              nes   = [ne | (_,_,ne) <- keep]
              gridLZ = zip ts lzs

              ref :: TreatmentId
              ref = case keep of
                ((_,_,ne):_) ->
                  let allTids = Map.keys (networkEstimates ne)
                  in head (sortOn show allTids)
                _ -> error "empty grid"
              targets = case keep of
                ((_,_,ne):_) ->
                  filter (/= ref)
                    (sortOn show (Map.keys (networkEstimates ne)))
                _ -> []

              priorSummary (lbl, pr) =
                let ws = gridWeights ts lzs pr
                    ps = posteriorUnder True gridLZ pr
                    (lo, hi) = psCi95 ps
                    effOf tgt =
                      let mus  = [ (networkEstimates ne Map.! ref) Map.! tgt
                                 | ne <- nes ]
                          vs   = [ (networkVariances ne Map.! ref) Map.! tgt
                                 | ne <- nes ]
                          mu   = sum (zipWith (*) ws mus)
                          mxx  = sum (zipWith3 (\w m v -> w*(m*m + v)) ws mus vs)
                          sd_  = sqrt (max 0 (mxx - mu*mu))
                      in (showTid tgt, mu, sd_)
                    effs = map effOf targets
                in object
                  [ "label"  .= lbl
                  , "tau2"   .= object
                      [ "mode"   .= psMode ps
                      , "median" .= psMedian ps
                      , "mean"   .= psMean ps
                      , "ci_lo"  .= lo
                      , "ci_hi"  .= hi ]
                  , "effects" .=
                      [ object [ "to" .= tid, "mean" .= m, "sd" .= s ]
                      | (tid, m, s) <- effs ]
                  ]
              !out = map priorSummary priors
              !forced = BL.length (A.encode out)
          t1 <- forced `seq` getCurrentTime
          let wall = realToFrac (diffUTCTime t1 t0) :: Double
          printf "rep %02d: %.2fs (%d τ² pts)\n" rep wall (length gridFull)
          return $ object
            [ "rep"      .= rep
            , "wall_sec" .= wall
            , "priors"   .= out
            ]

main :: IO ()
main = do
  let nReps = 5 :: Int
  printf "GC T=50 rare-events: %d reps × 3 priors × 1 fit/rep\n\n" nReps
  tStart <- getCurrentTime
  results <- mapM oneRep [1 .. nReps]
  tEnd <- getCurrentTime
  let total = realToFrac (diffUTCTime tEnd tStart) :: Double
  printf "\nGC total wall: %.1fs (%d replicates)\n" total nReps
  let json = object
        [ "total_wall_sec" .= total
        , "n_reps"         .= nReps
        , "results"        .= results
        ]
  BL.writeFile "/tmp/t50_rare_gc.json" (A.encode json)
  putStrLn "Wrote /tmp/t50_rare_gc.json"
