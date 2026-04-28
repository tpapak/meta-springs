{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Gaussian counterpart of proto_t30_reps_gc.hs.  Reads /tmp/t30_reps_cont/
-- rep_NN.json (mean/sd/n arm summaries), runs springGrandCanonicalAdaptive
-- with meanArm, computes posterior summaries under 4 priors. Same output
-- schema as the binary runner.
module Main where

import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import qualified Data.ByteString.Lazy as BL
import           Data.List            (sortOn)
import qualified Data.Map.Strict      as Map
import           Data.Time.Clock      (getCurrentTime, diffUTCTime)
import           Text.Printf          (printf)

import Data.Meta.Effects       (TreatmentId (..), StringIntId (..), meanArm)
import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects (..))
import Data.Meta.RandomEffects (springGrandCanonicalAdaptive,
                                springREML,
                                Tau2Prior (..), posteriorUnder,
                                PosteriorSummary (..),
                                logTau2Prior)

priors :: [(String, Tau2Prior)]
priors =
  [ ("Flat (REML)",            FlatTau2)
  , ("HalfNormal(τ; σ=0.5)",   HalfNormalTau 0.5)
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
  let path = printf "/tmp/t30_reps_cont/rep_%02d.json" rep
  est <- readStudies path
  case est of
    Left err -> do
      printf "rep %d: read err: %s\n" rep err
      return A.Null
    Right studies -> do
      t0 <- getCurrentTime
      case springGrandCanonicalAdaptive studies meanArm 500 of
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
              -- Force the entire posterior structure (psMode/psMean/effs)
              -- inside the timed block; otherwise lazy evaluation reports 0s.
              !forced = BL.length (A.encode out)
              remlIter = case springREML studies meanArm Nothing (Just 500) (Just 1e-6) of
                Right (_, (tHat, _)) -> tHat
                Left _ -> 0/0
          t1 <- forced `seq` getCurrentTime
          let wall = realToFrac (diffUTCTime t1 t0) :: Double
          printf "rep %02d: %.2fs (%d τ² pts, %d B)\n"
                 rep wall (length gridFull) forced
          return $ object
            [ "rep"                 .= rep
            , "wall_sec"            .= wall
            , "reml_mode_iterative" .= remlIter
            , "priors"              .= out
            ]

main :: IO ()
main = do
  let nReps = 30
  printf "GC continuous sweep: %d reps × %d priors × 1 fit/replicate\n\n"
         nReps (length priors)
  tStart <- getCurrentTime
  results <- mapM oneRep [1 .. nReps]
  tEnd <- getCurrentTime
  let total = realToFrac (diffUTCTime tEnd tStart) :: Double
  printf "\nGC continuous total wall: %.1fs (%d replicates)\n" total nReps
  let json = object
        [ "total_wall_sec" .= total
        , "n_reps"         .= nReps
        , "results"        .= results
        ]
  BL.writeFile "/tmp/t30_reps_cont_gc.json" (A.encode json)
  putStrLn "Wrote /tmp/t30_reps_cont_gc.json"
