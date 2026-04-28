{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Prior investigation on the published diabetes NMA (Dias et al.).
-- 22 studies, 6 treatments (ACE, ARB, BBlocker, CCB, Diuretic, Placebo),
-- 4 three-arm trials.  One GC fit; reweight under 4 priors and dump:
--   - τ² posterior summary + full grid (τ², logZ, weight)
--   - per-contrast (vs Diuretic ref) mixture mean+sd
-- Output /tmp/diabetes_gc.json.
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
                                springREMLBin,
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

main :: IO ()
main = do
  let path = "test/diabetes.json"
  Right studies <- readStudies path
  printf "diabetes: %d studies\n" (length studies)
  t0 <- getCurrentTime
  case springGrandCanonicalBinAdaptive studies 100 of
    Left e -> putStrLn ("err: " ++ e)
    Right (_, gridFull) -> do
      let keep = [ (t, lz, ne) | (t, lz, ne) <- gridFull, t > 0 ]
          ts   = [t  | (t,_,_) <- keep]
          lzs  = [lz | (_,lz,_) <- keep]
          nes  = [ne | (_,_,ne) <- keep]
          gridLZ = zip ts lzs

          ref :: TreatmentId
          ref = case keep of
            ((_,_,ne):_) ->
              let allTids = Map.keys (networkEstimates ne)
              in head (sortOn show allTids)
            _ -> error "empty"
          targets = case keep of
            ((_,_,ne):_) ->
              filter (/= ref)
                (sortOn show (Map.keys (networkEstimates ne)))
            _ -> []
          !forceLZ = sum lzs
      t1 <- forceLZ `seq` getCurrentTime
      let fitWall = realToFrac (diffUTCTime t1 t0) :: Double

      -- Per-prior summary; also dump per-contrast point-by-grid mass.
      let priorJson (lbl, pr) =
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
                      pts  =
                        [ object [ "tau2" .= t, "weight" .= w
                                 , "mean" .= m, "var" .= v ]
                        | (t, m, v, w) <- zip4 ts mus vs ws ]
                  in (showTid tgt, mu, sd_, pts)
                effs = map effOf targets
            in object
              [ "label" .= lbl
              , "tau2"  .= object
                  [ "mode"   .= psMode ps
                  , "median" .= psMedian ps
                  , "mean"   .= psMean ps
                  , "ci_lo"  .= lo
                  , "ci_hi"  .= hi ]
              , "tau2_grid" .=
                  [ object [ "tau2" .= t, "weight" .= w ]
                  | (t, w) <- zip ts ws ]
              , "effects" .=
                  [ object [ "to" .= tid, "mean" .= m, "sd" .= s
                           , "points" .= pts ]
                  | (tid, m, s, pts) <- effs ]
              ]
          out = map priorJson priors
          -- Iterative REML mode (bisection on dlogZ/dτ², no grid argmax bias).
          -- Mirrors gc-rust's `reml_mode_iterative` field.
          remlIter = case springREMLBin studies Nothing (Just 500) (Just 1e-6) of
            Right (_, (tHat, _)) -> tHat
            Left _ -> 0/0
          json = object
            [ "dataset"             .= path
            , "fit_wall_sec"        .= fitWall
            , "ref"                 .= showTid ref
            , "reml_mode_iterative" .= remlIter
            , "priors"              .= out
            ]
      BL.writeFile "/tmp/diabetes_gc.json" (A.encode json)
      printf "GC fit + 4 priors: %.3f s\nWrote /tmp/diabetes_gc.json\n" fitWall
  where
    zip4 [] _ _ _ = []
    zip4 _ [] _ _ = []
    zip4 _ _ [] _ = []
    zip4 _ _ _ [] = []
    zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds
