{-# LANGUAGE BangPatterns #-}
-- Diagnostic: compute logZ from springGrandCanonicalBin at the REML τ² and
-- at the GC argmax τ². If logZ(REML_mode) >= logZ(GC_argmax) we have a
-- grid resolution issue. If logZ(GC_argmax) > logZ(REML_mode) by more than
-- noise, REML is converging to a different objective than GC's Laplace logZ.
module Main where

import qualified Data.Map.Strict as Map
import           Data.List       (sortBy)
import           Data.Ord        (comparing)
import           Text.Printf     (printf)

import Data.Meta.Studies       (readStudies)
import Data.Meta.RandomEffects (springGrandCanonicalBin, springREMLBin)

main :: IO ()
main = do
  Right studies <- readStudies "test/synth_bin/synth_041.json"
  let kTau = 500 :: Double; nMax = 4000
  case (springREMLBin studies Nothing (Just 500) (Just 1e-6),
        springGrandCanonicalBin studies kTau nMax) of
    (Right (_, (tauHatReml, _)), Right (gcArgmax, grid, _)) -> do
      let lookupLZ t2 =
            let (_, lz) = head $ sortBy (comparing (\(t, _) -> abs (t - t2))) grid
            in lz
          lzAtReml = lookupLZ tauHatReml
          lzAtGc   = lookupLZ gcArgmax
          gridStep = 1.0 / kTau
          -- approximate first/second derivatives at REML mode via finite diff
          h = max gridStep (tauHatReml * 0.01)
          lzPlus  = lookupLZ (tauHatReml + h)
          lzMinus = lookupLZ (max 1e-10 (tauHatReml - h))
          dlz     = (lzPlus - lzMinus) / (2 * h)
      printf "REML τ²            = %.6f\n" tauHatReml
      printf "GC argmax τ²       = %.6f\n" gcArgmax
      printf "logZ(REML mode)    = %.6f\n" lzAtReml
      printf "logZ(GC argmax)    = %.6f\n" lzAtGc
      printf "Δ logZ (GC − REML) = %.6f\n" (lzAtGc - lzAtReml)
      printf "d logZ / dτ² at REML mode (finite-diff h=%.4f) = %+.6f\n" h dlz
      putStrLn ""
      putStrLn "Interpretation:"
      if lzAtGc - lzAtReml > 0.05
        then putStrLn "  GC's Laplace logZ favours its own argmax over REML mode."
        else putStrLn "  Both modes have similar logZ — discrepancy is ~ grid noise."
      if abs dlz > 0.5
        then putStrLn "  Gradient of logZ at REML mode is large — REML is NOT a stationary point of GC's logZ."
        else putStrLn "  Gradient of logZ at REML mode is small — both methods near same stationary point."
    _ -> putStrLn "FAIL: solver error"
