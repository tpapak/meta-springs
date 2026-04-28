{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Compare studyDeltaVariances from irlsSolve (full Newton + nma) vs
-- newtonSolveReducedBin on the same network at fixed τ². If REML went bad
-- after switching, this will show the discrepancy.
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import           Text.Printf     (printf)

import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects (..), makeBinomialSprings,
                                updateSprings)
import Data.Meta.RandomEffects (irlsSolve, newtonSolveReducedBin)

main :: IO ()
main = do
  let path = "/tmp/t20.json"
  Right studies <- readStudies path
  let Right net0 = makeBinomialSprings studies Nothing (Just 1.0)
  mapM_ (\t2 -> do
    let net = updateSprings net0 t2
        neF = irlsSolve net 200 1e-9
        neR = newtonSolveReducedBin net 200 1e-9
        dvF = studyDeltaVariances neF
        dvR = studyDeltaVariances neR
    printf "\nτ² = %.3f\n" t2
    printf "  irlsSolve studyDeltaVariances summary:\n"
    let sumF = sum [v | vs <- IM.elems dvF, v <- vs]
        nF   = sum [length vs | vs <- IM.elems dvF]
    printf "    studies=%d  arms=%d  sum=%.4f  mean=%.4f\n"
      (IM.size dvF) nF sumF (sumF / fromIntegral (max 1 nF))
    let sumR = sum [v | vs <- IM.elems dvR, v <- vs]
        nR   = sum [length vs | vs <- IM.elems dvR]
    printf "  reducedNewton studyDeltaVariances summary:\n"
    printf "    studies=%d  arms=%d  sum=%.4f  mean=%.4f\n"
      (IM.size dvR) nR sumR (sumR / fromIntegral (max 1 nR))
    printf "  Δ sum = %+.4f\n" (sumR - sumF)
    -- Tau spring length squared sum (used as the BLUP component)
    let _ = neF; _ = neR
    return ()
    ) [0.1, 0.5, 1.0]
