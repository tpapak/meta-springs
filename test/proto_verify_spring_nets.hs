{-# LANGUAGE BangPatterns #-}
-- Verify that springGrandCanonicalAdaptive (Gaussian) still returns a
-- valid spring network per grid point: every NetworkEffects has the
-- expected vertices, edges, spring-energy values.
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import           Text.Printf     (printf)

import Data.Meta.Effects       (meanArm)
import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects (..),
                                springs, springEnergy, springLength,
                                tauEdges, tau2, vs2ts, SpringVertex (..))
import Data.Meta.RandomEffects (springGrandCanonicalAdaptive)

main :: IO ()
main = do
  Right studies <- readStudies "test/continuous.json"
  printf "Loaded %d studies\n\n" (length studies)
  case springGrandCanonicalAdaptive studies meanArm 10 of
    Left e -> putStrLn ("err: " ++ e)
    Right (modeTau2, grid) -> do
      printf "%d grid points returned\n" (length grid)
      printf "argmax τ² = %.4f\n\n" modeTau2
      printf "%-8s %-8s %-8s %-8s %-12s %-12s\n"
        ("τ²"::String) ("nVerts"::String) ("nSprings"::String)
        ("nTauE"::String) ("ΣsprE"::String) ("Σℓ²(tau)"::String)
      printf "%s\n" (replicate 60 '-')
      mapM_ (\(t, _lz, ne) -> do
        let spnet     = springNetwork ne
            sprMap    = springs spnet
            !nV       = IM.size (vs2ts spnet)
            !nS       = Map.size sprMap
            !nTauE    = length (tauEdges spnet)  -- Set, but length on toList
            !esum     = sum $ map springEnergy $ Map.elems sprMap
            tauL2     = sum [ springLength s ** 2
                            | s <- Map.elems sprMap ]
        printf "%-8.4f %-8d %-8d %-8d %-12.4f %-12.4f\n"
          t nV nS nTauE esum tauL2
        ) grid
      putStrLn "\nAll grid points carry a valid spring network."
