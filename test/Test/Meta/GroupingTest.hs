{-# LANGUAGE BangPatterns #-}
-- | Smoke tests for the grafted tau-grouping estimator.
--
-- Exercises 'springREMLBinGrouped' and (indirectly) 'setTauGroups':
--   1. A single group (Nothing classifier) must reproduce the ungrouped
--      'springREMLBin' estimate.
--   2. A genuine two-group split must run end-to-end and return a finite,
--      non-negative tau-squared per group.
-- Uses the Dong2013 COPD network from "Test.Meta.SpringREMLTest".
module Test.Meta.GroupingTest where

import TestHS
import qualified Data.Map.Strict as Map

import Data.Numerics
import Data.Meta.Effects hiding (null)
import Data.Meta.Studies (StudyId)
import Data.Meta.NMA (TauGroup)
import Data.Meta.RandomEffects

import Test.Meta.SpringREMLTest (dong2013, tPlac)

ioTests :: [IO Test]
ioTests = [ groupedMatchesUngrouped, groupedTwoGroupsRuns ]

finiteNonNeg :: Double -> Bool
finiteNonNeg x = x >= 0 && not (isNaN x) && not (isInfinite x)

-- | One group (mult 2, symmetric) should match the ungrouped estimator.
groupedMatchesUngrouped :: IO Test
groupedMatchesUngrouped = do
  let name = "tau-grouping: single group reproduces ungrouped springREMLBin"
      ungrouped = springREMLBin dong2013 Nothing (Just 1000) (Just 1e-6)
      grouped   = springREMLBinGrouped dong2013 Nothing
                    (Map.fromList [(0, 2.0)])   -- symmetric multiplier
                    (Map.fromList [(0, 0.5)])   -- initial tau^2
                    (Just 1000) (Just 1e-6)
  case (ungrouped, grouped) of
    (Left e, _) -> return $ testFailed name ("springREMLBin error", e)
    (_, Left e) -> return $ testFailed name ("springREMLBinGrouped error", e)
    (Right (_, (t2u, _)), Right (_, (t2map, _))) -> do
      let t2g = Map.findWithDefault (-1) 0 t2map
          d   = abs (t2g - t2u)
      putStr $ "\n  ungrouped tau^2 = " <> show (roundDouble t2u 6)
            <> ", grouped(1) tau^2 = " <> show (roundDouble t2g 6)
            <> ", |diff| = " <> show (roundDouble d 6) <> "\n"
      if finiteNonNeg t2g && d < 5e-2
        then return $ testPassed name ("match within 5e-2: " <> show (roundDouble d 6))
        else return $ testFailed name ("grouped~=ungrouped (|diff|<5e-2)", show d)

-- | A real two-group split (Placebo-anchored vs the rest) must run and yield
-- finite, non-negative tau^2 in every group.
groupedTwoGroupsRuns :: IO Test
groupedTwoGroupsRuns = do
  let name = "tau-grouping: two-group split runs with finite tau^2 per group"
      classify :: StudyId -> TreatmentId -> TauGroup
      classify _ tid = if tid == tPlac then 0 else 1
      res = springREMLBinGrouped dong2013 (Just classify)
              (Map.fromList [(0, 2.0), (1, 2.0)])
              (Map.fromList [(0, 0.5), (1, 0.5)])
              (Just 1000) (Just 1e-6)
  case res of
    Left e -> return $ testFailed name ("springREMLBinGrouped error", e)
    Right (_, (t2map, _)) -> do
      let vals = Map.elems t2map
      putStr $ "\n  per-group tau^2 = "
            <> show [ (g, roundDouble v 6) | (g, v) <- Map.toList t2map ] <> "\n"
      if not (null vals) && all finiteNonNeg vals
        then return $ testPassed name (show (Map.size t2map) <> " groups, all finite>=0")
        else return $ testFailed name ("finite>=0 tau^2 per group", show vals)
