-- | Shortest-path mirror of @nma-contribution/test/Test/NMA.hs@ for the
-- slmm port. Loads each upstream hat-matrix fixture and checks the
-- shortest-path-only thresholds (test1 / test2 / testdiabetes4 / test8 /
-- teststreams), plus a synthetic per-study redistribution check.
--
-- Run:
--   stack runghc test/contribution/smoke.hs

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe)
import           System.Exit          (exitFailure, exitSuccess)

import qualified Data.Meta.Contribution as C
import qualified Data.Meta.Effects      as MetaEff
import qualified Data.Meta.Hatmatrix    as Hat

-- ─────────────────────────────────────────────────────────────────────
-- Fixture loader
-- ─────────────────────────────────────────────────────────────────────

loadHM :: String -> IO Hat.HatMatrix
loadHM name = do
  raw <- B.readFile ("test/contribution/" ++ name)
  case eitherDecode raw :: Either String Hat.HatMatrixRaw of
    Left e   -> fail ("parse " ++ name ++ ": " ++ e)
    Right xs -> pure (Hat.hatMatrixFromList xs)

-- ─────────────────────────────────────────────────────────────────────
-- Upstream test parity (shortest-path only)
-- ─────────────────────────────────────────────────────────────────────

-- | nma-contribution test1 — read every fixture.
testRead :: IO Bool
testRead = do
  mapM_ loadHM
    [ "diabetes_indrhat.json"
    , "big_widehat.json"
    , "cipriani_2011hat.json"
    , "Donghat.json"
    , "Leuchthat.json"
    ]
  pure True

-- | nma-contribution test2 — build per-row residual graph.
testBuildGraph :: IO Bool
testBuildGraph = do
  hm <- loadHM "big_widehat.json"
  let comp = MetaEff.ComparisonId
              (MetaEff.TreatmentId (MetaEff.StringId "agom"))
              (MetaEff.TreatmentId (MetaEff.StringId "amit"))
  case Map.lookup comp hm of
    Nothing  -> pure False
    Just row -> do
      let g = C.mkHatRowGraph comp row
      pure (C.source g == MetaEff.TreatmentId (MetaEff.StringId "agom")
         && C.sink   g == MetaEff.TreatmentId (MetaEff.StringId "amit"))

-- | nma-contribution testdiabetes4 — contributionRow with shortestStream
-- produces a finite, positive sum on diabetes 2:3.
testDiabetesRow :: IO Bool
testDiabetesRow = do
  hm <- loadHM "diabetes_indrhat.json"
  let comp = MetaEff.ComparisonId
              (MetaEff.TreatmentId (MetaEff.IntId 2))
              (MetaEff.TreatmentId (MetaEff.IntId 3))
  case Map.lookup comp hm of
    Nothing  -> pure False
    Just row ->
      let cr = C.contributionRowShortestPath comp row
          s  = C.sumContributionRow cr
      in pure (not (isNaN s) && s > 0)

-- | big_widehat sums to 1 ± 0.001 with shortestStream.
testGrizeldaSums :: IO Bool
testGrizeldaSums = do
  hm <- loadHM "big_widehat.json"
  let C.ContributionMatrix cm = C.contributionMatrixShortestPath hm
      sums = map C.sumContributionRow (Map.elems cm)
      bad  = [ s | s <- sums, abs (s - 1) >= 0.001 ]
  pure (null bad)

-- | nma-contribution test8 — Leucht residues < 0.0006 with shortestStream.
testLeuchtResidueShortest :: IO Bool
testLeuchtResidueShortest = do
  hm <- loadHM "Leuchthat.json"
  let (_, _, gs) = C.contributionMatrix hm
      maxFlow = maximum (0 : concatMap C.residualFlows (Map.elems gs))
  pure (maxFlow < 0.0006)

-- | nma-contribution teststreams — streams non-empty.
testStreamsNonEmpty :: IO Bool
testStreamsNonEmpty = do
  hm <- loadHM "diabetes_indrhat.json"
  let (_, C.StreamMatrix sm, _) = C.contributionMatrix hm
  pure (any (not . null) (Map.elems sm))

-- ─────────────────────────────────────────────────────────────────────
-- Per-study contribution sanity check (synthetic triangle)
-- ─────────────────────────────────────────────────────────────────────

testStudyContributions :: IO Bool
testStudyContributions = do
  let mkT n = MetaEff.TreatmentId (MetaEff.StringId n)
      tA = mkT "A"; tB = mkT "B"; tC = mkT "C"
      cAB = MetaEff.ComparisonId tA tB
      cBC = MetaEff.ComparisonId tB tC
      cAC = MetaEff.ComparisonId tA tC
      row :: Hat.HatMatrixRow
      row = Map.fromList [(cAB, 1/3), (cBC, 1/3), (cAC, 2/3)]
      hm  = Map.singleton cAC row
      cm  = C.contributionMatrixShortestPath hm
      ws  =
        [ C.PairwiseWeight "s1" cAC 1
        , C.PairwiseWeight "s1" cAB 1
        , C.PairwiseWeight "s1" cBC 1
        , C.PairwiseWeight "s2" cAB 3
        , C.PairwiseWeight "s2" cBC 3
        ]
      table = C.studyContributions cm ws
      val st = fromMaybe 0
                 (lookup st [(C.scStudy r, C.scContribution r) | r <- table])
      s1 = val "s1"
      s2 = val "s2"
      expectedS1 = 2/3 + 0.25 * (1/6) + 0.25 * (1/6)
      expectedS2 = 0.75 * (1/6) + 0.75 * (1/6)
      ok = abs (s1 - expectedS1) < 1e-9
        && abs (s2 - expectedS2) < 1e-9
        && abs ((s1 + s2) - 1) < 1e-9
  pure ok

-- ─────────────────────────────────────────────────────────────────────
-- Driver
-- ─────────────────────────────────────────────────────────────────────

run :: String -> IO Bool -> IO Bool
run name action = do
  ok <- action
  putStrLn ((if ok then "  ok   " else "  FAIL ") ++ name)
  pure ok

main :: IO ()
main = do
  putStrLn "Data.Meta.Contribution — shortest-path test parity"
  results <-
    sequence
      [ run "test1  read all fixtures"                     testRead
      , run "test2  build graph (agom:amit / big_widehat)" testBuildGraph
      , run "test4  diabetes 2:3 row finite sum (shortest)" testDiabetesRow
      , run "test5  big_widehat sums to 1 (shortest)"      testGrizeldaSums
      , run "test8  Leucht residue < 6e-4 (shortest)"      testLeuchtResidueShortest
      , run "tstrm  streams non-empty (diabetes)"          testStreamsNonEmpty
      , run "study  per-study redistribution (triangle)"   testStudyContributions
      ]
  if and results
    then do
      putStrLn $ show (length results) ++ "/" ++ show (length results) ++ " tests passed"
      exitSuccess
    else exitFailure
