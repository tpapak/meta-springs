{-# LANGUAGE BangPatterns #-}
-- | Debug spring-REML iteration
module Test.Meta.SpringREMLDebug where

import Data.Either
import Data.List
import qualified Data.Graph.AdjacencyList as G
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.IO (hFlush, stdout)

import TestHS
import Data.Numerics
import Data.Meta.Effects
import Data.Meta.NMA
import Data.Meta.RandomEffects
import Data.Meta.Studies

ioTests :: [IO Test]
ioTests = [debugSpringREML]

debugSpringREML :: IO Test
debugSpringREML = do
  let name = "Debug spring-REML Newton iteration (viecht1)"
  let studiesFile = "test/viecht1.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name ("Error reading studies", err)
    Right studies -> do
      let espringnet = makeSprings studies Nothing (Just 0.5) meanArm
      case espringnet of
        Left err -> return $ testFailed name ("makeSprings error", err)
        Right springsnet0 -> do
          putStr $ "\n  === Debug spring-REML (viecht1) ==="

          -- Evaluate at the known-good τ² = 0.19878
          let t2 = 0.19878
          let neteffs = nma $ updateSprings springsnet0 t2
              spnet = springNetwork neteffs
              studyVs = netStudies spnet
              k = fromIntegral $ length studyVs :: Double
              sprs = springs spnet
              tauEs = tauEdges spnet
              σ2map = studyWithinVariance spnet
              σ2s = map (σ2map Map.!) studyVs

          putStr $ "\n\n  --- Per-study diagnostics at τ² = " <> show t2 <> " ---"
          putStr $ "\n  k = " <> show (length studyVs) <> " studies"

          -- Collect per-study info
          let studyData = zipWith (\σ2 sv ->
                let -- find tau vertices for this study
                    tauVerts = map (\(G.Edge u v) -> if u == sv then v else u)
                             $ filter (\(G.Edge u v) -> u == sv || v == sv)
                             $ Set.toList tauEs
                    -- for each tau vertex, get the direct spring residual
                    armResiduals = map (\tv ->
                      let dspr = head $ Map.elems $
                            Map.filterWithKey
                              (\e@(G.Edge u v) _ -> (u == tv || v == tv) &&
                                not (Set.member e tauEs) &&
                                not (Set.member (G.reverseEdge e) tauEs))
                              sprs
                       in (naturalLength dspr, springLength dspr,
                           naturalLength dspr - springLength dspr)
                      ) tauVerts
                    yi_minus_muhat = sum $ map (\(_, _, r) -> r) armResiduals
                    -- δ_i = (1/σ²)(μ̂ - y_i) / (1/σ² + 1/τ²)
                    --      = -(y_i - μ̂) / (1 + σ²/τ²)
                    δi = (-yi_minus_muhat) / (1 + σ2 / t2)
                 in (sv, σ2, armResiduals, yi_minus_muhat, δi)
                ) σ2s studyVs

          -- Compute leave-one-out κ_i
          let ws = map (\σ2 -> 1 / (σ2 + t2)) σ2s
              sumW = sum ws
              κs = zipWith (\σ2 wi ->
                let sumW_minus_i = sumW - wi
                    varMuHat_i = 1 / sumW_minus_i
                 in 1 / σ2 + 1 / (t2 + varMuHat_i)
                ) σ2s ws

          -- Print per-study details
          mapM_ (\((sv, σ2, armRes, yi_m_mu, δi), κi) -> do
            putStr $ "\n\n  Study vertex " <> show sv <> ":"
            putStr $ "\n    σ_i² = " <> show (roundDouble σ2 6)
            putStr $ "\n    Arm-level springs:"
            mapM_ (\(nl, sl, res) ->
              putStr $ "\n      naturalLength=" <> show (roundDouble nl 6)
                    <> "  springLength=" <> show (roundDouble sl 6)
                    <> "  residual(nl-sl)=" <> show (roundDouble res 6)
              ) armRes
            putStr $ "\n    y_i - μ̂ (sum of arm residuals) = " <> show (roundDouble yi_m_mu 6)
            putStr $ "\n    δ_i (spring-REML) = " <> show (roundDouble δi 6)
            putStr $ "\n    δ_i² = " <> show (roundDouble (δi^2) 6)
            putStr $ "\n    κ_i = " <> show (roundDouble κi 6)
            putStr $ "\n    1/κ_i = " <> show (roundDouble (1/κi) 6)
            ) (zip studyData κs)

          -- Totals
          let δs = map (\(_, _, _, _, δi) -> δi) studyData
              sumDeltaSq = sum $ map (^2) δs
              sumInvKappa = sum $ map (1/) κs
          putStr $ "\n\n  --- Totals ---"
          putStr $ "\n  (1/k) Σ δ_i² = " <> show (roundDouble (sumDeltaSq / k) 6)
          putStr $ "\n  (1/k) Σ (1/κ_i) = " <> show (roundDouble (sumInvKappa / k) 6)
          putStr $ "\n  τ²_new = (1/k)Σδ² + (1/k)Σ(1/κ) = "
                <> show (roundDouble (sumDeltaSq / k + sumInvKappa / k) 6)

          -- ======================================================
          -- R-equivalent computation
          -- ======================================================
          putStr $ "\n\n  === R-equivalent deltais computation ==="

          -- y_i = observed study effect = sum of naturalLength of direct springs per study
          -- For pairwise (2-arm), this is the contrast (meanB - meanA)
          let yis = map (\sv ->
                let tauVerts = map (\(G.Edge u v) -> if u == sv then v else u)
                             $ filter (\(G.Edge u v) -> u == sv || v == sv)
                             $ Set.toList tauEs
                    armNLs = map (\tv ->
                      let dspr = head $ Map.elems $
                            Map.filterWithKey
                              (\e@(G.Edge u v) _ -> (u == tv || v == tv) &&
                                not (Set.member e tauEs) &&
                                not (Set.member (G.reverseEdge e) tauEs))
                              sprs
                       in naturalLength dspr
                      ) tauVerts
                 in sum armNLs
                ) studyVs

          -- k_i = 1/σ_i²
          let kis = map (\σ2 -> 1 / σ2) σ2s
              kτ = 1 / t2

          -- θ̂ = Σ(k_i·k_τ/(k_i+k_τ) · y_i) / Σ(k_i·k_τ/(k_i+k_τ))
          let wiStars = zipWith (\ki yi -> ki * kτ / (ki + kτ)) kis yis
              thetaHat = sum (zipWith (*) wiStars yis) / sum wiStars

          -- δ_i_R = k_i(θ̂ - y_i)/(k_i + k_τ)
          let deltaR = zipWith (\ki yi -> ki * (thetaHat - yi) / (ki + kτ)) kis yis

          putStr $ "\n  k_τ = 1/τ² = " <> show (roundDouble kτ 6)
          putStr $ "\n  θ̂ (weighted mean) = " <> show (roundDouble thetaHat 6)

          putStr $ "\n\n  --- Per-study: R-equivalent vs spring δ_i ---"
          mapM_ (\(i, (sv, σ2, _, _, δi_spr), yi, ki, δi_r) -> do
            putStr $ "\n  Study " <> show i <> " (vertex " <> show sv <> "):"
            putStr $ "\n    y_i = " <> show (roundDouble yi 6)
            putStr $ "\n    k_i = 1/σ² = " <> show (roundDouble ki 6)
            putStr $ "\n    w_i* = k_i·k_τ/(k_i+k_τ) = "
                  <> show (roundDouble (ki * kτ / (ki + kτ)) 6)
            putStr $ "\n    δ_i (R)      = " <> show (roundDouble δi_r 6)
            putStr $ "\n    δ_i (spring) = " <> show (roundDouble δi_spr 6)
            putStr $ "\n    difference   = " <> show (roundDouble (δi_spr - δi_r) 6)
            ) (zip5' [1..] studyData yis kis deltaR)

          let sumDeltaSqR = sum $ map (^2) deltaR
              sumInvKappaR = sumInvKappa  -- κ_i is the same formula
          putStr $ "\n\n  --- R-equivalent totals ---"
          putStr $ "\n  (1/k) Σ δ_i_R² = " <> show (roundDouble (sumDeltaSqR / k) 6)
          putStr $ "\n  (1/k) Σ (1/κ_i) = " <> show (roundDouble (sumInvKappaR / k) 6)
          putStr $ "\n  τ²_new (R) = " <> show (roundDouble (sumDeltaSqR / k + sumInvKappaR / k) 6)
          putStr $ "\n  τ²_new (spring) = " <> show (roundDouble (sumDeltaSq / k + sumInvKappa / k) 6)

          putStr $ "\n"
          hFlush stdout
          return $ testPassed name "debug output"

-- | Zip five lists together
zip5' :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5' (a:as) (b:bs) (c:cs) (d:ds) (e:es) = (a,b,c,d,e) : zip5' as bs cs ds es
zip5' _ _ _ _ _ = []
