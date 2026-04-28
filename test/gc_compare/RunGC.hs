{-# LANGUAGE OverloadedStrings #-}
-- Run springGrandCanonical / springGrandCanonicalBin on the 20 datasets
-- listed in test/gc_compare/datasets.json. Write one JSON summary per
-- dataset plus a combined summary.
module Main where

import qualified Data.Aeson              as A
import           Data.Aeson              ((.=), object)
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import           System.IO               (hFlush, stdout)

import           Data.Meta.Effects       (meanArm, TreatmentId(..))
import           Data.Meta.Studies       (readStudies)
import           Data.Meta.NMA           (NetworkEffects(..))
import           Data.Meta.RandomEffects (springGrandCanonical,
                                          springGrandCanonicalBin,
                                          springREML, springREMLBin)

data Likelihood = Gaussian | Binomial deriving (Eq, Show)

-- (name, likelihood, file, tau2_true_optional)
datasetList :: [(String, Likelihood, FilePath, Maybe Double)]
datasetList =
  [ ("synth_015", Binomial, "test/synth_bin/synth_015.json",   Just 0.0007)
  , ("synth_069", Binomial, "test/synth_bin/synth_069.json",   Just 0.0063)
  , ("synth_061", Binomial, "test/synth_bin/synth_061.json",   Just 0.1280)
  , ("synth_056", Binomial, "test/synth_bin/synth_056.json",   Just 0.1526)
  , ("synth_012", Binomial, "test/synth_bin/synth_012.json",   Just 0.2294)
  , ("synth_025", Binomial, "test/synth_bin/synth_025.json",   Just 0.2569)
  , ("synth_041", Binomial, "test/synth_bin/synth_041.json",   Just 0.3904)
  , ("synth_026", Binomial, "test/synth_bin/synth_026.json",   Just 0.5965)
  , ("synth_018", Binomial, "test/synth_bin/synth_018.json",   Just 1.1249)
  , ("synth_090", Binomial, "test/synth_bin/synth_090.json",   Just 1.2635)
  , ("cont_t6_01", Gaussian, "test/gc_compare/data/cont_t6_01.json", Just 0.001)
  , ("cont_t6_02", Gaussian, "test/gc_compare/data/cont_t6_02.json", Just 0.01)
  , ("cont_t6_03", Gaussian, "test/gc_compare/data/cont_t6_03.json", Just 0.05)
  , ("cont_t6_04", Gaussian, "test/gc_compare/data/cont_t6_04.json", Just 0.1)
  , ("cont_t6_05", Gaussian, "test/gc_compare/data/cont_t6_05.json", Just 0.15)
  , ("cont_t6_06", Gaussian, "test/gc_compare/data/cont_t6_06.json", Just 0.25)
  , ("cont_t6_07", Gaussian, "test/gc_compare/data/cont_t6_07.json", Just 0.4)
  , ("cont_t6_08", Gaussian, "test/gc_compare/data/cont_t6_08.json", Just 0.6)
  , ("cont_t6_09", Gaussian, "test/gc_compare/data/cont_t6_09.json", Just 1.0)
  , ("cont_t6_10", Gaussian, "test/gc_compare/data/cont_t6_10.json", Just 1.5)
  ]

kTau, nMaxGrid :: Int
kTau     = 500     -- step = 1/500 = 0.002
nMaxGrid = 5000    -- tau2 up to 10.0

-- Summarise log-posterior grid under a given prior.
summarise :: [(Double, Double)] -> (Double, Double, Double, Double, Double)
summarise grid =
  let logPs  = map snd grid
      maxL   = maximum logPs
      unnorm = map (\lp -> exp (lp - maxL)) logPs
      tot    = sum unnorm
      probs  = map (/ tot) unnorm
      t2s    = map fst grid
      mean_  = sum (zipWith (*) t2s probs)
      cum    = scanl1 (+) probs
      modeIx = snd $ foldr1 (\a b -> if fst a >= fst b then a else b)
                            (zip logPs [0 :: Int ..])
      medIx  = length (takeWhile (< 0.5)   cum)
      loIx   = length (takeWhile (< 0.025) cum)
      hiIx   = length (takeWhile (< 0.975) cum)
  in ( t2s !! modeIx, t2s !! medIx, mean_
     , t2s !! loIx,   t2s !! hiIx )

-- Half-Normal(0, 1) on tau: add to logZ on the tau2 grid.
-- log pi_tau2(tau2) = -(1/2) log(tau2) - tau2/2  + const
reweightHN :: [(Double, Double)] -> [(Double, Double)]
reweightHN grid =
  [ (t2, lz - 0.5 * log t2 - 0.5 * t2) | (t2, lz) <- grid, t2 > 0 ]

-- All non-trivial pairwise contrasts from NetworkEffects as (from, to, mean, var).
-- Includes both directions; downstream picks what's needed.
effectsAsList :: NetworkEffects -> [(String, String, Double, Double)]
effectsAsList ne =
  let est = networkEstimates ne
      var = networkVariances ne
      showTid (TreatmentId t) = show t
  in  [ (showTid a, showTid b, mu,
         Map.findWithDefault 0.0 b (Map.findWithDefault Map.empty a var))
      | (a, row) <- Map.toList est
      , (b, mu)  <- Map.toList row
      , a /= b
      ]

runOne :: (String, Likelihood, FilePath, Maybe Double) -> IO A.Value
runOne (name, lk, path, tau2True) = do
  putStr $ "[" ++ name ++ "] "; hFlush stdout
  est <- readStudies path
  case est of
    Left err -> do
      putStrLn $ "read err: " ++ err
      return $ object ["name" .= name, "error" .= err]
    Right studies -> do
      let nstudies = length studies
      putStr $ show nstudies ++ " studies; "; hFlush stdout

      -- 1. REML point estimate
      let remlRes = case lk of
            Binomial -> fmap (fst . snd) (springREMLBin studies Nothing (Just 500) (Just 1e-6))
            Gaussian -> fmap (fst . snd) (springREML studies meanArm Nothing (Just 500) (Just 1e-6))
          remlTau2 = case remlRes of { Right t -> Just t; Left _ -> Nothing }

      -- 2. Grand canonical
      let gcRes = case lk of
            Binomial -> springGrandCanonicalBin studies (fromIntegral kTau) nMaxGrid
            Gaussian -> springGrandCanonical studies meanArm (fromIntegral kTau) nMaxGrid

      case gcRes of
        Left err -> do
          putStrLn $ "GC err: " ++ err
          return $ object ["name" .= name, "error" .= err]
        Right (modeFlat, grid, modeNE) -> do
          let (m_f, med_f, mean_f, lo_f, hi_f) = summarise grid
              gridHN = reweightHN grid
              (_, med_h, mean_h, lo_h, hi_h)   = summarise gridHN
              modeHNTauSp =
                let tauLog = [ (sqrt t2, lz + 0.5 * log t2 - 0.5 * t2)
                             | (t2, lz) <- grid, t2 > 0 ]
                in fst $ foldr1 (\a b -> if snd a >= snd b then a else b) tauLog
              modeHN_t2 = modeHNTauSp * modeHNTauSp
              effs = effectsAsList modeNE

          putStrLn $ "REML="  ++ maybe "NA" (\v -> showRound v 4) remlTau2
                   ++ " GCmode=" ++ showRound modeFlat 4
                   ++ " HNmedian=" ++ showRound med_h 4
          return $ object
            [ "name"        .= name
            , "likelihood"  .= show lk
            , "n_studies"   .= nstudies
            , "tau2_true"   .= tau2True
            , "spring_reml_tau2" .= remlTau2
            , "tau2_flat"   .= object ["mode" .= m_f, "median" .= med_f,
                                       "mean" .= mean_f, "ci_lo" .= lo_f, "ci_hi" .= hi_f]
            , "tau2_halfnormal" .= object
                [ "mode_tau2_via_tau_space" .= modeHN_t2
                , "median" .= med_h, "mean" .= mean_h
                , "ci_lo"  .= lo_h,  "ci_hi" .= hi_h ]
            , "effects_at_gc_mode" .=
                [ object ["from" .= a, "to" .= b, "mean" .= mu, "var" .= v]
                | (a, b, mu, v) <- effs ]
            , "grid_kTau"      .= kTau
            , "grid_nMax"      .= nMaxGrid
            -- thin by 5 to keep the combined JSON manageable
            , "grid_logZ_flat" .=
                [ object ["tau2" .= t2, "logZ" .= lz]
                | (i, (t2, lz)) <- zip [0::Int ..] grid, i `mod` 5 == 0 ]
            ]

showRound :: Double -> Int -> String
showRound x n =
  let factor = 10 ^^ n :: Double
      r      = fromIntegral (round (x * factor) :: Integer) / factor
  in  show r

main :: IO ()
main = do
  results <- mapM runOne datasetList
  let obj = object [ "datasets" .= results
                   , "grid_kTau" .= kTau
                   , "grid_nMax" .= nMaxGrid ]
  BL.writeFile "test/gc_compare/gc_results.json" (A.encode obj)
  putStrLn "\nWrote: test/gc_compare/gc_results.json"
