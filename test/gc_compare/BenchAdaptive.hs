{-# LANGUAGE OverloadedStrings #-}
-- Benchmark: adaptive-grid GC vs uniform-grid GC vs REML (mode only).
-- On pairwise (test/binary.json) and on a synthetic NMA.
module Main where

import qualified Data.Aeson            as A
import           Data.Aeson            ((.=), object)
import qualified Data.ByteString.Lazy  as BL
import           Data.Time.Clock       (getCurrentTime, diffUTCTime)
import           System.IO             (hFlush, stdout)

import Data.Meta.Studies       (readStudies, Study)
import Data.Meta.RandomEffects
  ( springGrandCanonicalBin         -- uniform
  , springGrandCanonicalBinFull     -- uniform, full
  , springGrandCanonicalBinAdaptive -- NEW
  , springREMLBin                   -- EM-style mode
  )

-- Summarise log-Z grid under half-Normal(0,1) prior on τ.
summary :: [(Double, Double)] -> (Double, Double, Double, Double, Double)
summary grid0 =
  let grid   = [ (t2, lz - 0.5 * log t2 - 0.5 * t2)
               | (t2, lz) <- grid0, t2 > 0 ]
      t2s    = map fst grid
      lps    = map snd grid
      maxL   = maximum lps
      unnorm = map (\lp -> exp (lp - maxL)) lps
      weights = zipWith (\t2a t2b -> t2b - t2a)
                   (0 : init t2s) t2s
      total  = sum (zipWith (*) unnorm weights)
      dens   = zipWith (\u w -> u * w / total) unnorm weights
      pdfW   = zipWith (/) dens weights  -- density per τ²
      meanV  = sum (zipWith (*) t2s dens)
      cum    = scanl1 (+) dens
      medIx  = length (takeWhile (< 0.5)   cum)
      loIx   = length (takeWhile (< 0.025) cum)
      hiIx   = length (takeWhile (< 0.975) cum)
      modeIx = snd $ foldr1 (\a b -> if fst a >= fst b then a else b)
                           (zip pdfW [0 :: Int ..])
  in ( t2s !! modeIx, t2s !! medIx, meanV
     , t2s !! loIx,   t2s !! hiIx )

-- Force an Either of "something with a Double inside" by consuming all
-- doubles via a sum. Sufficient to trigger the full Ξ computation.
forceEither :: Either String (Double, [(Double, Double)], a) -> ()
forceEither (Left _)                     = ()
forceEither (Right (m, grid, _)) =
  let s = sum (map snd grid)
  in  m `seq` s `seq` ()

forceEitherFull :: Either String (Double, [(Double, Double, a)]) -> ()
forceEitherFull (Left _)                 = ()
forceEitherFull (Right (m, grid)) =
  let s = sum [ lz | (_, lz, _) <- grid ]
  in  m `seq` s `seq` ()

forceReml :: Either String (a, (Double, [Double])) -> ()
forceReml (Left _) = ()
forceReml (Right (_, (t, xs))) = t `seq` sum xs `seq` ()

timed :: String -> (a -> ()) -> a -> IO Double
timed label force_ x = do
  t0 <- getCurrentTime
  force_ x `seq` return ()
  t1 <- getCurrentTime
  let dt = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "  " ++ label ++ ": " ++ show dt ++ " s"
  hFlush stdout
  return dt

runOnDataset :: String -> FilePath -> IO A.Value
runOnDataset dsName path = do
  putStrLn $ "\n=== " ++ dsName ++ " (" ++ path ++ ") ==="
  est <- readStudies path
  case est of
    Left err      -> do putStrLn ("read err: " ++ err); return (object [])
    Right studies -> do
      putStrLn $ "  " ++ show (length studies) ++ " studies"

      let remlRes = springREMLBin studies Nothing (Just 500) (Just 1e-6)
          uniRes  = springGrandCanonicalBin studies 500 5000
          adpRes  = springGrandCanonicalBinAdaptive studies 100
      tREML <- timed "REML   " forceReml        remlRes
      tUni  <- timed "GC-uni " forceEither      uniRes
      tAdp  <- timed "GC-adp " forceEitherFull  adpRes
      let remlTau2 = case remlRes of
            Right (_, (t, _)) -> Just t
            Left _            -> Nothing
      let (uniMode, uniGrid) = case uniRes of
            Right (m, g, _) -> (Just m, g)
            Left _          -> (Nothing, [])
          uniSum = if null uniGrid then (0,0,0,0,0) else summary uniGrid
      let (adpMode, adpGrid) = case adpRes of
            Right (m, g) -> (Just m, map (\(t2, lz, _) -> (t2, lz)) g)
            Left _       -> (Nothing, [])
          adpSum = if null adpGrid then (0,0,0,0,0) else summary adpGrid

      -- Report
      putStrLn $ "  REML τ²     = " ++ show remlTau2
      putStrLn $ "  GC-uni mode = " ++ show uniMode
      putStrLn $ "  GC-adp mode = " ++ show adpMode
      let (m_u, med_u, mean_u, lo_u, hi_u) = uniSum
          (m_a, med_a, mean_a, lo_a, hi_a) = adpSum
      putStrLn $ "  GC-uni (HN): mode=" ++ show m_u
               ++ " median=" ++ show med_u
               ++ " mean="   ++ show mean_u
               ++ " 95% CI=(" ++ show lo_u ++ ", " ++ show hi_u ++ ")"
      putStrLn $ "  GC-adp (HN): mode=" ++ show m_a
               ++ " median=" ++ show med_a
               ++ " mean="   ++ show mean_a
               ++ " 95% CI=(" ++ show lo_a ++ ", " ++ show hi_a ++ ")"
      putStrLn $ "  speedup adp/uni: " ++ show (tUni / tAdp) ++ "x"

      return $ object
        [ "dataset"      .= dsName
        , "n_studies"    .= length studies
        , "reml_tau2"    .= remlTau2
        , "reml_sec"     .= tREML
        , "uniform" .= object
            [ "sec"     .= tUni
            , "mode"    .= m_u
            , "median"  .= med_u
            , "mean"    .= mean_u
            , "ci_lo"   .= lo_u
            , "ci_hi"   .= hi_u
            ]
        , "adaptive" .= object
            [ "sec"     .= tAdp
            , "mode"    .= m_a
            , "median"  .= med_a
            , "mean"    .= mean_a
            , "ci_lo"   .= lo_a
            , "ci_hi"   .= hi_a
            ]
        ]

main :: IO ()
main = do
  r1 <- runOnDataset "pairwise_binary"   "test/binary.json"
  r2 <- runOnDataset "synth_025 (T=6, k=11)" "test/synth_bin/synth_025.json"
  r3 <- runOnDataset "synth_041 (T=6, k=20)" "test/synth_bin/synth_041.json"
  r4 <- runOnDataset "nmadb_479650 (T=6, k=27)" "test/nmadb/nmadb_479650.json"
  let out = object ["benchmarks" .= [r1, r2, r3, r4]]
  BL.writeFile "test/gc_compare/bench_adaptive.json" (A.encode out)
  putStrLn "\nWrote: test/gc_compare/bench_adaptive.json"
