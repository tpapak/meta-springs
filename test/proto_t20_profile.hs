{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- Time breakdown for the GC adaptive pipeline on /tmp/t20.json.
-- Runs the same logic as springGrandCanonicalBinAdaptive but with explicit
-- phase timing so we can see where the 1.57 s actually goes.
--
-- Run: stack runghc --package meta-analysis -- test/proto_t20_profile.hs
module Main where

import qualified Data.Map.Strict as Map
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
import           Text.Printf     (printf)

import Data.Meta.Studies       (readStudies)
import Data.Meta.NMA           (NetworkEffects (..), makeBinomialSprings,
                                springs, springEnergy)
import Data.Meta.RandomEffects (evalXiBinAt, springREMLBin)

forceNE :: NetworkEffects -> Double
forceNE ne =
  let est = networkEstimates ne
      var = networkVariances ne
      sumNested m = sum [v | row <- Map.elems m, v <- Map.elems row]
      springsSum = sum $ map springEnergy $ Map.elems $ springs $ springNetwork ne
  in sumNested est + sumNested var + springsSum

phase :: String -> IO a -> IO a
phase nm io = do
  t0 <- getCurrentTime
  !x <- io
  t1 <- getCurrentTime
  let dt = realToFrac (diffUTCTime t1 t0) :: Double
  printf "  %-30s %8.3f s\n" nm dt
  return x

main :: IO ()
main = do
  putStrLn "=== T=20 profile (k=30, A=60, /tmp/t20.json) ==="
  est <- phase "readStudies" $ readStudies "/tmp/t20.json"
  case est of
    Left err -> putStrLn $ "read err: " ++ err
    Right studies -> do
      _ <- phase "makeBinomialSprings (template)" $
        case makeBinomialSprings studies Nothing (Just 1.0) of
          Left e -> error e
          Right n -> return n
      mTau <- phase "springREMLBin (full-Newton inside)" $
        case springREMLBin studies Nothing (Just 500) (Just 1e-6) of
          Left e -> error e
          Right (_, (t, _)) -> return t
      let nPts = 100
          tHat = max mTau 1e-5
          lo   = max 1e-6 (tHat / 30)
          hi   = max (tHat * 30) 5.0
          tau2Vs = 0.0 :
            [lo * ((hi/lo) ** (fromIntegral i / fromIntegral (nPts-1)))
            | i <- [0 .. nPts - 1]]
      printf "  REML τ² = %.4f → grid [%.4f, %.4f] (%d pts)\n"
        mTau lo hi (length tau2Vs)
      _ <- phase "evalXiBinAt 101 pts (uses reduced Newton)" $
        case evalXiBinAt studies tau2Vs of
          Left e -> error e
          Right (_, gridRes) -> do
            let !s = sum [forceNE ne | (_, _, ne) <- gridRes]
            return s
      return ()
