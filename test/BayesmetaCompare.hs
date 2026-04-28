-- Ad-hoc runner: report springGrandCanonicalBin posteriors on test/binary.json
-- under two priors:
--   (a) flat-in-tau2                  -> comparable with bayesmeta
--   (b) half-Normal(0, 1) on tau      -> comparable with multinma
module Main where

import Data.Meta.Studies     (readStudies)
import Data.Meta.RandomEffects (springGrandCanonicalBin, springREMLBin)

-- Summarise a log-posterior grid (list of (tau2, logPost)) -> mode, median,
-- mean, 95% CI. Mode is picked by explicit argmax over indices (no tuple
-- tie-breaking surprises near a flat peak).
summarise :: [(Double, Double)] -> (Double, Double, Double, (Double, Double))
summarise grid =
  let logPs   = map snd grid
      maxL    = maximum logPs
      unnorm  = map (\lp -> exp (lp - maxL)) logPs
      total   = sum unnorm
      probs   = map (/ total) unnorm
      tau2Vs  = map fst grid
      mean_   = sum (zipWith (*) tau2Vs probs)
      cum     = scanl1 (+) probs
      modeIx  = snd $ foldr1 (\a b -> if fst a >= fst b then a else b)
                             (zip logPs [0 ..])
      medIx   = length (takeWhile (< 0.5)   cum)
      loIx    = length (takeWhile (< 0.025) cum)
      hiIx    = length (takeWhile (< 0.975) cum)
  in (tau2Vs !! modeIx, tau2Vs !! medIx, mean_, (tau2Vs !! loIx, tau2Vs !! hiIx))

printSummary :: String -> (Double, Double, Double, (Double, Double)) -> IO ()
printSummary label (m, med, mn, (lo, hi)) = do
  putStrLn label
  putStrLn $ "  tau2 mode    : " ++ show m
  putStrLn $ "  tau2 median  : " ++ show med
  putStrLn $ "  tau2 mean    : " ++ show mn
  putStrLn $ "  95% CI       : (" ++ show lo ++ ", " ++ show hi ++ ")"

main :: IO ()
main = do
  est <- readStudies "test/binary.json"
  case est of
    Left err      -> putStrLn $ "ERR loading studies: " ++ err
    Right studies -> do
      putStrLn $ "n studies: " ++ show (length studies)
      case springREMLBin studies Nothing (Just 1000) (Just 1e-6) of
        Left err           -> putStrLn $ "REML err: " ++ err
        Right (_, (rt, _)) -> putStrLn $ "spring REML tau2 : " ++ show rt

      -- kTau=500, nMax=10000 -> tau2 grid 0..20.0, step 0.002
      case springGrandCanonicalBin studies 500 10000 of
        Left err                  -> putStrLn $ "GC err: " ++ err
        Right (_, grid, _) -> do
          -- (a) Flat-in-tau2 prior: log posterior = log Z
          printSummary "\n[spring GC, prior: flat-in-tau2]" (summarise grid)
          -- (b) Half-Normal(0, 1) on tau.
          -- Density on tau2: pi_{tau2}(tau2) = (1/(2 tau)) sqrt(2/pi) exp(-tau2/2)
          --   -> log pi += -(1/2) log(tau2) - tau2 / 2   (const dropped)
          -- Density on tau :  pi_{tau} (tau)  = sqrt(2/pi) exp(-tau2/2)
          --   -> log pi_tau += -tau2 / 2 ; transform uses dtau2/dtau = 2 tau
          --     so log p_tau(tau) = log Z(tau2) - tau2/2 + log(tau) + const
          let gridHN_t2 = [ (t2, lz - 0.5 * log t2 - 0.5 * t2)
                          | (t2, lz) <- grid, t2 > 0 ]
              (_, medT2, meanT2, (loT2, hiT2)) = summarise gridHN_t2
          putStrLn "\n[spring GC, prior: half-Normal(0,1) on tau]"
          putStrLn $ "  tau2 median  : " ++ show medT2
          putStrLn $ "  tau2 mean    : " ++ show meanT2
          putStrLn $ "  95% CI tau2  : (" ++ show loT2 ++ ", " ++ show hiT2 ++ ")"
          -- Mode in tau-space (density p_tau(tau) = Z(t2) * tau * exp(-t2/2)
          -- up to constants). Argmax is invariant to grid spacing.
          let tauLogP = [ (sqrt t2, lz + 0.5 * log t2 - 0.5 * t2)
                        | (t2, lz) <- grid, t2 > 0 ]
              modeTau = fst $ foldr1 (\a b -> if snd a >= snd b then a else b)
                                     tauLogP
          putStrLn $ "  tau  mode    : " ++ show modeTau
                     ++ "  (i.e. tau2 = " ++ show (modeTau * modeTau) ++ ")"
          -- Via the CDF invariance, median/CI of tau = sqrt of tau2 values.
          putStrLn $ "  tau  median  : " ++ show (sqrt medT2)
          putStrLn $ "  95% CI tau   : (" ++ show (sqrt loT2)
                     ++ ", " ++ show (sqrt hiT2) ++ ")"
