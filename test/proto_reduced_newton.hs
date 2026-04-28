{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- Validation prototype: Schur-complement (reduced) Newton for binomial NMA
-- at fixed τ², compared against the existing full-network irlsSolve.
--
-- The full network has dim n_full = k + (T-1) + A (study + free-treat + tau-aux).
-- Each iteration of irlsSolve does dense linear algebra at this size.
--
-- The reduced Newton eliminates the τ-aux block analytically per iteration:
--   per arm:  k_eff = 1/(τ²_arm + 1/w_obs)
--   solve:    (k + T - 1) × (k + T - 1) system on (study, treatment) only
--   recover:  x_τ* = (x_S/τ²_arm + w·(x_T + ℓ_0)) / (1/τ²_arm + w)
--
-- Stationary points are identical (it's the same Lagrangian), so this
-- prototype must converge to the same treatment-effect estimates as
-- irlsSolve to numerical tolerance. That's what this file checks.
--
-- Run:  stack runghc --package meta-analysis -- test/proto_reduced_newton.hs
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.List       (foldl', nub)
import           Data.Maybe      (fromMaybe)
import           Text.Printf     (printf)

import Data.Meta.Effects       (TreatmentId (..), tidOfArm, Arm (..))
import Data.Meta.Studies       (readStudies, getStudyArms, getStudyId,
                                StudyId (..), Study)
import Data.Meta.NMA           (NetworkEffects (..), makeBinomialSprings)
import Data.Meta.RandomEffects (irlsSolve)

-- ---------------------------------------------------------------------
-- Flat representation: per arm we keep (study_idx, trt_idx, events, n).
-- Indexes are 0-based; trt_idx = 0 is the pinned reference treatment.
-- ---------------------------------------------------------------------

data Flat = Flat
  { flatK     :: !Int
  , flatT     :: !Int
  , flatArms  :: ![(Int, Int, Int, Int)]   -- (s, t, ev, n)
  , flatTids  :: ![TreatmentId]            -- index → TreatmentId
  } deriving Show

flatten :: [Study] -> Flat
flatten studies =
  let tidsAll = concatMap (\s -> map tidOfArm (getStudyArms s)) studies
      tids    = nub tidsAll                       -- order of first occurrence
      tidIdx  = Map.fromList (zip tids [0 :: Int ..])
      sidIdx  = Map.fromList (zip (map getStudyId studies) [0 :: Int ..])
      arms    = concatMap (\s ->
                  let si = sidIdx Map.! getStudyId s
                  in map (\a -> case a of
                           BinaryArm tid (toInt -> ev) (toInt -> n) ->
                             (si, tidIdx Map.! tid, ev, n)
                           _ -> error "expected BinaryArm")
                         (getStudyArms s))
                  studies
  in Flat (length studies) (length tids) arms tids
  where toInt = fromIntegral

-- ---------------------------------------------------------------------
-- Reduced-Newton state.
-- ---------------------------------------------------------------------

data State = State
  { stS :: ![Double]   -- length k    (study log-odds positions)
  , stT :: ![Double]   -- length T    (treatment effects, with stT !! 0 = 0)
  , stA :: ![Double]   -- length |arms|  (per-arm tau-aux logit)
  } deriving Show

-- Tame initialisation: x_τ = global mean logit (clipped),
-- x_S = 0, x_T = 0. Matches the FE start used by the existing irlsSolve.
initialState :: Flat -> State
initialState (Flat k t arms _) =
  let totalY = sum [fromIntegral ev | (_, _, ev, _) <- arms]
      totalN = sum [fromIntegral nn | (_, _, _, nn) <- arms]
      pBar   = max 1e-3 (min (1 - 1e-3) (totalY / totalN))
      l0     = log (pBar / (1 - pBar))
  in State { stS = replicate k     0.0
           , stT = replicate t     0.0
           , stA = replicate (length arms) l0
           }

-- ---------------------------------------------------------------------
-- One reduced-Newton iteration at fixed τ²_arm.
-- ---------------------------------------------------------------------

logistic :: Double -> Double
logistic x = 1 / (1 + exp (-x))

reducedNewtonStep :: Double -> Flat -> State -> (State, Double)
reducedNewtonStep tau2arm (Flat k t arms _) st =
  let nStudy = k                        -- all study nodes free
      nTreat = t - 1                    -- T-1 free treatments (index 0 pinned)
      nFree  = nStudy + nTreat
      idxS si = si                      -- studies span [0..k-1]
      idxT tj = if tj == 0 then Nothing else Just (k + tj - 1)

      -- Linearisation per arm. The binomial spring lives on the tau-treat
      -- edge with length ℓ = x_τ_ij - x_T_j. We need to use that, not just
      -- the raw x_τ position.
      perArm = [ let xTau    = stA st !! i
                     xT_     = stT st !! tj                     -- 0 when tj==0
                     -- Spring convention in NMA.hs: edge tau→treat has length
                     -- pos(treat) - pos(tau) = x_T - x_τ. p = σ(l) is then
                     -- the correct binomial probability for the arm.
                     l       = xT_ - xTau
                     p       = logistic l
                     n_      = fromIntegral nn
                     kMin    = max 1e-6 (n_ * 1e-10)
                     w       = max kMin (n_ * p * (1 - p))
                     resid   = fromIntegral ev - n_ * p
                     -- Clamp working-response increment to ±5, matching the
                     -- existing linearize() in NMA.hs to keep rare-event arms
                     -- from producing wild Newton steps.
                     clamp lo hi = max lo . min hi
                     yLin    = l + clamp (-5) 5 (resid / w)
                     kEff    = 1.0 / (tau2arm + 1.0 / w)
                 in (i, si, tj, w, yLin, kEff)
               | (i, (si, tj, ev, nn)) <- zip [0..] arms ]

      -- Build sparse entries for A and b on the reduced (study, free-treat) basis.
      addArm (am, bm) (_, si, tj, _, yLin, kE) =
        let s = idxS si
            mt = idxT tj
        in case mt of
             Just t' ->
               -- Effective spring between study s and treatment t' with
               -- natural length yLin. Energy (1/2) k_eff (x_T - x_S - yLin)^2.
               -- Stationary equations:
               --   x_S row: -k_eff·(x_T - x_S - yLin) = 0  → +k_eff x_S - k_eff x_T = -k_eff yLin
               --   x_T row: +k_eff·(x_T - x_S - yLin) = 0  → -k_eff x_S + k_eff x_T = +k_eff yLin
               ( [((s,s), kE), ((t',t'), kE),
                  ((s,t'), -kE), ((t',s), -kE)] ++ am
               , [(s, -kE * yLin), (t', kE * yLin)] ++ bm )
             Nothing ->
               -- treatment 0 pinned at 0: only the study row has a contribution
               -- x_S row:  +k_eff x_S - k_eff·0 = -k_eff yLin → x_S contribution.
               ( [((s,s), kE)] ++ am
               , [(s, -kE * yLin)] ++ bm )

      (entriesRaw, bEntriesRaw) = foldl' addArm ([], []) perArm
      aMap = Map.fromListWith (+) entriesRaw
      bMap = Map.fromListWith (+) bEntriesRaw

      aMat = [ [ Map.findWithDefault 0 (i, j) aMap | j <- [0 .. nFree - 1] ]
             | i <- [0 .. nFree - 1] ]
      bVec = [ Map.findWithDefault 0 i bMap | i <- [0 .. nFree - 1] ]

      sol = solveDense aMat bVec
      newS = take nStudy sol
      newT = 0 : drop nStudy sol      -- prepend pinned treatment 0

      -- Recover x_τ for each arm from the closed-form equilibrium.
      newA = [ let (_, _, _, w, yLin, _) = perArm !! i
                   xS_ = newS !! si
                   xT_ = newT !! tj
                   -- Recovery: x_τ = (x_S/τ²_arm + w(x_T - yLin)) / (1/τ²_arm + w).
                   -- Sign on yLin reflects spring length = x_T - x_τ, so the
                   -- linearised obs equilibrium has x_T - x_τ = yLin → x_τ = x_T - yLin.
                   tauPos = (xS_ / tau2arm + w * (xT_ - yLin))
                            / (1 / tau2arm + w)
               in tauPos
             | (i, (si, tj, _, _)) <- zip [0..] arms ]

      -- Convergence on TRUE Newton step: max change in spring length
      -- (= x_τ - x_T) across all arms. The linear-system residual is always
      -- ~0 after Gaussian elimination, so it's not a meaningful criterion.
      stepDelta =
        maximum $ 0 :
          [ let oldL = (stT st !! tj) - (stA st !! i)
                newL = (newT !! tj)   - (newA !! i)
            in abs (newL - oldL)
          | (i, (_, tj, _, _)) <- zip [0..] arms ]

  in (State newS newT newA, stepDelta)

-- Bare-bones Gaussian elimination, just for the prototype.
solveDense :: [[Double]] -> [Double] -> [Double]
solveDense a b =
  let n = length b
      aug = zipWith (\r y -> r ++ [y]) a b
      forward m = foldl elim m [0 .. n - 1]
      elim m k0 =
        let pivotRow = m !! k0
            piv      = pivotRow !! k0
        in [ if i == k0 then m !! i
             else
               let mr = m !! i
                   f  = (mr !! k0) / piv
               in zipWith (-) mr (map (* f) pivotRow)
           | i <- [0 .. n - 1] ]
      backSub m =
        let go i acc =
              let row = m !! i
                  rhs = (row !! n) - sum [ (row !! j) * (acc !! (j - i - 1))
                                         | j <- [i + 1 .. n - 1] ]
                  xi  = rhs / (row !! i)
              in xi : acc
        in foldr go [] [0 .. n - 1]
  in backSub (forward aug)

-- ---------------------------------------------------------------------
-- Driver: load synth_041, run reduced Newton, compare with irlsSolve.
-- ---------------------------------------------------------------------

iterateUntil :: Int -> Double -> Double -> Flat -> State -> (State, Int, Double)
iterateUntil maxIters eps tau2arm flat = go 0
  where
    go iter st
      | iter >= maxIters = (st, iter, 1/0)
      | otherwise =
          let (st', g) = reducedNewtonStep tau2arm flat st
          in if g < eps then (st', iter + 1, g)
             else go (iter + 1) st'

main :: IO ()
main = do
  let path        = "test/synth_bin/synth_041.json"
      tau2contrast = 0.5            -- match springGrandCanonicalBin convention
      tau2arm      = tau2contrast / 2.0
  est <- readStudies path
  case est of
    Left err      -> putStrLn $ "read err: " ++ err
    Right studies -> do
      let flat@(Flat k t arms tids) = flatten studies
          nFull = k + (t - 1) + length arms
          nRed  = k + (t - 1)
      printf "Loaded %s\n" path
      printf "  k=%d studies, T=%d treatments, A=%d arms\n"
              k t (length arms)
      printf "  full Newton dim     = %d\n" nFull
      printf "  reduced Newton dim  = %d\n" nRed
      printf "  per-iter cost ratio = %.1f×\n"
              (fromIntegral nFull ^^ (3::Int) /
               fromIntegral nRed  ^^ (3::Int) :: Double)
      printf "  τ²_contrast = %.3f, τ²_arm = %.3f\n" tau2contrast tau2arm

      let st0 = initialState flat
          (stFinal, iters, gFinal) =
              iterateUntil 200 1e-9 tau2arm flat st0

      printf "\nReduced Newton converged in %d iters (max|grad| = %.3e)\n"
              iters gFinal

      -- Compare against the existing full-network irlsSolve at the same τ².
      case makeBinomialSprings studies Nothing (Just tau2contrast) of
        Left err -> putStrLn $ "spring build err: " ++ err
        Right springnet0 -> do
          let neteffs = irlsSolve springnet0 200 1e-9
              estFull = networkEstimates neteffs

          putStrLn ""
          putStrLn "=== Effect-by-effect comparison (irlsSolve vs reduced) ==="
          printf "%-15s %12s %12s %12s\n"
            ("a -> b" :: String) ("irlsSolve" :: String)
            ("reduced" :: String) ("|Δ|" :: String)
          printf "%s\n" (replicate 55 '-')

          let ts = stT stFinal
              showT (TreatmentId tid) = show tid

              cmpOne (a, bMap) =
                concatMap (\(b, eFull) ->
                  let aIdx = fromMaybe (-1) (lookup a (zip tids [0..]))
                      bIdx = fromMaybe (-1) (lookup b (zip tids [0..]))
                      eRed = (ts !! bIdx) - (ts !! aIdx)
                      d    = abs (eFull - eRed)
                  in if aIdx < 0 || bIdx < 0 then []
                     else [(showT a ++ " -> " ++ showT b, eFull, eRed, d)]
                ) (Map.toList bMap)

              rows = concatMap cmpOne (Map.toList estFull)
              maxAbsDelta = if null rows then 0 else
                            maximum [d | (_, _, _, d) <- rows]
          mapM_ (\(lbl, ef, er, d) ->
                  printf "%-15s %12.6f %12.6f %12.3e\n" lbl ef er d) rows
          printf "%s\n" (replicate 55 '-')
          printf "max |Δ| over all effects: %.3e\n" maxAbsDelta
          if maxAbsDelta < 1e-5
            then putStrLn "PASS: reduced Newton matches full irlsSolve."
            else putStrLn "FAIL: discrepancy too large."
