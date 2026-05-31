{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Meta.GSLEstimators
-- Description : GSL-based τ² estimators for comparison with spring-REML
-- Copyright   : (c) Thodoris Papakonstantinou, 2022
-- License     : GPL-3
-- Maintainer  : dev@tpapak.com
--
-- These estimators use GSL for numerical optimization/root-finding.
-- They serve as reference implementations to compare against the pure
-- Haskell spring-REML estimator (vbτ2).
module Test.Meta.GSLEstimators
  ( rEML
  , rootDOFQτDiff
  , minDOFQτDiff
  , rootQtau'
  ) where

import Data.Maybe
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.GSL.Root as GSLR
import qualified Numeric.GSL.Minimization as GSLM

import Data.Meta.Effects
import Data.Meta.Studies
import Data.Meta.NMA
import Data.Meta.RandomEffects

-- | find tau which maximizes the loglikelihood. This produces equivalent with
-- the REML estimator
rEML ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] -> -- list of studies
  (Arm -> Either String a) -> -- study effect calculator
  Maybe TauSquare -> -- first guess
  Maybe Int -> -- iterations
  Maybe Double -> -- epsilon
  Either String (NetworkEffects, (Double, LA.Matrix Double))
rEML studies armEffect mtau0 miters mε =
  let guess0 = fromMaybe 0.5 mtau0
      iters = fromMaybe 1000 miters
      ε = fromMaybe 10E-4 mε
      espringnet = makeSprings studies Nothing (Just guess0) armEffect
   in case espringnet of
        Left err -> Left err
        Right springsnet0 ->
          let neteffects0 = nma (updateSprings springsnet0 guess0)
              newNetEffects tau = nma (updateSprings springsnet0 tau)
              f taus =
                let tau = head taus
                  in (\nef -> let spnet = springNetwork nef
                               in (logLikelihood spnet)) (newNetEffects tau)
              (tau:rest, path) = GSLM.minimize GSLM.NMSimplex ε iters [1] (negate . f) [guess0]
         in (Right $ (nma (updateSprings springsnet0 tau), (tau, path)))

-- | Get τ2 for which Qτ = DOF by root finding
rootDOFQτDiff ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] -> -- list of studies
  (Arm -> Either String a) -> -- study effect calculator
  Maybe TauSquare -> -- first guess
  Maybe Int -> -- iterations
  Maybe Double -> -- epsilon
  Either String (NetworkEffects, (Double, LA.Matrix Double))
rootDOFQτDiff studies armEffect mtau0 miters mε =
  let guess0 = fromMaybe 0.5 mtau0
      iters = fromMaybe 1000 miters
      ε = fromMaybe 10E-4 mε
      espringnet = makeSprings studies Nothing (Just guess0) armEffect
   in case espringnet of
        Left err -> Left err
        Right springsnet0 ->
          let neteffects0 = nma (updateSprings springsnet0 guess0)
              newNetEffects tau = nma (updateSprings springsnet0 tau)
              f taus =
                let tau = head taus
                  in (\nef -> let spnet = springNetwork nef
                               in [tauCostFunction spnet]) (newNetEffects tau)
              (tau:rest, path) = GSLR.root GSLR.Hybrid ε iters f [guess0]
         in (Right $ (nma (updateSprings springsnet0 tau), (tau, path)))

-- | Get τ2 for which |Qτ - DOF| is minimized
minDOFQτDiff ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] -> -- list of studies
  (Arm -> Either String a) -> -- study effect calculator
  Maybe TauSquare -> -- first guess
  Maybe Int -> -- iterations
  Maybe Double -> -- epsilon
  Either String (NetworkEffects, (Double, LA.Matrix Double))
minDOFQτDiff studies armEffect mtau0 miters mε =
  let guess0 = fromMaybe 0.5 mtau0
      iters = fromMaybe 1000 miters
      ε = fromMaybe 10E-5 mε
      espringnet = makeSprings studies Nothing (Just guess0) armEffect
   in case espringnet of
        Left err -> Left err
        Right springsnet0 ->
          let !neteffects0 = nma (updateSprings springsnet0 guess0)
              newNetEffects tau = nma (updateSprings springsnet0 tau)
              f taus =
                let tau = head taus
                  in (\nef -> let spnet = springNetwork nef
                               in (tauCostFunction spnet)) (newNetEffects tau)
              !(tau:rest, path) = GSLM.minimize GSLM.NMSimplex ε iters [1] (abs . f) [guess0]
         in (Right $ (nma (updateSprings springsnet0 tau), (tau, path)))

-- | Get τ2 for which Qτ' = 0
rootQtau' ::
  (ArmEffect a b, Gaussian a, Gaussian b) =>
  [Study] -> -- list of studies
  (Arm -> Either String a) -> -- study effect calculator
  Maybe TauSquare -> -- first guess
  Maybe Int -> -- iterations
  Maybe Double -> -- epsilon
  Either String (NetworkEffects, (Double, LA.Matrix Double))
rootQtau' studies armEffect mtau0 miters mε =
  let guess0 = fromMaybe 0.5 mtau0
      iters = fromMaybe 1000 miters
      ε = fromMaybe 10E-4 mε
      espringnet = makeSprings studies Nothing (Just guess0) armEffect
   in case espringnet of
        Left err -> Left err
        Right springsnet0 ->
          let neteffects0 = nma (updateSprings springsnet0 guess0)
              newNetEffects tau = nma (updateSprings springsnet0 tau)
              f taus =
                let tau = head taus
                  in (\nef -> let spnet = springNetwork nef
                               in [qqTau' spnet]) (newNetEffects tau)
              (tau:rest, path) = GSLR.root GSLR.Hybrid ε iters f [guess0]
         in (Right $ (nma (updateSprings springsnet0 tau), (tau, path)))
