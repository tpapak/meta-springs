{-|
Module      : Meta.Pairwise.CommonEffect
Description : Common or Fixed effect meta-analysis
Copyright   : (c) Thodoris Papakonstantinou, 2018
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Chapter 11 Borenstein's Introduction to Meta-Analysis
Common Effect analysis asumes all studies share a single true effect.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Meta.Pairwise.CommonEffect
  ( CommonEffect(..)
  )
where

import           Control.Applicative
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.IntMap.Strict            as IM
import           Data.Maybe
import           GHC.Generics
import           Data.Either
import           Data.List

import           Data.Numerics
import           Data.Meta.Effects
import           Data.Meta.Studies

-- | Common Effect
class Effect a => CommonEffect a where
  commonEffect :: [a] -> a

instance CommonEffect MD where
  commonEffect effects =
    let
      ws  = fmap (\e -> 1 / variance e) effects
      nrm = sum ws
      -- (11.3)
      pe =
        foldl' (\ac (e, w) -> (ac + ((point e) * w) / nrm)) 0 $ zip effects ws
      -- (11.4)
      var = 1 / nrm
    in
      (MD pe var)

instance CommonEffect SMD where
  commonEffect effects =
    let
      ws  = fmap (\e -> 1 / (variance e)) effects
      nrm = sum ws
      -- (11.3)
      pe =
        foldl' (\ac (e, w) -> (ac + ((point e) * w) / nrm)) 0 $ zip effects ws
      -- (11.4)
      var = 1 / nrm
    in
      (SMD pe var)

instance CommonEffect LogOR where
  commonEffect effects =
    let
      ws  = fmap (\e -> 1 / (variance e)) effects
      nrm = sum ws
      -- (11.3)
      pe =
        foldl' (\ac (e, w) -> (ac + ((point e) * w) / nrm)) 0 $ zip effects ws
      -- (11.4)
      var = 1 / nrm
    in
      (LogOR pe var)

instance CommonEffect LogRR where
  commonEffect effects =
    let
      ws  = fmap (\e -> 1 / (variance e)) effects
      nrm = sum ws
      -- (11.3)
      pe =
        foldl' (\ac (e, w) -> (ac + ((point e) * w) / nrm)) 0 $ zip effects ws
      -- (11.4)
      var = 1 / nrm
    in
      (LogRR pe var)

instance CommonEffect RD where
  commonEffect effects =
    let
      ws  = fmap (\e -> 1 / (variance e)) effects
      nrm = sum ws
      -- (11.3)
      pe =
        foldl' (\ac (e, w) -> (ac + ((point e) * w) / nrm)) 0 $ zip effects ws
      -- (11.4)
      var = 1 / nrm
    in
      (RD pe var)

instance CommonEffect OR where
  commonEffect effects =
    let lefs = fmap orToLogOR effects
        lce  = commonEffect lefs
    in  logORToOR lce

instance CommonEffect RR where
  commonEffect effects =
    let lefs = fmap rrToLogRR effects
        lce  = commonEffect lefs
    in  logRRToRR lce
