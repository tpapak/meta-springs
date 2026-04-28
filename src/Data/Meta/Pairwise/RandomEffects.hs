{-|
module      : Meta.Pairwise.RandomEffects
Description : Random effects meta-analysis
Copyright   : (c) Thodoris Papakonstantinou, 2018
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Chapter 12 Borenstein's Introduction to Meta-Analysis
Random effects study synthesis asumes every study has its own true effect \(θ_i\)
normally distributed around the summary effect.
-}

module Data.Meta.Pairwise.RandomEffects
  ( RandomEffectsClass (..)
  , RandomEffects (..)
  , TauSquare (..)
  --, ISquare (..)
  , randomEffectsOR
  , randomEffectsRR
  ) where

import           Control.Applicative
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           GHC.Generics
import Data.Either
import Data.List

import Data.Numerics
import Data.Meta.Effects

data TauSquare = TauSquare PointEstimate Variance ConfidenceInterval
  deriving (Ord,Eq,Show)
instance Estimate TauSquare where
  point (TauSquare p v ci) = p
  ci = ci
  mapEstimate f (TauSquare p v c) = TauSquare (f p) (f v) c
 
--data ISquare = ISquare PointEstimate Variance
  --deriving (Read,Ord,Eq,Show)
--instance Gaussian ISquare where
  --expectation (ISquare p v) = p
  --variance (ISquare p v) = v
--instance Estimate ISquare where
  --point (ISquare p v) = p
  --ci = normalCI
  --mapEstimate f (ISquare p v) = ISquare (f p) (f v)

-- | Random Effects class
class (Gaussian a, Estimate a) => RandomEffectsClass a where
  randomEffects :: [a] -> RandomEffects a

data RandomEffects a = 
  RandomEffects { summary :: a 
                , qq :: Double
                , df :: Int -- degrees of freedom
                , τsq :: TauSquare
                }

randomeffects' :: Gaussian a => [a] -> (PointEstimate, Variance, Double, Int, TauSquare)
randomeffects' effects = 
  let ws = fmap (\e -> 1 / (variance e)) effects
      nrm = sum ws
      -- (12.3)
      qq = (foldl' (\ac (w,y) -> ac + (w * (expectation y)^2)) 0 $ zip ws effects)
           - (foldl' (\ac (w,y) -> ac + w * (expectation y) ) 0 $ zip ws effects)^2 / nrm
      -- (12.4)
      df = (length effects) - 1
      df' = fromIntegral df
      -- (12.5)
      cc = nrm - (sum $ fmap (\w -> w^2) ws) / nrm
      -- (12.2)
      out = (qq - (fromIntegral df))/ cc
      τsq = if out > 0
               then out
               else 0
      -- (16.12)
      sw1 = sum $ fmap (\e -> 1 / (variance e) ) effects
      sw2 = sum $ fmap (\e -> 1 / (variance e)^2 ) effects
      sw3 = sum $ fmap (\e -> 1 / (variance e)^3 ) effects
      -- (16.11)
      aa = df' + 2 * (sw1 - (sw2/sw1)) * τsq + (sw2 - 2 * (sw3/sw1) + (sw2)^2/(sw1)^2 ) * τsq^2
      τvar = if out < 0
                 then 0
                 else 2 * aa / cc^2
      -- (16.15)
      bb = if qq > (df' + 1) 
              then
                0.5 * ((log qq) - log (df')) / ((sqrt (2 * qq)) - sqrt ((2 * df') - 1))
              else
                sqrt (1/(2*(df'-1)*(1-(1/(3*(df'-1)^2))))) -- (16.15)
      ll = (exp (0.5 * log (qq/df') - invcumul975 * bb)) -- (16.16)
      uu = (exp (0.5 * log (qq/df') + invcumul975 * bb)) -- (16.17)
      τcil = (df' * (ll^2 -1)/cc) -- (16.18)
      τciu = (df' * (uu^2 -1)/cc) -- (16.19)
      τci = CI (if τcil > 0 then τcil else 0) 
               (if τciu > 0 then τciu else 0)
      -- (12.6)
      ws' = fmap (\e -> 1 / (variance e + τsq)) effects
      nrm' = sum ws'
      -- (12.7)
      pe' = foldl' (\ac (e, w') -> (ac + ((expectation e) * w')/nrm')) 0 $ zip effects ws'
      -- (12.8)
      var' = 1 / nrm'
   in (pe', var', qq, df, TauSquare τsq τvar τci)

instance RandomEffectsClass MD where
  randomEffects effects = 
    RandomEffects { summary = MD pe' var'
                  , qq = qq
                  , df = df
                  , τsq = tsq
                  }
    where (pe', var', qq, df, tsq) = randomeffects' effects

instance RandomEffectsClass SMD where
  randomEffects effects = 
    RandomEffects { summary = SMD pe' var'
                  , qq = qq
                  , df = df
                  , τsq = tsq
                  }
    where (pe', var', qq, df, tsq) = randomeffects' effects

instance RandomEffectsClass LogOR where
  randomEffects effects = 
    RandomEffects { summary = LogOR pe' var'
                  , qq = qq
                  , df = df
                  , τsq = tsq
                  }
    where (pe', var', qq, df, tsq) = randomeffects' effects

instance RandomEffectsClass LogRR where
  randomEffects effects = 
    RandomEffects { summary = LogRR pe' var'
                  , qq = qq
                  , df = df
                  , τsq = tsq
                  }
    where (pe', var', qq, df, tsq) = randomeffects' effects

instance RandomEffectsClass RD where
  randomEffects effects = 
    RandomEffects { summary = RD pe' var'
                  , qq = qq
                  , df = df
                  , τsq = tsq
                  }
    where (pe', var', qq, df, tsq) = randomeffects' effects

randomEffectsRR :: [RR] -> RR
randomEffectsRR effects =
    let lefs = fmap rrToLogRR effects
        lce = summary $ randomEffects lefs
     in logRRToRR lce

randomEffectsOR :: [OR] -> OR
randomEffectsOR effects =
    let lefs = fmap orToLogOR effects
        lce = summary $ randomEffects lefs
     in logORToOR lce
