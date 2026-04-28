{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Meta.Hatmatrix
  ( HatMatrix
  , HatMatrixRaw
  , HatMatrixRow
  , hatMatrixFromList
  , rowNames
  ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.List.Split
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import qualified Data.Text           as T
import qualified Data.Text.Read      as TR
import           GHC.Generics
import qualified Data.Meta.Effects as MetaEff

data HElement = HElement { row        :: MetaEff.ComparisonId
                         , comparison :: MetaEff.ComparisonId
                         , value      :: Double
                         }
  deriving (Show,Generic,Eq)
instance FromJSON HElement
instance ToJSON HElement

type HatMatrixRaw = [HElement]
type HatMatrixRow = Map.Map MetaEff.ComparisonId Double
type HatMatrix = Map.Map MetaEff.ComparisonId HatMatrixRow

hatMatrixFromList :: HatMatrixRaw -> HatMatrix
hatMatrixFromList hmr =
  let hml =
        map (\he ->
          (row he, Map.singleton (comparison he) (value he))) hmr
   in Map.fromListWith Map.union hml

rowNames :: HatMatrix -> [String]
rowNames hm =
  map show $ Map.keys hm
