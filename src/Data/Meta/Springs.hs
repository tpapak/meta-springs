{-# LANGUAGE DeriveGeneric #-}

module Data.Meta.Springs
  ( Spring(..)
  , Length
  , Hamiltonian
  , Energy
  , Stiffness
  , force
  , studyToSpring
  , commonEffectSpring
  )
where

import           Data.Meta.Effects
import           Data.Meta.Pairwise.CommonEffect
import           GHC.Generics
import qualified Data.Map.Strict               as Map

type Force = Double

type Energy = Double

-- | The spring's both ends position
data Position =
  Position Double Double
  deriving (Ord, Eq, Show, Generic, Read)

-- | The spring's length
type Length = Double

-- | The Natural length of a spring where no force is exerded on the spring
type NaturalLength = Length

-- | Stiffness coefficient
type Stiffness = Double

class Hamiltonian h where
  energy :: h -> Energy

springLength :: Position -> Length
springLength (Position s e) = e - s

-- | Takes natural length and position of ends and gives stretch positive
-- (squize negative)
stretch :: NaturalLength -> Position -> Length
stretch l p = springLength p - l

-- | Harmonic oscillator. It is defined by its 
-- label comparison stiffness natural length and position
data Spring =
  Spring StringIntId ComparisonId Stiffness NaturalLength Position
  deriving (Ord, Eq, Show)

instance Hamiltonian Spring where
  energy (Spring sid cid k l p) = (1 / 2) * (k * (stretch l p) ^ 2)

force :: Spring -> Force
force (Spring sid cid k l p) = -k * (stretch l p)

contrastToSpring :: (Effect a, Gaussian a) => Contrast a -> String -> Spring
contrastToSpring (Contrast ta tb ef) label = 
  let stiffness     = 1 / variance ef
      naturalLength = point ef
      position      = Position 0.0 naturalLength
   in Spring label (ComparisonId ta tb) stiffness naturalLength position

-- | Create springs of a collection of studies
studiesToSprings 
  :: (Effect a, Gaussian a) => [IVStudy a] -> Maybe TreatmentId -> Either String [Spring]
