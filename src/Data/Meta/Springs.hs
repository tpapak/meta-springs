{-# Language DeriveGeneric #-}
module Data.Meta.Springs
  ( Spring (..)
  , Length
  , Hamiltonian
  , Energy
  , Stiffness
  , force
  ) where

import GHC.Generics
import Data.Meta.Effects

type Force = Double
type Energy = Double
-- | One end of the spring.
type Start = Double
-- | Other end of the spring.
type End = Double
-- | The spring's both ends position
data Position = Position Start End
  deriving (Ord, Eq, Show, Generic, Read)
-- | The spring's length
type Length = Double
-- | The Natural length of a spring where no force is exerded on the spring
type NaturalLength = Length
-- | Stiffness coefficient
type Stiffness = Double


class Hamiltonian h where
  energy :: h -> Position -> Energy

springLength :: Position -> Length
springLength (Position s e) = e - s 

-- | Takes natural length and position of ends and gives stretch positive
-- (squize negative)
stretch :: NaturalLength -> Position -> Length
stretch l p =  (springLength p) - l

-- Harmonic oscillator. It is defined by its stiffness and natural position.
data Spring = Spring Stiffness NaturalLength
  deriving (Ord, Eq, Show)
instance Hamiltonian Spring where
  energy (Spring k l) p
    = 1/2 * k * (stretch l p)^2

force :: Spring -> Position -> Force
force (Spring k l) p = - k * (stretch l p)
