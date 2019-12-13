module Main where

import qualified Data.Meta.Effects as Meta

import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG.CmdLine as DC

myCircle :: D.Diagram DC.B
myCircle = D.circle 1

main = DC.mainWith myCircle
