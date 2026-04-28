{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import           Data.Meta.Studies
import           Data.Meta.Effects
import           Data.Meta.NMA
import           Data.Meta.RandomEffects

import qualified Data.ByteString.Lazy as B
import System.Environment

import           Data.Numerics
import           Data.Aeson
import qualified Data.Map.Strict               as Map
import qualified Data.Vector                   as V

main :: IO ()
main = do
  putStrLn "running meta-analysis"
  args <- getArgs
  let studiesFile = args !! 0
      method' = args !! 1 
      tau2' = read (args !! 2) :: Double
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> putStr $ show ("Error merging arms to studies", err)
    Right studies -> do
      let method = case method' of
                    _ -> springREML
          enetes = method studies logOdds (Just tau2') Nothing Nothing 
       in case enetes of
            Left err -> putStr $  show ("no nets \n", show studies <> "\n" <> show enetes)
            Right (netes, taus) -> do
              let foundEnergy = (tauEnergy $ springNetwork netes)
                  numberOfArms = length $ tauEdges $ springNetwork netes
                  dofs = fromIntegral $ dof $ springNetwork netes 
                  foundTauSquare = tau2 $ springNetwork netes
                  qqT = (qqTau $ springNetwork netes)
              putStr $ "\n" <> "number of Arms " <> show numberOfArms
              putStr $  "\n" <> "Tau2 :" <> show foundTauSquare <> "\n"
              putStr $ "found TauSq: " <> show taus <> "\n"
              putStr $  "\n" <> "Q - dof = " <> show (2 * foundEnergy - dofs)<> "\n"
              putStr $ show $ networkEstimates netes
