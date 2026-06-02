{-# LANGUAGE OverloadedStrings #-}
-- Dump the spring joint (β,τ²) posterior as JSON densities for the HTML overlay.
-- args: <studiesFile.json> <covFile.json> <outFile.json>   (half-normal(0.5) τ prior)
module Main where

import qualified Data.Aeson           as A
import           Data.Aeson           ((.=), object)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import           System.Environment   (getArgs)

import Data.Meta.Effects (TreatmentId (..), StringIntId (..), meanArm)
import Data.Meta.Studies (StudyId (..), readStudies)
import Data.Meta.NMA     (nmaRegressionPosterior, linearizeBinaryRegression, RegPosterior (..), Tau2PriorR (..))

gaussPdf :: Double -> Double -> Double -> Double
gaussPdf m v x = exp (negate ((x-m)^2) / (2*v)) / sqrt (2*pi*v)

main :: IO ()
main = do
  [sf, cf, outf] <- getArgs
  Just covRaw <- A.decode <$> BL.readFile cf :: IO (Maybe (Map.Map String Double))
  let cov = Map.fromList [ (StudyId (StringId k), v) | (k,v) <- Map.toList covRaw ]
  Right binStudies <- readStudies sf
  let Right studies = linearizeBinaryRegression binStudies cov
  case nmaRegressionPosterior studies cov 240 (HalfNormalTauR 0.5) meanArm of
    Left e  -> putStrLn ("ERR " ++ e)
    Right rp -> do
      let (bm,bs,bl,bh) = rpBeta rp
          (tmo,tme,tl,th) = rpTau2 rp
          grid = rpGrid rp                                   -- (τ², w, βhat, βvar)
          ts   = [ t | (t,_,_,_) <- grid ]
          ws   = [ w | (_,w,_,_) <- grid ]
          dt   = (last ts - head ts) / fromIntegral (length ts - 1)
          -- β mixture density on a grid
          bgrid = [ bm - 4.5*bs + 9*bs*fromIntegral i/200 | i <- [0..200::Int] ]
          bdens = [ sum [ w * gaussPdf bh bv x | (_,w,bh,bv) <- grid ] | x <- bgrid ]
          -- τ² density = mass / dt
          t2dens = map (/ dt) ws
          j = object
              [ "beta" .= object ["mean".=bm,"sd".=bs,"lo".=bl,"hi".=bh]
              , "tau2" .= object ["mode".=tmo,"mean".=tme,"lo".=tl,"hi".=th]
              , "betaGrid" .= bgrid, "betaDens" .= bdens
              , "tau2Grid" .= ts,    "tau2Dens" .= t2dens ]
      BL.writeFile outf (A.encode j)
      putStrLn ("wrote " ++ outf ++ "  beta=" ++ show bm ++ "  tau2 mode=" ++ show tmo)
