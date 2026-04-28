{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson           (eitherDecode, encode, object, (.=))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict      as Map
import           System.Exit          (exitFailure)

import qualified Data.Meta.Contribution as C
import qualified Data.Meta.Effects      as MetaEff
import qualified Data.Meta.Hatmatrix    as Hat

-- | Render a TreatmentId without slmm's `newtype` constructor prefix.
-- Mirrors `nma-contribution`'s hand-written Show so JSON keys come out
-- as @"5"@ / @"A"@ instead of @"TreatmentId 5"@.
showTid :: MetaEff.TreatmentId -> String
showTid (MetaEff.TreatmentId inner) = show inner

showComp :: MetaEff.ComparisonId -> String
showComp (MetaEff.ComparisonId a b) = showTid a ++ ":" ++ showTid b

main :: IO ()
main = do
  raw <- B.readFile "test/contribution/synthetic/hatmatrix.json"
  case eitherDecode raw :: Either String Hat.HatMatrixRaw of
    Left e -> do
      putStrLn ("parse error: " ++ e)
      exitFailure
    Right els -> do
      let hm = Hat.hatMatrixFromList els
          C.ContributionMatrix cm = C.contributionMatrixShortestPath hm
          wide =
            [ object [ "row"        .= showComp rid
                     , "comparison" .= showComp cid
                     , "value"      .= v
                     ]
            | (rid, cr) <- Map.toList cm
            , (cid, v) <- Map.toList cr
            ]
      B.writeFile "test/contribution/synthetic/haskell_contrib.json"
                  (encode (A.toJSON wide))
      putStrLn $ "wrote haskell_contrib.json (" ++ show (length wide) ++ " elements)"
