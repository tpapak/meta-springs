{-# LANGUAGE OverloadedStrings #-}
-- | Compute per-study contributions in Haskell using the same hat
-- matrix and netmeta-internal IV weights, then write them out for
-- comparison against CINeMA's `studyContributions`.

module Main where

import           Data.Aeson           (FromJSON (..), Value (..),
                                       eitherDecode, encode, object,
                                       withObject, (.:), (.=))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import qualified Data.Text.Read       as TR
import           System.Exit          (exitFailure)

import qualified Data.Meta.Contribution as C
import qualified Data.Meta.Effects      as MetaEff
import qualified Data.Meta.Hatmatrix    as Hat

-- | Re-implementation of `Hat.parseJSON ComparisonId` for use outside
-- aeson's automatic instance plumbing.
parseComp :: String -> MetaEff.ComparisonId
parseComp s =
  let parts = T.splitOn (T.pack ":") (T.pack s)
      tid t =
        let etx = TR.decimal t
        in case etx of
             Right (n, rst) | T.null rst -> MetaEff.TreatmentId (MetaEff.IntId n)
             _                          -> MetaEff.TreatmentId (MetaEff.StringId (T.unpack t))
  in case parts of
       [a, b] -> MetaEff.ComparisonId (tid a) (tid b)
       _      -> error ("bad comparison id: " ++ s)

-- | Render a TreatmentId without slmm's `newtype` constructor prefix.
showTid :: MetaEff.TreatmentId -> String
showTid (MetaEff.TreatmentId inner) = show inner

showComp :: MetaEff.ComparisonId -> String
showComp (MetaEff.ComparisonId a b) = showTid a ++ ":" ++ showTid b

-- | One IV-weight record from the JSON file.
data WRec = WRec { wStudy :: String, wComp :: String, wWeight :: Double }
  deriving Show
instance FromJSON WRec where
  parseJSON = withObject "WRec" $ \o -> WRec
    <$> o .: "study"
    <*> o .: "comparison"
    <*> o .: "weight"

main :: IO ()
main = do
  hmRaw <- B.readFile "test/contribution/synthetic/hatmatrix.json"
  wRaw  <- B.readFile "test/contribution/synthetic/iv_weights_netmeta.json"
  case (,) <$> (eitherDecode hmRaw :: Either String Hat.HatMatrixRaw)
           <*> (eitherDecode wRaw  :: Either String [WRec]) of
    Left e -> do putStrLn ("parse error: " ++ e); exitFailure
    Right (els, ws) -> do
      let hm = Hat.hatMatrixFromList els
          cm = C.contributionMatrixShortestPath hm
          pwWeights = [ C.PairwiseWeight (wStudy w)
                                         (parseComp (wComp w))
                                         (wWeight w)
                      | w <- ws ]
          rows = C.studyContributions cm pwWeights
          out  =
            [ object
                [ "comparison"   .= showComp (C.scComparison r)
                , "study"        .= C.scStudy r
                , "contribution" .= C.scContribution r
                ]
            | r <- rows ]
      B.writeFile "test/contribution/synthetic/haskell_study.json"
                  (encode (A.toJSON out))
      putStrLn $ "wrote haskell_study.json (" ++ show (length out) ++ " rows)"
