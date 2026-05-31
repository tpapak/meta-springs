module Test.Meta.Multiarm where

import           Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import qualified Data.Map.Strict               as Map
import qualified Data.Vector                   as V
import qualified Data.Csv                      as C
import           Data.Either

import           TestHS

import           Data.Numerics
import           Data.Meta.Effects
import           Data.Meta.Multiarm

ioTests :: [IO Test]
ioTests = [fourArm]

fourArm :: IO Test
fourArm = do
  let name = "Reweight 4 arm study"
  let
    cts =
      [ (Contrast (TreatmentId (IntId 1))
                  (TreatmentId (IntId 6))
                  (LogOR (-0.741) (0.523^2))
        )
      , (Contrast (TreatmentId (IntId 1))
                  (TreatmentId (IntId 7))
                  (LogOR (-0.887) (0.529^2))
        )
      , (Contrast (TreatmentId (IntId 1))
                  (TreatmentId (IntId 9))
                  (LogOR (-0.462) (0.516^2))
        )
      , (Contrast (TreatmentId (IntId 6))
                  (TreatmentId (IntId 7))
                  (LogOR (-0.147) (0.542^2))
        )
      , (Contrast (TreatmentId (IntId 6))
                  (TreatmentId (IntId 9))
                  (LogOR 0.278 (0.528^2))
        )
      , (Contrast (TreatmentId (IntId 7))
                  (TreatmentId (IntId 9))
                  (LogOR 0.425 (0.535^2))
        )
      ]
  let
    netmetaWeighted = map (\(Contrast ta tb ef) ->
                            (Contrast ta tb (mapEstimate (flip roundDouble 4) ef))
                          )
      [ (Contrast (TreatmentId (IntId 1))
                  (TreatmentId (IntId 6))
                  (LogOR (-0.741) (0.7319524^2))
        )
      , (Contrast (TreatmentId (IntId 1))
                  (TreatmentId (IntId 7))
                  (LogOR (-0.887) (0.7463111^2))
        )
      , (Contrast (TreatmentId (IntId 1))
                  (TreatmentId (IntId 9))
                  (LogOR (-0.462) (0.7134189^2))
        )
      , (Contrast (TreatmentId (IntId 6))
                  (TreatmentId (IntId 7))
                  (LogOR (-0.147) (0.7873284^2))
        )
      , (Contrast (TreatmentId (IntId 6))
                  (TreatmentId (IntId 9))
                  (LogOR 0.278  (0.7451525^2))
        )
      , (Contrast (TreatmentId (IntId 7))
                  (TreatmentId (IntId 9))
                  (LogOR 0.425  (0.7659215^2))
        )
      ]
  let ereweighted = reweightMultiArm cts
  case ereweighted of
      Left err ->
         return $ testFailed name $ (show netmetaWeighted, show err)
      Right reweighted' ->
        let reweighted = map (\(Contrast ta tb ef) ->
                               (Contrast ta tb (mapEstimate (flip roundDouble 4) ef))
                             ) reweighted'
            seTEs = map (\(Contrast ta tb ef) -> sqrt (variance ef)
                        ) reweighted
         in if netmetaWeighted == reweighted
               then
                 return $ testPassed name "passed!"
               else
                 return $ testFailed name $ (show netmetaWeighted, show reweighted)
