{-|
Module      : Data.Meta.Studies
Description : Study definitions
Copyright   : (c) Thodoris Papakonstantinou, 2018
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Study definitions, import, manipulations
-}

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings       #-}

module Data.Meta.Studies
  ( StudyId (..)
  , Study (..)
  , IVStudy (..)
  , CSVStudy (..)
  , StudyGraph (..) -- |for one stage nma
  , StudyGraph' (..) -- |for two stage nma
  , getStudyId
  , getStudyArms
  , csvStudyToComparison
  , csvToStudyArms
  , mergeStudyArms
  , studiesGraph'
  , studyToIVStudy
  , getEffectsOfIVStudy
  , readStudies
  ) where

import           Control.Applicative
import           Data.List.Split
import           Data.Tuple.Extra
import qualified Data.Map.Strict     as Map
import qualified Data.Set as Set
import           Data.Maybe
import           Data.Either
import Data.List
import qualified Data.IntMap.Strict as IM
import Data.Text.Lazy (Text)
import           GHC.Generics
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text           as T

import Data.Graph.AdjacencyList
import Data.Meta.Effects
-- import Data.Meta.Multiarm  -- unused: spring NMA handles multiarm directly

newtype StudyId = StudyId StringIntId
  deriving (Generic, Show, Read,Ord,Eq)
instance ToJSON StudyId
instance FromJSON StudyId
  where
    parseJSON = do
      let outint = withScientific "StudyId"
                    $ \tid -> return (StudyId $ IntId (floor tid))
          outstr = withText "StudyId"
                    $ \tid -> return (StudyId $ StringId (T.unpack tid))
       in (\v -> outint v <|> outstr v)

-- | Study as a collection of treatments (arms). This definitions coveres
-- multiarm studies
data Study = BinaryStudy StudyId [Arm]
           | ContinuousStudy StudyId [Arm]
  deriving (Show, Read, Ord, Eq, Generic)
instance ToJSON Study
instance FromJSON Study

-- | Easy Study definition for csv reading studies
data CSVStudy =
      -- |Constructor for continuous outcomes 
      --  ID, mean, standard deviation, sample size of comparison
      CSVContinuousWide { study :: StudyId
                        , treatmentA :: TreatmentId
                        , meanA :: !Double
                        , sdA :: !Double
                        , nA :: !Int
                        , treatmentB :: TreatmentId
                        , meanB :: !Double
                        , sdB :: !Double
                        , nB :: !Int
                        }
      -- |Constructor for binary outcomes 
      --  ID, events and number of participants
      | CSVBinaryWide { study :: StudyId
                      , treatmentA :: TreatmentId
                      , eventsA :: !Int
                      , nA :: !Int
                      , treatmentB :: TreatmentId
                      , eventsB :: !Int
                      , nB :: !Int
                      }
       | CSVContinuousLong { study :: StudyId
                           , treatment :: TreatmentId
                           , mean :: !Double
                           , sd :: !Double
                           , n :: !Int
                           }
       | CSVBinaryLong { study :: StudyId
                       , treatment :: TreatmentId
                       , events :: !Int
                       , n :: !Int
                       }
  deriving (Generic,Read,Ord,Eq,Show)
instance ToJSON CSVStudy
instance FromJSON CSVStudy
  where
    parseJSON =
      let contwide v = CSVContinuousWide <$> v .: "study"
                        <*> v .: "treatmentA"
                        <*> v .: "meanA"
                        <*> v .: "sdA"
                        <*> v .: "nA"
                        <*> v .: "treatmentB"
                        <*> v .: "meanB"
                        <*> v .: "sdB"
                        <*> v .: "nB"
          contlong v = CSVContinuousLong <$> v .: "study"
                        <*> v .: "treatment"
                        <*> v .: "mean"
                        <*> v .: "sd"
                        <*> v .: "n"
          binwide v = CSVBinaryWide <$> v .: "study"
                        <*> v .: "treatmentA"
                        <*> v .: "eventsA"
                        <*> v .: "nA"
                        <*> v .: "treatmentB"
                        <*> v .: "eventsB"
                        <*> v .: "nB"
          binlong v = CSVBinaryLong <$> v .: "study"
                        <*> v .: "treatment"
                        <*> v .: "events"
                        <*> v .: "n"
       in (\(Object v) -> contwide v <|> contlong v <|> binwide v <|> binlong v)

-- | Data structure for representing studies for a two stage approach
data StudyGraph' a =
  StudyGraph' { directGraph' :: Graph
              , vsts' :: IM.IntMap TreatmentId
              , tsvs' :: Map.Map TreatmentId Vertex
              --, studies' :: Map.Map ComparisonId [(StudyId, a)]
              --, studies :: [(StudyId, Contrast a)]
              --, studies :: [IVStudy a]
              , studies' :: [[Either String (StudyId, Contrast a)]]
              , directEffects' :: Map.Map ComparisonId a -- ^ summary of studies for two stage nma
              }
  deriving (Show, Eq)

-- | Data structure for representing whole network of studies
data StudyGraph =
  StudyGraph { studyGraph :: Graph
             , directGraph :: Graph
             , vsts :: IM.IntMap TreatmentId
             , tsvs :: Map.Map TreatmentId Vertex
             --, studies :: Map.Map ComparisonId [(StudyId, a)]
             --, studies :: [(StudyId, Contrast a)]
             --, studies :: [IVStudy a]
             --, studies :: [[Either String (StudyId, Contrast a)]]
             --, directEffects :: Map.Map TreatmentId (Map.Map TreatmentId a) -- ^ summary of studies for two stage nma
             }
  deriving (Show, Eq)

getStudyId :: Study -> StudyId
getStudyId (BinaryStudy sid _) = sid
getStudyId (ContinuousStudy sid _) = sid

getStudyArms :: Study -> [Arm]
getStudyArms (BinaryStudy sid arms) = arms
getStudyArms (ContinuousStudy sid arms) = arms

-- | Study as inverse variance estimates
data IVStudy a = IVStudy StudyId (Contrasts a)
  deriving (Show, Read, Ord, Eq)

csvStudyToComparison :: CSVStudy -> Comparison
csvStudyToComparison (CSVBinaryWide sid ta ea na tb eb nb)
  = ( BinaryArm ta ea na
    , BinaryArm tb eb nb
    )
csvStudyToComparison s
  = ( ContinuousArm (treatmentA s) (meanA s) (sdA s) (nA s)
    , ContinuousArm (treatmentB s) (meanB s) (sdB s) (nB s)
    )

csvToStudyArms :: CSVStudy -> Study
csvToStudyArms (CSVContinuousWide sid ta ma sa na tb mb sb nb) =
  let pwst = CSVContinuousWide sid ta ma sa na tb mb sb nb
      comparison = csvStudyToComparison pwst
   in ContinuousStudy sid [fst comparison, snd comparison]
csvToStudyArms (CSVBinaryWide sid ta ea na tb eb nb) =
  let pwst = CSVBinaryWide sid ta ea na tb eb nb
      comparison = csvStudyToComparison pwst
   in BinaryStudy sid [fst comparison, snd comparison]
csvToStudyArms (CSVContinuousLong sid t m sd n) =
  let pwst = CSVContinuousLong sid t m sd n
   in ContinuousStudy sid [ContinuousArm t m sd n]
csvToStudyArms (CSVBinaryLong sid t r n) =
  let pwst = CSVBinaryLong sid t r n
   in BinaryStudy sid [BinaryArm t r n]

outcomeType :: [Study] -> Either String String
outcomeType studies =
  let allbinaries = all (\st -> case st of
                                  BinaryStudy sid arms -> True
                                  _ -> False) studies
      allcontinuous = all (\st -> case st of
                                  ContinuousStudy sid arms -> True
                                  _ -> False) studies
   in if allbinaries && not allcontinuous
         then Right "binaries"
         else
           if allcontinuous
              then Right "continuous"
              else Left "mixed types"

mergeStudyArms :: [Study] -> Either String [Study]
mergeStudyArms studyRows =
  case outcomeType studyRows of
    Right "binaries" ->
      let studyMap = foldl' (\ac (BinaryStudy st arms)
                              -> Map.insertWith union st arms ac)
                              Map.empty studyRows
       in Right $ map (\(stid, arms) -> (BinaryStudy stid arms)) $ Map.toList studyMap
    Right "continuous" ->
      let studyMap = foldl' (\ac (ContinuousStudy st arms)
                              -> Map.insertWith union st arms ac)
                              Map.empty studyRows
       in Right $ map (\(stid, arms) -> (ContinuousStudy stid arms)) $ Map.toList studyMap
    Left err -> Left err

getEffectsOfIVStudy :: Effect a => IVStudy a -> [a]
getEffectsOfIVStudy (IVStudy st contrs) =
  let cts = contrastsToList contrs
   in map (\(Contrast ta tb ef) -> ef) cts

studyToIVStudy :: Effect a => Study
                -> (Comparison -> Either String a)
                -> Either String (IVStudy a)
studyToIVStudy st getEffect =
  let sid = getStudyId st
      arms = getStudyArms st
      eef = sequence $ map (comparisonToContrast getEffect) $ armsToComparisons arms
      -- armsToComparisons sorts arms
   in case eef of
        Left err -> Left err
        Right contrasts ->
          let cntrs = foldl (\acc (Contrast ta tb ef) ->
                            Map.insert (ComparisonId ta tb) ef acc)
                         Map.empty contrasts
           in Right $ IVStudy sid (Contrasts cntrs)

-- | Convert list of studies into their network
studiesGraph' :: (Effect e, Gaussian e, InverseVariance e) => [IVStudy e]
              -> Either String (StudyGraph' e)
studiesGraph' studyList =
  let econtrasts' = map (\(IVStudy sid cnts) ->
                     let rawcls = contrastsToList cnts
                      in map (\contrast -> Right (sid, contrast) ) rawcls)
                               studyList
      econtrasts = sequence $ concat econtrasts'
   in case econtrasts of
        Left err -> Left err
        Right contrasts ->
          let studiesEffects = foldl' (\ac (sid, Contrast ta tb e) ->
                    let cid = ComparisonId ta tb
                     in Map.insertWith (\[new] old ->
                         if Data.List.null old then [new]
                                               else new : old
                               ) cid [(sid, e)] ac) Map.empty contrasts
              treatments = foldl' (\ac (ComparisonId a b)
                  -> Set.insert a (Set.insert b ac)) Set.empty $ Map.keys studiesEffects
              vsts = IM.fromList $ zip [1..] (Set.toList treatments)
              tsvs = Map.fromList $ zip (Set.toList treatments) [1..]
              es = let comparisons = Map.keys studiesEffects
                    in map (\(ComparisonId a b) ->
                      let va = fromJust $ Map.lookup a tsvs
                          vb = fromJust $ Map.lookup b tsvs
                      in Edge va vb) comparisons
              gr = graphFromEdges es

              directEffects =
                --map (\(Edge va vb) -> ) es
                Map.map (\sts -> inverseVariance (map (\(sid, e) -> e) sts)) studiesEffects
           in Right $ StudyGraph' { directGraph' = gr
                                 , vsts' = vsts
                                 , tsvs' = tsvs
                                 , studies' = econtrasts'
                                 , directEffects' = directEffects
                                 }

-- | Read JSON file with studies.
readStudies :: String -> IO (Either String [Study])
readStudies studiesFileName = do
  let jsonData = B.readFile studiesFileName
  ecsvstudies <- (eitherDecode <$> jsonData) :: IO (Either String [CSVStudy])
  case ecsvstudies of
    Left  err ->
      let errormsg = "CSVContinuousWide <$> v .: 'study' \n\
                     \              <*> v .: 'treatmentA' \n\
                     \              <*> v .: 'meanA' \n\
                     \              <*> v .: 'sdA' \n\
                     \              <*> v .: 'nA' \n\
                     \              <*> v .: 'treatmentB' \n\
                     \              <*> v .: 'meanB' \n\
                     \              <*> v .: 'sdB' \n\
                     \              <*> v .: 'nB' \n\
                     \contlong v = CSVContinuousLong <$> v .: 'study' \n\
                     \              <*> v .: 'treatment' \n\
                     \              <*> v .: 'mean' \n\
                     \              <*> v .: 'sd' \n\
                     \              <*> v .: 'n' \n\
                     \binwide v = CSVBinaryWide <$> v .: 'study' \n\
                     \              <*> v .: 'treatmentA' \n\
                     \              <*> v .: 'eventsA' \n\
                     \              <*> v .: 'nA' \n\
                     \              <*> v .: 'treatmentB' \n\
                     \              <*> v .: 'eventsB' \n\
                     \              <*> v .: 'nB' \n\
                     \binlong v = CSVBinaryLong <$> v .: 'study' \n\
                     \              <*> v .: 'treatment' \n\
                     \              <*> v .: 'events' \n\
                     \              <*> v .: 'n'" :: String
       in return $ Left (err <> errormsg)
    Right csvstudies -> do
     let arms = map csvToStudyArms csvstudies
         estudies = mergeStudyArms arms
      in case estudies of
           Left err -> return $ Left $ "Error merging arms to studies" <> err
           Right studies -> do
             return $ Right studies
