{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..))
import Data.Meta.Studies (StudyId (..), readStudies)
import Data.Meta.NMA (nmaRegressionBinPosteriorB, RegPosteriorB (..), BetaModel (..), Tau2PriorR (..))
showT (TreatmentId (StringId s))=s; showT _="?"
main = do
  Just cr <- A.decode <$> BL.readFile "/tmp/binnmr/cov5.json" :: IO (Maybe (Map.Map String Double))
  let cov = Map.fromList [ (StudyId (StringId k), v) | (k,v)<-Map.toList cr ]
  Right st <- readStudies "/tmp/binnmr/data5.json"
  case nmaRegressionBinPosteriorB st cov 120 ExchangeableB (HalfNormalTauR 0.5) (UniformSD 0.9612) of
    Left e -> putStrLn e
    Right rp -> do
      case rpbCommonB rp of Just (m,s,lo,hi)->printf "B: mean %.4f [%.4f,%.4f]\n" m lo hi; _->pure()
      mapM_ (\(t,(m,s,lo,hi))->printf "  beta[%s]: %.4f\n" (showT t) m) (rpbBetas rp)
      let (tm,_,_,_)=rpbTau2 rp; (sm,_,_,_)=maybe (0,0,0,0) id (rpbSigmaB2 rp)
      printf "tau2: mean %.4f  mode %.4f\n" tm (rpbModeTau2 rp)
      printf "sigmaB2: MEAN %.4f  mode %.4f\n" sm (maybe (0/0) id (rpbModeSigmaB2 rp))
