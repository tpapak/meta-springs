{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..))
import Data.Meta.Studies (StudyId (..), readStudies)
import Data.Meta.NMA (nmaRegressionBinRE, nmaRegressionBinGroupedREML)
t s = TreatmentId (StringId s); showT (TreatmentId (StringId s))=s; showT _="?"
main = do
  Just cr <- A.decode <$> BL.readFile "/tmp/binnmr/cov5.json" :: IO (Maybe (Map.Map String Double))
  let cov = Map.fromList [ (StudyId (StringId k), v) | (k,v)<-Map.toList cr ]
  Right st <- readStudies "/tmp/binnmr/data5.json"
  case nmaRegressionBinRE st cov (Just 0.1) of
    Left e -> putStrLn ("RE: "++e)
    Right (beta,tau2,cs) -> printf "BinRE shared: beta=%.4f tau2=%.4f A->B=%.4f A->C=%.4f\n"
                              beta tau2 (cs Map.! t "A" Map.! t "B") (cs Map.! t "A" Map.! t "C")
  case nmaRegressionBinGroupedREML st cov of
    Left e -> putStrLn ("EXCH: "++e)
    Right (t2s,b,bm,_) -> do
      printf "BinExch: B=%.4f tau2=%.4f sigmaB2=%.4f\n" b (t2s Map.! 0) (t2s Map.! 1)
      mapM_ (\(tt,v)->printf "  beta[%s]=%.4f\n" (showT tt) v) (Map.toList bm)
