{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..), meanArm)
import Data.Meta.Studies (StudyId (..), readStudies)
import Data.Meta.NMA (nmaRegressionPosteriorB, RegPosteriorB (..), BetaModel (..), Tau2PriorR (..))

showT :: TreatmentId -> String
showT (TreatmentId (StringId s)) = s
showT (TreatmentId (IntId i)) = show i

main :: IO ()
main = do
  Just covRaw <- A.decode <$> BL.readFile "/tmp/nmr5/cov.json" :: IO (Maybe (Map.Map String Double))
  let cov = Map.fromList [ (StudyId (StringId k), v) | (k,v) <- Map.toList covRaw ]
  Right studies <- readStudies "/tmp/nmr5/data.json"
  mapM_ (\(name,bm) -> case nmaRegressionPosteriorB studies cov 120 bm (HalfNormalTauR 0.5) meanArm of
      Left e -> putStrLn (name ++ ": ERR " ++ e)
      Right rp -> do
        printf "\n=== %s ===\n" name
        case rpbCommonB rp of Just (m,s,lo,hi) -> printf "  common B : %.3f (sd %.3f) [%.3f, %.3f]\n" m s lo hi; Nothing -> pure ()
        mapM_ (\(t,(m,s,lo,hi)) -> printf "  beta[%s] : %.3f (sd %.3f) [%.3f, %.3f]\n" (showT t) m s lo hi) (rpbBetas rp)
        let (tm,ts,tlo,thi) = rpbTau2 rp
        printf "  tau2     : %.4f (sd %.4f) [%.4f, %.4f]\n" tm ts tlo thi
        case rpbSigmaB2 rp of Just (m,s,lo,hi) -> printf "  sigmaB2  : %.4f (sd %.4f) [%.4f, %.4f]\n" m s lo hi; Nothing -> pure ()
    ) [("SHARED",SharedB),("UNRELATED",UnrelatedB),("EXCHANGEABLE",ExchangeableB)]
