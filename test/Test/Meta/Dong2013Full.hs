{-# LANGUAGE BangPatterns #-}
-- | Dong2013 full dataset: binomial spring vs netmetabin MH.
-- 38 studies (excluding 3 with 0/0 in all arms), 6 treatments.
module Test.Meta.Dong2013Full where

import qualified Data.Map.Strict as Map
import Data.Meta.Effects
import Data.Meta.NMA
import Data.Meta.RandomEffects
import Data.Meta.Studies

mkSid :: Int -> StudyId
mkSid = StudyId . IntId

-- Treatment IDs matching netmetabin alphabetical order:
-- ICS=1, LABA=2, LABA-ICS=3, Placebo=4, TIO-HH=5, TIO-SMI=6
tICS  = TreatmentId (StringId "ICS")
tLABA = TreatmentId (StringId "LABA")
tLAIC = TreatmentId (StringId "LABA-ICS")
tPlac = TreatmentId (StringId "Placebo")
tHH   = TreatmentId (StringId "TIO-HH")
tSMI  = TreatmentId (StringId "TIO-SMI")

dong2013 :: [Study]
dong2013 =
  [ BinaryStudy (mkSid 1)  [BinaryArm tSMI 52 1989, BinaryArm tPlac 38 2002]
  , BinaryStudy (mkSid 2)  [BinaryArm tSMI 35 1337, BinaryArm tPlac  9  653]
  , BinaryStudy (mkSid 3)  [BinaryArm tHH 381 2986, BinaryArm tPlac 411 3006]
  , BinaryStudy (mkSid 4)  [BinaryArm tHH   3  266, BinaryArm tPlac   6  288]
  , BinaryStudy (mkSid 6)  [BinaryArm tHH  15  608, BinaryArm tPlac   4  305]
  , BinaryStudy (mkSid 7)  [BinaryArm tHH   1   69, BinaryArm tPlac   2   73]
  , BinaryStudy (mkSid 8)  [BinaryArm tHH   7  500, BinaryArm tPlac   8  510]
  , BinaryStudy (mkSid 9)  [BinaryArm tHH   1   55, BinaryArm tPlac   0   53]
  , BinaryStudy (mkSid 10) [BinaryArm tHH  22  914, BinaryArm tPlac  19  915]
  , BinaryStudy (mkSid 11) [BinaryArm tHH   7  550, BinaryArm tPlac   7  317]
  , BinaryStudy (mkSid 12) [BinaryArm tHH  64 3707, BinaryArm tLABA 78 3669]
  , BinaryStudy (mkSid 13) [BinaryArm tHH   1  402, BinaryArm tLABA  6  405, BinaryArm tPlac  5  400]
  , BinaryStudy (mkSid 14) [BinaryArm tHH  38  665, BinaryArm tLAIC 21  658]
  , BinaryStudy (mkSid 15) [BinaryArm tLABA  6  316, BinaryArm tPlac  5  318]
  , BinaryStudy (mkSid 16) [BinaryArm tLABA  3  440, BinaryArm tPlac  0  217]
  , BinaryStudy (mkSid 17) [BinaryArm tLABA  1  201, BinaryArm tPlac  2  207]
  , BinaryStudy (mkSid 18) [BinaryArm tICS   5  127, BinaryArm tPlac  5  127]
  , BinaryStudy (mkSid 19) [BinaryArm tICS   3  128, BinaryArm tPlac  0  132]
  , BinaryStudy (mkSid 20) [BinaryArm tICS   4  123, BinaryArm tPlac  0  121]
  , BinaryStudy (mkSid 21) [BinaryArm tICS  32  376, BinaryArm tPlac 36  375]
  , BinaryStudy (mkSid 22) [BinaryArm tICS   8  634, BinaryArm tPlac 10  643]
  , BinaryStudy (mkSid 23) [BinaryArm tICS   4  145, BinaryArm tPlac  5  145]
  , BinaryStudy (mkSid 24) [BinaryArm tICS   0  142, BinaryArm tPlac  2  139]
  , BinaryStudy (mkSid 26) [BinaryArm tLAIC  6  479, BinaryArm tLABA  0  239]
  , BinaryStudy (mkSid 27) [BinaryArm tLAIC  4  394, BinaryArm tLABA  6  403]
  , BinaryStudy (mkSid 28) [BinaryArm tLAIC  6  394, BinaryArm tLABA  3  388]
  , BinaryStudy (mkSid 29) [BinaryArm tLAIC  7  507, BinaryArm tLABA  9  487]
  , BinaryStudy (mkSid 30) [BinaryArm tLAIC  2  189, BinaryArm tLABA  4  184]
  , BinaryStudy (mkSid 31) [BinaryArm tLAIC  5   92, BinaryArm tLABA  7   94]
  , BinaryStudy (mkSid 32) [BinaryArm tLAIC  7  988, BinaryArm tLABA  4  495, BinaryArm tPlac  4  481]
  , BinaryStudy (mkSid 33) [BinaryArm tLAIC  1  131, BinaryArm tLABA  0  131, BinaryArm tPlac  0  125]
  , BinaryStudy (mkSid 34) [BinaryArm tLAIC  2  297, BinaryArm tPlac  0  148]
  , BinaryStudy (mkSid 35) [BinaryArm tLAIC  7  845, BinaryArm tLABA  1  284, BinaryArm tICS  2  275, BinaryArm tPlac  1  300]
  , BinaryStudy (mkSid 36) [BinaryArm tLAIC 193 1546, BinaryArm tLABA 205 1542, BinaryArm tICS 246 1552, BinaryArm tPlac 231 1544]
  , BinaryStudy (mkSid 38) [BinaryArm tLAIC  2  358, BinaryArm tLABA  3  372, BinaryArm tICS  3  374, BinaryArm tPlac  7  361]
  , BinaryStudy (mkSid 39) [BinaryArm tLAIC  5  254, BinaryArm tLABA 13  255, BinaryArm tICS  6  257, BinaryArm tPlac  5  256]
  , BinaryStudy (mkSid 40) [BinaryArm tLAIC  6  208, BinaryArm tLABA  6  201, BinaryArm tICS  5  198, BinaryArm tPlac  9  205]
  , BinaryStudy (mkSid 41) [BinaryArm tLAIC  0  165, BinaryArm tLABA  0  160, BinaryArm tICS  0  168, BinaryArm tPlac  3  181]
  ]

main :: IO ()
main = do
  case makeBinomialSprings dong2013 Nothing Nothing of
    Left err -> putStrLn $ "Error: " ++ err
    Right net -> do
      let solved = irlsSolve net 100 1e-10
          ests = networkEstimates solved
          trts = [tICS, tLABA, tLAIC, tPlac, tHH, tSMI]
          names = ["ICS", "LABA", "LABA-ICS", "Placebo", "TIO-HH", "TIO-SMI"]
      putStrLn "=== Binomial Spring CE (Dong2013 full, excl 0/0) ==="
      putStrLn ""
      -- Print header
      putStr $ padR 10 ""
      mapM_ (\n -> putStr $ padR 10 n) names
      putStrLn ""
      -- Print rows (negate to match netmeta convention: θ_col - θ_row)
      mapM_ (\(tRow, nRow) -> do
        putStr $ padR 10 nRow
        mapM_ (\tCol ->
          let v = case Map.lookup tRow ests of
                    Just m -> case Map.lookup tCol m of
                                Just x -> -x  -- negate to match netmeta
                                Nothing -> 0
                    Nothing -> 0
           in putStr $ padR 10 (showR 6 v)
          ) trts
        putStrLn ""
        ) (zip trts names)

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '

showR :: Int -> Double -> String
showR d x =
  let s = show (fromIntegral (round (x * 10^d)) / 10^(fromIntegral d))
   in s
