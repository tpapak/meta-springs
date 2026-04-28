module Data.Numerics
  ( Roundable (..)
  , roundDouble
  , populationVariance -- E[X^2] - E[X]^2
  , populationVariance' 
  , sampleVariance' -- Unbiased sample variance
  , sampleVariance -- Unbiased sample variance
  ) where

roundDouble :: Double -> Int -> Double
roundDouble f n =
     (fromInteger $ round $ f * (10^n)) / (10.0^^n)

{-roundEffect :: Effect a => a -> a-}
{-roundEffect e =-}
  {-let -}

class Roundable r where
  roundme :: r -> Int -> r

average :: [Double] -> Double
average la =
  let nn = fromIntegral (length la)
   in sum $ map (\x -> x / nn) la


--[Welford's online algorith](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm)
populationVariance :: [Double] -> Double
populationVariance la =
  let update accum new_value = 
        let (count,meann,mm2) = accum 
            count' = count + 1
            delta = new_value - meann
            meann' = meann + delta / (fromIntegral count')
            delta2 = new_value - meann'
            mm2' = mm2 + delta * delta2
         in (count', meann', mm2')
      (countf, meanf, mm2f) = foldl update (0,0.0,0.0) la
   in mm2f / (fromIntegral countf)

--[Welford's online algorith](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm)
sampleVariance :: [Double] -> Double
sampleVariance la =
  let update accum new_value = 
        let (count,meann,mm2) = accum 
            count' = count + 1
            delta = new_value - meann
            meann' = meann + delta / (fromIntegral count')
            delta2 = new_value - meann'
            mm2' = mm2 + delta * delta2
         in (count', meann', mm2')
      (countf, meanf, mm2f) = foldl update (0,0.0,0.0) la
   in mm2f / (fromIntegral (countf - 1))

populationVariance' :: [Double] -> Double
populationVariance' la =
  let ma = average la
      nn = fromIntegral $ length la
      sqla2 = sum $ map (\x -> x^2/nn) la
   in sqla2 - ma^2

sampleVariance' :: [Double] -> Double
sampleVariance' la =
  let ma = average la
      nn = fromIntegral $ length la
   in sum (map (\x-> (x - ma)^2) la) / (nn - 1)

