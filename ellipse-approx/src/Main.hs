module Main
  ( main
  )
where

{-
  Approximation of ellipse circumference.
  Ref: https://en.wikipedia.org/wiki/Ellipse#Circumference

  Implements the fast converging version as derived by James Ivory and Bessel.
 -}

import Data.Ratio

type RI = Ratio Integer

-- term ((2n-3)!! / (2^n * n!))^2, starting from 2
termParts :: [RI]
termParts = fmap (\x -> x * x) $ zipWith (%) numers denoms
  where
    numers = scanl1 (*) [1, 3 ..]
    denoms = zipWith (*) (fmap (2 ^) [2 :: Integer ..]) (scanl1 (*) [2 ..])

terms :: RI -> [RI]
terms h = zipWith (*) termParts powOfH
  where
    powOfH = h : fmap (* h) powOfH

mulTerm :: RI -> [RI]
mulTerm h = fmap (\s -> 1 + h / 4 + s) $ scanl1 (+) (terms h)

circum :: Integer -> Integer -> [Double]
circum a b = fmap (\t -> pi * fromIntegral (a + b) * realToFrac t) (mulTerm h)
  where
    h = ((a - b) * (a - b)) % ((a + b) * (a + b))

main :: IO ()
main = print (take 10 (circum 1234 1000))
