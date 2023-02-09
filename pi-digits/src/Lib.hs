{-# LANGUAGE ParallelListComp #-}

module Lib (
  main,
) where

import Data.Ratio

-- most of the following originally from exact-pi

chudnovsky :: [Rational]
chudnovsky = [426880 / s | s <- partials]
  where
    lk = iterate (+ 545140134) 13591409
    xk = iterate (* (-262537412640768000)) 1
    kk = iterate (+ 12) 6
    mk = 1 : [m * ((k ^ (3 :: Int) - 16 * k) % (n + 1) ^ (3 :: Int)) | m <- mk | k <- kk | n <- [0 ..]]
    values = [m * l / x | m <- mk | l <- lk | x <- xk]
    partials = scanl1 (+) values

rootApproximation :: [Rational]
rootApproximation = map head . iterate (drop 4) $ go 1 0 100 1 40
  where
    go pk' qk' pk qk a = (pk % qk) : go pk qk (pk' + a * pk) (qk' + a * qk) (240 - a)

piApproximations :: [Rational]
piApproximations = [q * 10005^^k * c^^z * r | c <- chudnovsky | r <- rootApproximation]
  where
     q = 1
     z = 1
     k = div z 2

main :: IO ()
main = print $ floor @_ @Integer (10 ^ (1000 :: Int) * piApproximations !! 2000)
