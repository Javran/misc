{-# LANGUAGE ParallelListComp #-}

module Lib (
  main,
) where

import Data.Ratio

-- most of the following originally from exact-pi

type R = (Integer, Integer)

addR :: R -> R -> R
addR (a, b) (c, d) = if b == d then (a + c, b) else (a * d + b * c, b * d)

mulR :: R -> R -> R
mulR (a, b) (c, d) = (a * c, b * d)

divR :: R -> R -> R
divR (a, b) (c, d) = (a * d, b * c)

chudnovsky :: [R]
chudnovsky = [divR (426880, 1) s | s <- partials]
  where
    lk :: [Integer]
    lk = iterate (+ 545140134) 13591409
    xk = iterate (mulR ((-262537412640768000, 1))) (1, 1)
    kk :: [Integer]
    kk = iterate (+ 12) 6
    mk :: [R]
    mk = (1, 1) : [mulR m ((k ^ (3 :: Int) - 16 * k), (n + 1) ^ (3 :: Int)) | m <- mk | k <- kk | n <- [0 ..]]
    values = [divR (mulR m (l, 1)) x | m <- mk | l <- lk | x <- xk]
    partials = scanl1 addR values

rootApproximation :: [R]
rootApproximation = map head . iterate (drop 4) $ go 1 0 100 1 40
  where
    go pk' qk' pk qk a = (pk, qk) : go pk qk (pk' + a * pk) (qk' + a * qk) (240 - a)

rToRational :: (Integer, Integer) -> Rational
rToRational = uncurry (%)

piApproximations :: [Rational]
piApproximations = [q * 10005 ^^ k * rToRational c ^^ z * rToRational r | c <- chudnovsky | r <- rootApproximation]
  where
    q = 1
    z = 1
    k = div z 2

main :: IO ()
main = print $ floor @_ @Integer (10 ^ (100 :: Int) * piApproximations !! 200)

-- 31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679
