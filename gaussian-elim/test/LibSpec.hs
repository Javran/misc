{-# LANGUAGE TypeApplications #-}

module LibSpec where

import Lib
import Math.NumberTheory.Primes
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

smallPrimes :: [Integer]
smallPrimes = takeWhile (< 10000) $ fmap unPrime primes

spec :: Spec
spec =
  describe "extEuclidean" $ do
    specify "example" $
      extEuclidean @Integer 1234 4147 `shouldBe` (1, (-1314, 391))
    prop "props" $ do
      x <- choose (1, 0xFFFFFF)
      y <- choose (1, 0xFFFFFF)
      let (t, (u, v)) = extEuclidean @Integer x y
          gcdResult = gcd x y
          lbl = if gcdResult == 1 then "coprimes" else "not coprimes"
      pure $
        label lbl $
          t === gcd x y
            .&&. (gcd x y =/= 1
                    .||. u * x + v * y === 1)
