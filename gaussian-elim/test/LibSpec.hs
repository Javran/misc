{-# LANGUAGE TypeApplications #-}

module LibSpec where

import Lib
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Gen

spec :: Spec
spec =
  describe "extEuclidean" $ do
    specify "example" $
      extEuclidean @Integer 1234 4147 `shouldBe` (1, (-1314, 391))
    prop "props" $ do
      x <- choose (1, 0xFFFFFF)
      y <- choose (1, 0xFFFFFF)
      let (t, (u, v)) = extEuclidean @Integer x y
      pure $
        t === gcd x y
          .&&. (gcd x y =/= 1
                  .||. u * x + v * y === 1)
