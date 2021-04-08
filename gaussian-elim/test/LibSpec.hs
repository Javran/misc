{-# LANGUAGE TypeApplications #-}
module LibSpec where

import Test.Hspec
import Test.QuickCheck
import Lib

spec :: Spec
spec =
  describe "extEuclidean" $ do
    specify "example" $
      extEuclidean @Integer 1234 4147 `shouldBe` (1, (-1314, 391))

