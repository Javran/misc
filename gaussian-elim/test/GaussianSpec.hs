{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module GaussianSpec where

import Control.Monad
import Gaussian
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "solveMat" $
  prop "correctness" $ do
    sz <- choose @Int (3, 20)
    let rndIntVal = choose @Integer (-0xFF_FFFF, 0xFF_FFFF)
    mat <-
      (fmap . fmap) fromInteger <$> do
        matLhs <- replicateM sz $ replicateM sz rndIntVal
        assns <- replicateM sz rndIntVal
        let matRhs :: [Integer]
            matRhs = fmap (sum . zipWith (*) assns) matLhs
        pure $ zipWith (\lhs rhs -> lhs <> [rhs]) matLhs matRhs
    let result = solveMat (upperTriangular mat) :: [Rational]
        isUnderDet = length result /= sz
        lbl = if isUnderDet then "underdetermined" else "unique"
    pure $
      label lbl $
        isUnderDet
          .||. conjoin (do
                  eqn <- mat
                  let lhs = init eqn
                      rhs = last eqn
                  pure $ sum (zipWith (*) lhs result) === rhs)
