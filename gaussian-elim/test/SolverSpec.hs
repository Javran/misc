{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module SolverSpec where

import Control.Monad
import Data.Int
import Math.NumberTheory.Primes
import Solver
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

smallPrimes :: [Integer]
smallPrimes = takeWhile (< 10000) $ fmap unPrime primes

spec :: Spec
spec = do
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
  describe "multInv" $ do
    prop "props" $ do
      m <- choose (1, 0xFFFFFF)
      n <- choose (1, 0xFFFFFF)
      let r = multInv @Integer m n
      pure $
        either
          (label "no inv" . (=== n))
          (\n' ->
             label "has inv" $
               n' >= 0 .&&. n' < m .&&. (n' * n) `mod` m === 1)
          r
  describe "solveMat" $ do
    specify "example" $
      solveMat @Int
        17
        [ [3, 4, 7, 2]
        , [4, 11, 2, 8]
        , [16, 7, 3, 3]
        ]
        `shouldBe` Right [4, 15, 7]
    prop "correctness" $ do
      sz <- choose @Int (3, 20)
      let rndIntVal = choose @Int64 (-0xFF_FFFF, 0xFF_FFFF)
      mPre <- elements smallPrimes
      let m = fromInteger mPre
      mat <- replicateM sz $ replicateM (sz + 1) rndIntVal
      let result = solveMat m mat
          lbl = case result of
            Right _ -> "right"
            Left v ->
              "left: " <> case v of
                NoMultInv _ -> "no mult inv"
                Underdetermined -> "underdet"
      pure $
        label lbl $
          case result of
            Right xs ->
              counterexample (show (m, mat)) $
                conjoin $ do
                  eqn <- mat
                  let lhs = init eqn
                      rhs = last eqn
                  pure $
                    length xs === length lhs
                      .&&. counterexample (show ((lhs, xs), rhs)) (
                              sum (zipWith (*) lhs xs) `mod` m === (rhs `mod` m))
            Left _ -> property True
  describe "shuffler" $ do
    specify "example" $ do
      let (f, g) = shuffler 2
      f "ABCD" `shouldBe` "CABD"
      g "CABD" `shouldBe` "ABCD"
    prop "correctness" $ \(xs :: String) ->
      conjoin $ do
        (i,e) <- zip [0..] xs
        let (f,g) = shuffler i
            ys = f xs
        pure $ xs === g ys .&&. head ys === e
