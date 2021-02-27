{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module VlqSpec where

import Cases
import Control.Monad
import qualified Data.Bits as Bits
import Data.Coerce
import qualified Data.Text as T
import Data.Word
import GenCase
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Vlq

spec :: Spec
spec = do
  let encodes' :: PprHex Word32 -> PprHex Word8
      encodes' = coerce encodes
      decodes' :: PprHex Word8 -> Either DecodeError (PprHex Word32)
      decodes' = coerce decodes
      mkCases testCases testFunc =
        forM_ testCases $
          \( Case
               { description
               , inputAndExpected = (input, expected)
               }
             ) -> do
              specify (T.unpack description) $
                testFunc input `shouldBe` expected
  describe "encodes" $ do
    prop "encoded result is non-empty" $
      \x -> not (null (encodes' [x]))
    prop "top bits are set correctly" $
      \x ->
        let PprHex encoded = encodes' [x]
            mask = 0b1000_0000
         in all ((/= 0) . (Bits..&. mask)) (init encoded)
              .&&. ((last encoded Bits..&. mask) == 0)
    mkCases encodeTests encodes'

  describe "decodes" $ do
    mkCases decodeTests decodes'
    prop "octet length of 5 but with too many bits" $ do
      lo <- choose (0, 0b0111_1111)
      mids <- replicateM 3 $ choose (0b1000_0000, 0b1111_1111)
      hi <- choose (0b1001_1111, 0b1111_1111)
      pure $ decodes' (PprHex ([hi] <> mids <> [lo])) === Left TooManyBits

  describe "properties" $ do
    prop "decodes . encodes ~ id" $
      forAll
        (PprHex <$> listOf (choose (minBound, maxBound)))
        (\xs -> (decodes' . encodes') xs === Right xs)
