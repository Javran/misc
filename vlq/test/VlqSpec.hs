{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module VlqSpec where

import Cases
import Control.Monad
import Data.Coerce
import qualified Data.Text as T
import GenCase
import Test.Hspec
import Vlq

spec :: Spec
spec = do
  let mkSpec label testCases testFunc =
        describe label $
          forM_ testCases $
            \( Case
                 { description
                 , inputAndExpected = (input, expected)
                 }
               ) -> do
                specify (T.unpack description) $
                  (coerce testFunc) input `shouldBe` expected
  mkSpec "encode" encodeTests encodes
  mkSpec "decode" decodeTests decodes
  -- TODO: fail when too many bits
  -- TODO: encodes sets top bit correctly
  -- TODO: encodes then decodes is identity
  -- TODO: decodes then encodes is identity
