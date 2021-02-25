{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( main
  ) where

import Alex
import Token
import Control.Monad

-- TODO: project idea: a super verbose tokenizer just to see what can be carried around?

main :: IO ()
main = print $ runAlex "let xzzz = 1234 in ts" parseAll
  where
    parseAll = alexMonadScan >>= \case
      EOF -> pure []
      x -> (x:) <$> parseAll
