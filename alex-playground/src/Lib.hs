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
main = print $ runAlex "let x = y in t" parseAll
  where
    parseAll = alexMonadScan >>= \case
      EOF -> pure []
      x -> (x:) <$> parseAll
