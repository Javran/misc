module Lib
  ( main
  ) where

import Alex

-- TODO: project idea: a super verbose tokenizer just to see what can be carried around?

main :: IO ()
main = print (alexScanTokens "let stuff = a b c in stuff 123")
