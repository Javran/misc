module Lib
  ( main
  ) where

import Parser

main :: IO ()
main = print (calc . lexer $ "let x = 10 in x + x")
