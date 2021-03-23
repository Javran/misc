module Lib
  ( main
  ) where

import Lexer
import Parser

main :: IO ()
main = print (($ []) . calc . lexer $ "let x = 10 in (let z = 7 in z * z) + x")
