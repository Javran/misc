module Lib
  ( main
  )
where

import CPython.Simple

main :: IO ()
main = do
  initialize
  pure ()
