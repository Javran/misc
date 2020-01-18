module Main
  ( main
  ) where

import System.Environment

{-
  For packing poi battle-detail records into one.

  WIP:
  - read a directory and find all ".json.gz" files.
  - put them all into one file, with records merged together
    separated by newline. (the output file)
  - then we can use whatever tool we like to compress that to a smaller one.

 -}

main :: IO ()
main = pure ()
