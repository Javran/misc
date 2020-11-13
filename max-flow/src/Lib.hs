module Lib
  ( main
  ) where

import Parser
import Paths_max_flow
import qualified Data.Text.IO as T

main :: IO ()
main = do
  fnData <- getDataFileName "data/simple.dimacs"
  raw <- T.readFile fnData
  print (parseContent raw)
  pure ()
