module Lib
  ( main
  ) where

import Parser
import Paths_max_flow
import qualified Data.Text.IO as T
import EdmondsKarp

main :: IO ()
main = do
  fnData <- getDataFileName "data/simple.dimacs"
  raw <- T.readFile fnData
  let Right nr = parseFromRaw raw
  print nr
  let (result, logs) = maxFlow nr
  print result
  mapM_ T.putStrLn logs
  pure ()
