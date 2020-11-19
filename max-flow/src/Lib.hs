module Lib
  ( main
  )
where

import qualified Data.Text.IO as T
import EdmondsKarp
import Parser
import Paths_max_flow

main :: IO ()
main = do
  fnData <-
    getDataFileName
      -- "data/simple.dimacs"
      "data/wash-t10.dimacs.txt"
  raw <- T.readFile fnData
  let Right nr = parseFromRaw raw
  print nr
  let (result, logs) = maxFlow nr
  mapM_ T.putStrLn logs
  print result
