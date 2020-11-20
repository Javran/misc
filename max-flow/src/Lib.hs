module Lib
  ( main
  )
where

import qualified Data.Text.IO as T
import EdmondsKarp
import Parser
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fName] -> do
      raw <- T.readFile fName
      let Right nr = parseFromRaw raw
      print nr
      let (result, logs) = maxFlow nr
      mapM_ T.putStrLn logs
      print result
    _ -> do
      putStrLn "<prog> <data file>"
      exitFailure
