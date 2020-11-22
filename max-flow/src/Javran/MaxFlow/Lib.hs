module Javran.MaxFlow.Lib
  ( main
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import Javran.MaxFlow.EdmondsKarp
import Javran.MaxFlow.Parser
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fName] -> do
      raw <- T.readFile fName
      case parseFromRaw raw of
        Left msg -> do
          putStrLn $ "parse error: " <> msg
          exitFailure
        Right nr -> do
          print nr
          let (result, logs) = maxFlow nr
          mapM_ T.putStrLn logs
          case result of
            Left msg -> do
              putStrLn $ "error: " <> msg
              exitFailure
            Right (v, arcs) -> do
              putStrLn $ "max flow: " <> show v
              putStrLn $
                "non zero assignments: "
                  <> show
                    (filter ((/= 0) . snd) $ M.toList arcs)
    _ -> do
      putStrLn "<prog> <data file>"
      exitFailure
