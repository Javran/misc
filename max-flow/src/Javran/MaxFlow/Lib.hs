{-# LANGUAGE NamedFieldPuns #-}

module Javran.MaxFlow.Lib
  ( main
  )
where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import Javran.MaxFlow.EdmondsKarp
import Javran.MaxFlow.Parser
import Javran.MaxFlow.Verify
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix

batchProcess :: FilePath -> IO ()
batchProcess pBase = do
  de <- doesDirectoryExist pBase
  if de
    then
      listDirectory pBase
        >>= mapM_ (batchProcess . (pBase </>))
    else do
      let fName = pBase
      putStr $ pBase <> ": "
      raw <- T.readFile fName
      case parseFromRaw raw of
        Left msg -> do
          putStrLn $ "parse error: " <> msg
        Right nr -> do
          let (result, _logs) = maxFlow nr
          case result of
            Left msg -> do
              putStrLn $ "error: " <> msg
            Right (v, _arcs, _) -> do
              putStrLn $ "max flow: " <> show v

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["dev", fName] -> do
      raw <- T.readFile fName
      case parseFromRaw raw of
        Left msg -> do
          putStrLn $ "parse error: " <> msg
          exitFailure
        Right nrRaw -> do
          let nr@NetworkRep {nrSource, nrSink} = normalize nrRaw
          print nr
          let (result, logs) = maxFlow nr
          mapM_ T.putStrLn logs
          case result of
            Left msg -> do
              putStrLn $ "error: " <> msg
              exitFailure
            Right (v, arcs, cMap) -> do
              putStrLn $ "max flow: " <> show v
              putStrLn $
                "non zero assignments: "
                  <> show
                    (filter ((/= 0) . snd) $ M.toList arcs)
              putStrLn $
                "verification: "
                  <> (show $ runExcept $ verify nrSource nrSink cMap arcs)
    ["batch", basePath] -> do
      p <- doesPathExist basePath
      if p
        then batchProcess basePath
        else do
          putStrLn "path does not exist"
          exitFailure
    _ -> do
      putStrLn "<prog> dev <data file>: run on a single dimacs file."
      putStrLn "<prog> batch <base path>: batch-process files."
      exitFailure
