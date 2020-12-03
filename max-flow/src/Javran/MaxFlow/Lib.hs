{-# LANGUAGE NamedFieldPuns #-}

module Javran.MaxFlow.Lib
  ( main
  )
where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import Javran.MaxFlow.Common
import Javran.MaxFlow.EdmondsKarp
import Javran.MaxFlow.Parser
import Javran.MaxFlow.TestData
import Javran.MaxFlow.Verify
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import qualified Javran.MaxFlow.Dinitz as Dinitz

batchProcess :: FilePath -> IO ()
batchProcess pBase = do
  de <- doesDirectoryExist pBase
  if de
    then
      listDirectory pBase
        >>= mapM_ (batchProcess . (pBase </>))
    else when (takeExtension pBase == ".dimacs") $
      do
        let fName = pBase
        putStr $ pBase <> ": "
        raw <- T.readFile fName
        case parseFromRaw raw of
          Left msg -> do
            putStrLn $ "parse error: " <> msg
          Right nr -> do
            let (result, _logs) = maxFlow (normalize nr)
            case result of
              Left msg -> do
                putStrLn $ "error: " <> msg
              Right (v, _arcs, _) -> do
                putStrLn $ "max flow: " <> show v

-- load, parse, and normalize a network from FilePath.
loadNetwork :: FilePath -> IO NormalizedNetwork
loadNetwork fName = do
  raw <- T.readFile fName
  case parseFromRaw raw of
    Left msg -> do
      putStrLn $ "parse error: " <> msg
      exitFailure
    Right nrRaw -> do
      pure $ normalize nrRaw

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["dev", fName] -> do
      nn <- loadNetwork fName
      let nr@NetworkRep {nrSource, nrSink} = getNR nn
      print (Dinitz.experiment nn)
    ["run", fName] -> do
      nn <- loadNetwork fName
      let nr@NetworkRep {nrSource, nrSink} = getNR nn
      print nr
      let (result, logs) = maxFlow nn
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
    ["embed"] -> do
      print (fst <$> (packSimple <> packGenetic <> packHandmade <> packRandomBest))
    _ -> do
      putStrLn "<prog> run <data file>: run on a single dimacs file."
      putStrLn "<prog> batch <base path>: batch-process files."
      exitFailure
