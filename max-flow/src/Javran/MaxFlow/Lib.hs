{-# LANGUAGE NamedFieldPuns #-}

module Javran.MaxFlow.Lib
  ( main
  )
where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import Javran.MaxFlow.Common
import qualified Javran.MaxFlow.Dinitz as Dinitz
import qualified Javran.MaxFlow.EdmondsKarp as EdmondsKarp
import Javran.MaxFlow.Parser
import Javran.MaxFlow.TestData
import Javran.MaxFlow.Verify
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix

visitDimacsFiles :: (NetworkRep -> IO ()) -> FilePath -> IO ()
visitDimacsFiles visit pBase = do
  de <- doesDirectoryExist pBase
  if de
    then
      listDirectory pBase
        >>= mapM_ (visitDimacsFiles visit . (pBase </>))
    else when (takeExtension pBase == ".dimacs") $
      do
        let fName = pBase
        putStr $ pBase <> ": "
        raw <- T.readFile fName
        case parseFromRaw raw of
          Left msg -> do
            putStrLn $ "parse error: " <> msg
          Right nr -> do
            visit nr

batchProcess :: FilePath -> IO ()
batchProcess pBase =
  visitDimacsFiles
    (\nr -> do
       let (result, _logs) = EdmondsKarp.maxFlow (normalize nr)
       case result of
         Left msg -> do
           putStrLn $ "error: " <> msg
         Right (v, _arcs, _) -> do
           putStrLn $ "max flow: " <> show v)
    pBase

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
    ["dev", basePath] -> do
      let visit nrPre = do
            let nn = normalize nrPre
                NetworkRep {nrSource, nrSink} = getNR nn
                {-
                  Assuming that this will always succeed,
                  we will only examine whether algorithms agree on the max flow value
                  rather than comparing the flow itself - since algorithms are free to
                  assign flows as long as the maximum is reached.
                 -}
                (Right (_, fl0, cm0), _) = EdmondsKarp.maxFlow nn
                (Right (_, fl1, cm1), _) = Dinitz.maxFlow nn
                Right r0 = verify nrSource nrSink cm0 fl0
                Right r1 = verify nrSource nrSink cm1 fl1
            if r0 == r1
              then
                putStrLn "agree"
              else
                print (r0, r1)
      p <- doesPathExist basePath
      if p
        then visitDimacsFiles visit basePath
        else do
          putStrLn "path does not exist"
          exitFailure
    ["run", fName] -> do
      nn <- loadNetwork fName
      let nr@NetworkRep {nrSource, nrSink} = getNR nn
      print nr
      let (result, logs) = EdmondsKarp.maxFlow nn
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
              <> (show $ verify nrSource nrSink cMap arcs)
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
