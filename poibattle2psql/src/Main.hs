module Main
  ( main
  ) where

import Dhall
import System.Environment
import System.Exit

import Config

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configPath] -> do
      pConf@ProgConfig
        { pcSqlConfig = sqlConfig
        } <- inputFile auto configPath
      pure ()
    _ -> do
      putStrLn "pb2psql <config.dhall>"
      exitFailure
