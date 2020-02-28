{-# LANGUAGE LambdaCase #-}
module Main
  ( main
  ) where

import Data.Text.Encoding (encodeUtf8)
import Dhall
import Hasql.Connection
import System.Environment
import System.Exit

import qualified Data.Text as Text

import Config

acquireFromConfig :: PsqlConfig -> IO Connection
acquireFromConfig (PsqlConfig hst pt u pw db) =
    acquire sqlSettings >>= \case
      Left e -> do
        putStrLn "error while connecting to database."
        print e
        exitFailure
      Right conn -> pure conn
  where
    sqlSettings =
      settings
        (encodeUtf8 hst)
        (fromIntegral pt)
        (encodeUtf8 u)
        (encodeUtf8 pw)
        (encodeUtf8 db)

main :: IO ()
main = getArgs >>= \case
  [configPath] -> do
    pConf@ProgConfig
      { pcSqlConfig = sqlConfig
      } <- inputFile auto configPath
    conn <- acquireFromConfig sqlConfig
    putStrLn "connection acquired successfully."
    putStrLn "releasing connection ..."
    release conn
  _ -> do
    putStrLn "pb2psql <config.dhall>"
    exitFailure
