{-# LANGUAGE LambdaCase #-}
module Main
  ( main
  ) where

import Data.Text.Encoding (encodeUtf8)
import Dhall
import Hasql.Connection
import Hasql.Session
import System.Environment
import System.Exit

import qualified Data.Text as Text

import Config
import RecordScanner

import qualified Statement

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

{-
  TODO: workflow:

  - scan and collect filenames from poi battle directory.
  - query database to see which of them are new records.
  - insert records into the database.

 -}

main :: IO ()
main = getArgs >>= \case
  [configPath] -> do
    pConf@ProgConfig
      { pcSqlConfig = sqlConfig
      , pcBattleDataPath = fp
      } <- inputFile auto configPath
    -- fetch battle records
    getBattleRecordIds fp >>= print . take 10
    conn <- acquireFromConfig sqlConfig
    putStrLn "connection acquired successfully."
    -- create the table
    let sess = statement () $ Statement.createTable (pcTableName pConf)
    run sess conn >>= \case
      Left qe -> do
        putStrLn "query error"
        print qe
      Right rs -> print rs
    putStrLn "releasing connection ..."
    release conn
  _ -> do
    putStrLn "pb2psql <config.dhall>"
    exitFailure
