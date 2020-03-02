{-# LANGUAGE LambdaCase #-}
module Main
  ( main
  ) where

import Control.Monad
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
    records <- getBattleRecordIds fp
    conn <- acquireFromConfig sqlConfig
    putStrLn "connection acquired successfully."
    -- create the table
    do
      let sess = statement () $ Statement.createTable (pcTableName pConf)
      run sess conn >>= \case
        Left qe -> do
          putStrLn "query error"
          print qe
        Right _ -> pure ()
    do
      putStrLn $ "record count: " <> show (length records)
      let sess = statement (fst <$> records) $ Statement.queryMissingRecords (pcTableName pConf)
      run sess conn >>= \case
        Left qe -> do
          putStrLn "query error"
          print qe
        Right rsPre -> do
          putStrLn $ "missing records count: " <> show (length rsPre)
          -- importing all at once sounds like a terrible idea for testing,
          -- so instead let's just import a small bit and ramp it up if all goes well.
          let (rs, dropped) = splitAt 128 rsPre
          unless (null dropped) $
            putStrLn $ "keeping only first " <> show (length rs) <> " records."
    putStrLn "releasing connection ..."
    release conn
  _ -> do
    putStrLn "pb2psql <config.dhall>"
    exitFailure
