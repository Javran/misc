{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main
  ( main
  ) where

import Control.Exception
import Control.Monad.State.Strict
import Data.Int
import Data.Text.Encoding (encodeUtf8)
import Dhall
import Hasql.Connection
import Hasql.Session
import Hasql.Statement
import PostgreSQL.Binary.Data
import System.Environment
import System.Random.TF
import System.Random.TF.Gen
import System.Random.TF.Instances

import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Data.ByteString as BS
import qualified Data.Text as T

data PsqlConfig
  = PsqlConfig
  { host :: Text
  , port :: Natural
  , user :: Text
  , password :: Text
  , database :: Text
  } deriving (Generic)

instance FromDhall PsqlConfig

testStatement :: Statement Int64 [Int64]
testStatement = Statement sql encoder decoder True
  where
    sql = "SELECT ((raw -> 'time') :: bigint) AS t FROM poi_recordsb ORDER BY t LIMIT $1"
    encoder = Encoders.param (Encoders.nonNullable Encoders.int8)
    decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))

data TestRow
  = TestRow
  { trTime :: LocalTime
  , trV :: Int64
  , trS :: Text
  , trJ :: TestJson
  } deriving Show

data TestJson
  = TestJson
  { tjLength :: Int
  , tjNums :: [Int]
  , tjMeta :: Text
  } deriving Show

type M = StateT TFGen IO

genChar :: M Char
genChar = (xs !!) <$> state (randomR (0, length xs - 1))
  where
    xs = ['a'..'z'] <> ['A' .. 'Z'] <> ['0'..'9'] <> "_+=!?"

genText :: (Int, Int) -> M Text
genText range = do
    l <- state (randomR range)
    T.pack <$> replicateM l genChar

genTestJson :: M TestJson
genTestJson = do
  l <- state (randomR (0, 5))
  xs <- replicateM l (state (randomR (-10000,10000)))
  meta <- genText (7,10)
  pure $ TestJson l xs meta

{-
  Create a test table on demand, the schema will look like:

  - time: as primary key (for our use case, it can rarely duplicate)
  - v: a random integer column
  - s: a random string column
  - j: a column holding json AST value
    - {length: <length>, nums: <array of numbers>, meta: <some random string>}
 -}

testTableCreationStatement :: Statement () ()
testTableCreationStatement =
    Statement sql Encoders.noParams Decoders.noResult False
  where
    sql =
      "CREATE TABLE IF NOT EXISTS test_table (\
      \  time timestamp PRIMARY KEY NOT NULL,\
      \  v int8 NOT NULL,\
      \  s text NOT NULL,\
      \  j jsonb NOT NULL\
      \)"

main :: IO ()
main = do
  [configPath] <- getArgs
  PsqlConfig hst pt u pw db <- inputFile auto configPath
  let sqlSettings =
        settings
          (encodeUtf8 hst)
          (fromIntegral pt)
          (encodeUtf8 u)
          (encodeUtf8 pw)
          (encodeUtf8 db)
  mConn <- acquire sqlSettings
  case mConn of
    Left e -> do
      putStrLn "error while connecting to database."
      print e
    Right conn -> do
      putStrLn "connection acquired successfully."
      g <- newTFGen
      xs <- evalStateT (replicateM 6 genTestJson) g
      mapM_ print xs
      -- main logic after connection goes here.
      let sess = statement () testTableCreationStatement
      mResult <- run sess conn
      case mResult of
        Left qe -> do
          putStrLn "query error"
          print qe
        Right rs -> print rs
      putStrLn "releasing connection ..."
      release conn
