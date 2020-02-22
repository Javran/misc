{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main
  ( main
  ) where

import Data.Int
import Control.Exception
import Dhall
import Hasql.Connection
import Hasql.Statement
import Hasql.Session
import System.Environment
import Data.Text.Encoding (encodeUtf8)

import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Data.ByteString as BS

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
    sql = "SELECT raw -> 'time' from poi_recordsb LIMIT $1"
    encoder = Encoders.param (Encoders.nonNullable Encoders.int8)
    decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))

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
      -- main logic after connection goes here.
      let sess = statement 20 testStatement
      mResult <- run sess conn
      case mResult of
        Left qe -> do
          putStrLn "query error"
          print qe
        Right rs -> print rs
      putStrLn "releasing connection ..."
      release conn
