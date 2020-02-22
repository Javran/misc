{-# LANGUAGE DeriveGeneric #-}
module Main
  ( main
  ) where

import Data.Word
import Control.Exception
import Dhall
import Hasql.Connection
import System.Environment
import Data.Text.Encoding (encodeUtf8)

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
      putStrLn "releasing connection ..."
      release conn
