{-# LANGUAGE DeriveGeneric #-}
module Config where

import Dhall

data ServerInfo
  = ServerInfo
    { id :: Natural
    , name :: Text
    , address :: Text
    } deriving (Show, Generic)

instance FromDhall ServerInfo

data Config
  = Config
  { serverList :: Vector ServerInfo
  , baseOutputPath :: Text
  } deriving (Show, Generic)

instance FromDhall Config
