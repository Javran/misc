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

type Config = Vector ServerInfo
