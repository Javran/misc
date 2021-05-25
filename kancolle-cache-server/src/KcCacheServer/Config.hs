{-# LANGUAGE DeriveGeneric #-}

module KcCacheServer.Config where

import Data.Word
import Dhall

data ServAddr = ServAddr
  { port :: Word16
  , host :: Text
  }
  deriving (Generic)

instance FromDhall ServAddr

data Config = Config
  { local :: ServAddr
  , cacheBase :: FilePath
  }
  deriving (Generic)

instance FromDhall Config
