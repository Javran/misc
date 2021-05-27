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
  , -- | 'http://xxx.yyy.zzz.www/' (whether final '/' exists should not be significant)
    remoteHost :: Text
  }
  deriving (Generic)

instance FromDhall Config
