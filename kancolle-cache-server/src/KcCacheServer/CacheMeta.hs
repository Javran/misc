{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module KcCacheServer.CacheMeta where

import Data.Aeson
import qualified Data.Text as T

data ResourceMeta = ResourceMeta
  { version :: Maybe T.Text
  , lastModified :: T.Text
  , rLength :: Int
  , cache :: Maybe T.Text
  }

instance FromJSON ResourceMeta where
  parseJSON = withObject "ResourceMeta" $ \o -> do
    version' <- o .: "version"
    let version = if T.null version' then Nothing else Just version'
    lastModified <- o .: "lastmodified"
    rLength <- o .: "length"
    cache <- o .: "cache"
    pure $ ResourceMeta {version, lastModified, rLength, cache}
