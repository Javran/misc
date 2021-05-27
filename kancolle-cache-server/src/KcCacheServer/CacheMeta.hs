{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module KcCacheServer.CacheMeta where

import Data.Aeson
import qualified Data.Text as T

{-

  Reference:  https://github.com/Tibowl/KCCacheProxy/blob/5d6180371902f803116123758eb8293d1ade0782/src/proxy/cacher.js#L206-L211

  - 'lastmodified': 'Last-Modified' header
  - 'cache': 'Cache-Control' header
  - 'rLength': content length

 -}

data ResourceMeta = ResourceMeta
  { version :: Maybe T.Text
  , lastModified :: T.Text
  , rLength :: Int
  , cacheControl :: Maybe T.Text
  }
  deriving (Show)

instance FromJSON ResourceMeta where
  parseJSON = withObject "ResourceMeta" $ \o -> do
    version' <- o .: "version"
    let version = if T.null version' then Nothing else Just version'
    lastModified <- o .: "lastmodified"
    rLength <- o .: "length"
    cacheControl <- o .: "cache"
    pure $ ResourceMeta {version, lastModified, rLength, cacheControl}
