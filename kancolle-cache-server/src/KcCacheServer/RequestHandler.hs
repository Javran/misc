module KcCacheServer.RequestHandler where

import Control.Concurrent.MSem
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified KcCacheServer.CacheMeta as CM

data KcRequest = KcRequest
  { reqPath :: T.Text -- HTTP URI resource (beginning with '/')
  , reqVersion :: Maybe T.Text
  }

data KcResponse = KcResponse
  { respMeta :: CM.ResourceMeta
  , respBody :: BSL.ByteString
  }

{-
  fetchFromCache can return Nothing in case network is forced.
 -}
handleRequest
  :: MonadIO m
  => (KcRequest -> m KcResponse)
  -> (KcRequest -> m (Maybe KcResponse))
  -> (KcRequest -> KcResponse -> m ())
  -> KcRequest
  -> m KcResponse
handleRequest networkRequest fetchFromCache updateCache req = do
  mResp <- fetchFromCache req
  case mResp of
    Just resp -> pure resp
    Nothing -> do
      resp <- networkRequest req
      updateCache req resp
      pure resp
