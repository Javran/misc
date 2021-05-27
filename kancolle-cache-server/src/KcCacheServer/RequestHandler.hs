module KcCacheServer.RequestHandler where

import Control.Concurrent.MSem
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified KcCacheServer.CacheMeta as CM
import qualified Data.ByteString.Lazy as BSL

data KcRequest = KcRequest
  { reqPath :: T.Text -- HTTP URI resource (beginning with '/')
  , reqVersion :: Maybe T.Text
  }

data KcResponse = KcResponse
  { respMeta :: CM.ResourceMeta
  , respBody :: BSL.ByteString
  }

handleRequest
  :: MonadIO m
  => (KcRequest -> IO KcResponse)
  -> (KcRequest -> IO (Maybe KcResponse))
  -> (KcRequest -> KcResponse -> IO ())
  -> KcRequest
  -> m KcResponse
handleRequest networkRequest fetchFromCache updateCache req = do
  -- TODO: what if we want to force network?
  mResp <- liftIO $ fetchFromCache req
  case mResp of
    Just resp -> pure resp
    Nothing -> liftIO $ do
      resp <- networkRequest req
      updateCache req resp
      pure resp
