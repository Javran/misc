module KcCacheServer.RequestHandler where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Network.HTTP.Client

handleRequest
  :: MonadIO m
  => (Request -> IO (Response BS.ByteString))
  -> (Request -> IO (Maybe (Response BS.ByteString)))
  -> (Response BS.ByteString -> IO ())
  -> Request
  -> m (Response BS.ByteString)
handleRequest networkRequest fetchFromCache updateCache req = do
  mResp <- liftIO $ fetchFromCache req
  case mResp of
    Just resp -> pure resp
    Nothing -> liftIO $ do
      resp <- networkRequest req
      updateCache resp
      pure resp
