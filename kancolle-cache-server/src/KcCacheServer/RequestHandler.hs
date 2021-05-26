module KcCacheServer.RequestHandler where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified KcCacheServer.CacheMeta as CM
import qualified Network.Wai as Wai
import Control.Concurrent.MSem
import qualified Data.HashSet as HS

handleRequest
  :: MonadIO m
  => (Wai.Request -> IO Wai.Response)
  -> (Wai.Request -> IO (Maybe Wai.Response))
  -> (Wai.Request -> Wai.Response -> IO ())
  -> Wai.Request
  -> m Wai.Response
handleRequest networkRequest fetchFromCache updateCache req = do
  mResp <- liftIO $ fetchFromCache req
  case mResp of
    Just resp -> pure resp
    Nothing -> liftIO $ do
      resp <- networkRequest req
      updateCache req resp
      pure resp
