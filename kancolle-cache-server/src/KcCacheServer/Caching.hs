{-# LANGUAGE NamedFieldPuns #-}
module KcCacheServer.Caching where

import Control.Concurrent.MSem
import Control.Concurrent.MVar
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef
import qualified Data.Text as T
import qualified KcCacheServer.CacheMeta as CM
import Network.HTTP.Types.Status
import System.FilePath.Posix
import KcCacheServer.RequestHandler

-- TODO: handle max-in-flight requests and de-dup.
-- TODO: handle cache verfication and invalidation
data CacheContext = CacheContext
  { ccStore :: MVar (HM.HashMap T.Text CM.ResourceMeta)
  , ccBaseDir :: FilePath
  , ccSem :: MSem Int
  , ccNetworkInFlight :: MVar (HS.HashSet T.Text)
  }

fetchFromCache :: CacheContext -> KcRequest -> IO (Maybe KcResponse)
fetchFromCache cc req = do
  let path = reqPath req
  st <- readMVar (ccStore cc)
  case HM.lookup path st of
    Nothing -> pure Nothing
    Just respMeta -> do
      -- TODO: check whether file exists.
      let fp = ccBaseDir cc </> T.unpack (T.dropWhile (== '/') path)
      respBody <- BSL.readFile fp
      pure $ Just $ KcResponse {respMeta,respBody}

updateCache :: CacheContext -> KcRequest -> KcResponse -> IO ()
updateCache cc req resp =
  -- TODO
  pure ()

{-
-- https://stackoverflow.com/a/45485526/315302
responseBody :: KcResponse -> IO BSL.ByteString
responseBody res =
  let (status, headers, body) = Wai.responseToStream res
   in body $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<> chunk)) (return ())
        toLazyByteString <$> readIORef content
 -}
