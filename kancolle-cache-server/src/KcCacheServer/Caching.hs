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
import qualified Network.Wai as Wai
import System.FilePath.Posix

-- TODO: handle max-in-flight requests and de-dup.
-- TODO: handle cache verfication and invalidation
data CacheContext = CacheContext
  { ccStore :: MVar (HM.HashMap T.Text CM.ResourceMeta)
  , ccBaseDir :: FilePath
  , ccSem :: MSem Int
  , ccNetworkInFlight :: MVar (HS.HashSet T.Text)
  }

mkPath :: [T.Text] -> T.Text
mkPath = T.concat . fmap (T.cons '/')

fetchFromCache :: CacheContext -> Wai.Request -> IO (Maybe Wai.Response)
fetchFromCache cc req = do
  let path = mkPath (Wai.pathInfo req)
  st <- readMVar (ccStore cc)
  case HM.lookup path st of
    Nothing -> pure Nothing
    Just _meta -> do
      -- TODO: respect meta
      content <- BSL.readFile (ccBaseDir cc </> T.unpack path)
      pure $ Just $ Wai.responseLBS status200 [] content

updateCache :: CacheContext -> Wai.Request -> Wai.Response -> IO ()
updateCache cc req resp =
  pure ()

-- https://stackoverflow.com/a/45485526/315302
responseBody :: Wai.Response -> IO BSL.ByteString
responseBody res =
  let (status, headers, body) = Wai.responseToStream res
   in body $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<> chunk)) (return ())
        toLazyByteString <$> readIORef content
