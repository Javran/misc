{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module KcCacheServer.HandlerImpl where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified KcCacheServer.CacheMeta as CM
import KcCacheServer.Caching
import qualified KcCacheServer.Config as Cfg
import KcCacheServer.RequestHandler
  ( KcRequest (..)
  , KcResponse (..)
  )
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import System.FilePath.Posix

{-
  To perform a network request in case of cache miss / invalidation
 -}
networkRequest :: (MonadIO m, MonadLogger m) => Cfg.Config -> Manager -> KcRequest -> m KcResponse
networkRequest Cfg.Config {Cfg.remoteHost} mgr KcRequest {reqPath, reqVersion} = do
  initReq <- liftIO $ parseRequest (T.unpack remoteHost </> T.unpack (T.dropWhile (== '/') reqPath))
  let req =
        maybe
          id
          (\v -> setQueryString [("version", Just $ encodeUtf8 v)])
          reqVersion
          initReq
  resp <- liftIO $ httpLbs req mgr
  when (responseStatus resp /= ok200) $
    error $ "unexpected status: " <> show (responseStatus resp)
  let respBody = responseBody resp
      respHeaders = responseHeaders resp
  respMeta <- do
    lm <- case lookup hLastModified respHeaders of
      Nothing -> do
        $(logWarn) "Last-Modified not available, using empty string."
        pure ""
      Just v -> pure (decodeUtf8 v)
    rLength <- case lookup hContentLength respHeaders of
      Just v | [(l, "")] <- reads (T.unpack $ decodeUtf8 v) -> pure l
      _ -> do
        $(logWarn) "Content-Length missing or malformed, computing from body."
        pure $ fromIntegral $ BSL.length (responseBody resp)
    pure
      CM.ResourceMeta
        { CM.version = reqVersion
        , CM.lastModified = lm
        , CM.rLength
        , CM.cacheControl =
            decodeUtf8
              <$> lookup hCacheControl respHeaders
        }
  $(logInfoSH) respMeta
  pure KcResponse {respBody, respMeta}
