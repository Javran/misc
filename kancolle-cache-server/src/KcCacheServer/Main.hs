{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module KcCacheServer.Main where

import Control.Concurrent.MSem as MSem
import Control.Concurrent.MVar
import Control.Monad.Logger
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Text as T
import Dhall hiding (newManager)
import qualified KcCacheServer.CacheMeta as R
import KcCacheServer.Caching
import qualified KcCacheServer.Config as Config
import KcCacheServer.HandlerImpl
import KcCacheServer.RequestHandler
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import System.Environment
import System.Exit
import System.FilePath.Posix
import qualified Data.ByteString.Lazy as BSL
main :: IO ()
main =
  getArgs >>= \case
    configFp : _extraArgs -> do
      cfg@Config.Config {Config.cacheBase} <- inputFile @Config.Config auto configFp
      mgr <- newManager tlsManagerSettings
      r <-
        eitherDecodeFileStrict
          @(HM.HashMap T.Text R.ResourceMeta)
          (cacheBase </> "cached.json")
      case r of
        Left err -> error err
        Right parsed -> do
          ccStore <- newMVar parsed
          ccNetworkInFlight <- newMVar mempty
          ccSem <- MSem.new 32
          -- /img/title/02.png?version=5.0.0.0
          let cc =
                CacheContext
                  { ccStore
                  , ccNetworkInFlight
                  , ccSem
                  , ccBaseDir = cacheBase
                  }
              req = KcRequest {reqPath = "/kcs2/img/title/02.png", reqVersion = Just "?version=5.0.0.0"}
          resp <-
            runStderrLoggingT $
              handleRequest
                (networkRequest cfg mgr)
                (fetchFromCache cc)
                (updateCache cc)
                req
          print (BSL.length $ respBody resp)
    _ -> do
      putStrLn "<prog> <server config> [args...]"
      exitFailure
