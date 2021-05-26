{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications, OverloadedStrings #-}

module KcCacheServer.Main where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Text as T
import Dhall
import qualified KcCacheServer.CacheMeta as R
import qualified KcCacheServer.Config as Config
import Network.Wai.Handler.Warp
import Network.Wai
import System.Environment
import System.Exit
import System.FilePath.Posix
import Network.HTTP.Types

app :: Application
app req respond = respond $ responseLBS status200 [] "foo"

main :: IO ()
main =
  getArgs >>= \case
    configFp : _extraArgs -> do
      cfg@Config.Config {Config.cacheBase} <- inputFile @Config.Config auto configFp
      r <-
        eitherDecodeFileStrict
          @(HM.HashMap T.Text R.ResourceMeta)
          (cacheBase </> "cached.json")
      case r of
        Left err -> error err
        Right parsed -> do
          let _s = S.fromList . fmap R.lastModified $ HM.elems parsed
          -- print s
          run (fromIntegral $ Config.port $ Config.local cfg) app
          pure ()
    _ -> do
      putStrLn "<prog> <server config> [args...]"
      exitFailure
