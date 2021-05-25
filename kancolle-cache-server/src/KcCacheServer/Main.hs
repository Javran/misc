{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module KcCacheServer.Main where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Text as T
import Dhall
import qualified KcCacheServer.CacheMeta as R
import qualified KcCacheServer.Config as Config
import System.Environment
import System.Exit
import System.FilePath.Posix

main :: IO ()
main =
  getArgs >>= \case
    configFp : _extraArgs -> do
      _cfg@Config.Config {Config.cacheBase} <- inputFile @Config.Config auto configFp
      r <- eitherDecodeFileStrict @(HM.HashMap T.Text R.ResourceMeta) (cacheBase </> "cached.json")
      case r of
        Left err -> error err
        Right parsed -> do
          let s = S.fromList . fmap R.version $ HM.elems parsed
          print s
          pure ()
    _ -> do
      putStrLn "<prog> <server config> [args...]"
      exitFailure
