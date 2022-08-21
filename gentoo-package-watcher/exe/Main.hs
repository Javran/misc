{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Algorithms.NaturalSort
import Control.Monad
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Javran.Gentoo.PackageWatcher.Data.EbuildInfo as Eb
import qualified Javran.Gentoo.PackageWatcher.Data.Package as Pkg
import Javran.Gentoo.PackageWatcher.Gather
import Network.HTTP.Client
import Network.HTTP.Client.TLS

watchlist :: [Pkg.Package]
watchlist =
  nvidiaDrivers :
  [ "sys-kernel/gentoo-sources"
  , "media-video/pipewire"
  ]

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  results <- gatherAllInfo mgr watchlist
  forM_ results \(pkg, m) -> do
    putStrLn $ "Package: " <> show pkg
    case m of
      Nothing -> putStrLn "  <Fetch error>"
      Just ebsPre ->
        let ebs = sortOn (Data.Ord.Down . Algorithms.NaturalSort.sortKey . Eb.version) ebsPre
         in forM_ ebs \Eb.EbuildInfo {Eb.version, Eb.extra} -> do
              putStrLn $
                "- " <> T.unpack version <> case extra of
                  Nothing -> ""
                  Just ~(Object v) ->
                    let String kv = v HM.! "NV_KERNEL_MAX"
                     in ", kernel max: " <> T.unpack kv
