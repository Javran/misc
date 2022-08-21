{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import qualified Algorithms.NaturalSort
import Control.Concurrent.Async
import Control.Monad
import Data.Aeson.Types
import Data.Char
import qualified Data.Map.Strict as M
import Data.Either
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Javran.Gentoo.PackageWatcher.Data.EbuildInfo as Eb
import qualified Javran.Gentoo.PackageWatcher.Data.Package as Pkg
import Javran.Gentoo.PackageWatcher.Gather
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main =
  getArgs >>= \case
    ["_dev"] -> do
      getLocalPackages >>= print
    [fpWatchlist] -> do
      rawWl <- readFile fpWatchlist
      let (malforms, watchlist) =
            partitionEithers
              . fmap convert
              . filter (not . all isSpace)
              $ lines rawWl
            where
              convert x = case Pkg.safeFromString x of
                Nothing -> Left x
                Just t -> Right t
      unless (null malforms) do
        hPutStrLn stderr "Failed to parse the following:"
        mapM_ (hPutStrLn stderr) malforms
      wLoc <- async getLocalPackages
      mgr <- newManager tlsManagerSettings
      results <- gatherAllInfo mgr watchlist
      localPkgs <- wait wLoc
      forM_ results \(pkg, m) -> do
        putStrLn $ "Package: " <> show pkg
        case localPkgs M.!? pkg of
          Nothing -> hPutStrLn stderr "Warning: local version not found."
          Just lvs -> putStrLn $ "- local: " <> T.unpack (T.intercalate ", " $ sortOn (Data.Ord.Down . Algorithms.NaturalSort.sortKey) lvs)
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
    _ -> die "<prog> <path to watchlist file>"
