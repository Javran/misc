{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( main
  )
where

import qualified Algorithms.NaturalSort
import Control.Concurrent.Async
import Control.Exception.Safe
import Control.Monad
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Javran.Gentoo.PackageWatcher.Data.EbuildInfo as Eb
import qualified Javran.Gentoo.PackageWatcher.Data.Package as Pkg
import Javran.Gentoo.PackageWatcher.Fetch (nfFetch)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.IO
import qualified Text.HTML.DOM as Html
import Text.XML
import Text.XML.Cursor

type Version = T.Text

nvidiaDrivers :: Pkg.Package
nvidiaDrivers = "x11-drivers/nvidia-drivers"

watchlist :: [Pkg.Package]
watchlist =
  nvidiaDrivers :
  [ "sys-kernel/gentoo-sources"
  , "media-video/pipewire"
  ]

dischargeExceptionToStderr :: IO (Either SomeException a) -> IO (Maybe a)
dischargeExceptionToStderr action =
  action >>= \case
    Left e ->
      hPutStrLn stderr (displayException e) >> pure Nothing
    Right v -> pure $ Just v

gatherInfoForPackage :: Manager -> Pkg.Package -> IO (Maybe [Eb.EbuildInfo])
gatherInfoForPackage mgr pkg = do
  mVers <- dischargeExceptionToStderr $ nfFetch mgr (show pkg) (versionsFromRaw pkg)
  case mVers of
    Nothing -> pure Nothing
    Just vers ->
      Just
        <$> if pkg == nvidiaDrivers
          then
            mapConcurrently
              (\version -> do
                 mKVer <- dischargeExceptionToStderr $ fetchNvDriverExtra mgr version
                 let extra = fmap (\kv -> toJSON $ HM.singleton ("NV_KERNEL_MAX" :: T.Text) kv) mKVer
                 pure $ Eb.EbuildInfo {Eb.version, Eb.extra})
              vers
          else
            pure $
              fmap (\version -> Eb.EbuildInfo {Eb.version, Eb.extra = Nothing}) vers

gatherAllInfo :: Manager -> [Pkg.Package] -> IO [(Pkg.Package, Maybe [Eb.EbuildInfo])]
gatherAllInfo mgr = mapConcurrently (\pkg -> (pkg,) <$> gatherInfoForPackage mgr pkg)

versionsFromRaw :: Pkg.Package -> BSL.ByteString -> [Version]
versionsFromRaw Pkg.Package {Pkg.name} raw = do
  let doc = Html.parseLBS raw
      extractFromEbuild :: Cursor -> [T.Text]
      extractFromEbuild cur = case node cur of
        NodeElement e
          | [NodeContent c0] <- elementNodes e ->
            maybeToList $
              T.stripPrefix (name <> "-") c0 >>= T.stripSuffix ".ebuild"
        _ -> []
  fromDocument doc
    $// element "ul"
      &/ element "li"
      &/ element "a"
      >=> extractFromEbuild

parseNvKernelMax :: BSL.ByteString -> Maybe Version
parseNvKernelMax raw = listToMaybe do
  l0 <- BSLC.lines raw
  Just l1 <- pure do
    BSLC.stripPrefix "NV_KERNEL_MAX=\"" l0
      >>= BSLC.stripSuffix "\""
  pure (decodeUtf8 $ BSLC.toStrict l1)

fetchNvDriverExtra :: Manager -> T.Text -> IO (Either SomeException Version)
fetchNvDriverExtra mgr ver =
  nfFetch
    mgr
    (show nvidiaDrivers <> "/nvidia-drivers-" <> T.unpack ver <> ".ebuild")
    (fromMaybe (error "parse failure") . parseNvKernelMax)

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
