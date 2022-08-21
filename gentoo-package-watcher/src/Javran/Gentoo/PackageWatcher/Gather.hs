{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Javran.Gentoo.PackageWatcher.Gather
  ( nvidiaDrivers
  , gatherInfoForPackage
  , gatherAllInfo
  )
where

import Control.Concurrent.Async
import Control.Exception.Safe
import Control.Monad
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Javran.Gentoo.PackageWatcher.Data.EbuildInfo as Eb
import qualified Javran.Gentoo.PackageWatcher.Data.Package as Pkg
import Javran.Gentoo.PackageWatcher.Fetch (nfFetch)
import Javran.Gentoo.PackageWatcher.Types
import Network.HTTP.Client
import System.IO
import qualified Text.HTML.DOM as Html
import Text.XML
import Text.XML.Cursor

nvidiaDrivers :: Pkg.Package
nvidiaDrivers = "x11-drivers/nvidia-drivers"

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
                 let extra =
                       fmap
                         (\kv -> toJSON $ HM.singleton ("NV_KERNEL_MAX" :: T.Text) kv)
                         mKVer
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
