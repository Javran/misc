{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( main
  )
where

import qualified Algorithms.NaturalSort
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Javran.Gentoo.PackageWatcher.Data.Package as Pkg
import Network.HTTP.Client
import Network.HTTP.Client.TLS
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

fetchPackageDirRaw :: Manager -> Pkg.Package -> IO BSL.ByteString
fetchPackageDirRaw mgr pkg = do
  req <- parseRequest $ "https://gitweb.gentoo.org/repo/gentoo.git/plain/" <> show pkg
  resp <- httpLbs req mgr
  pure $ responseBody resp

versionsFromRaw :: Pkg.Package -> BSL.ByteString -> [Version]
versionsFromRaw Pkg.Package {Pkg.name} raw = do
  let magicPref = name <> "-"
      magicSuff = ".ebuild"
      doc = Html.parseLBS raw
      isEbuild e = case elementNodes e of
        [NodeContent c] | magicPref `T.isPrefixOf` c && magicSuff `T.isSuffixOf` c -> True
        _ -> False
      extractContent e = case e of
        NodeElement Element {elementNodes = [NodeContent t]} ->
          -- TODO: use stripPrefix and stripSuffix and we can probably get this directly from query.
          T.dropEnd (T.length magicSuff) $
            T.drop (T.length magicPref) t
        _ -> error "mismatched"
      parsed = fromDocument doc $// element "ul" &/ element "li" &/ element "a" >=> checkElement isEbuild
  sortOn (Data.Ord.Down . Algorithms.NaturalSort.sortKey) $ fmap (extractContent . node) parsed

fetchEbuild :: Manager -> Pkg.Package -> Version -> IO BSL.ByteString
fetchEbuild mgr pkg ver = do
  req <- parseRequest $ "https://gitweb.gentoo.org/repo/gentoo.git/plain/" <> show pkg <> "/nvidia-drivers-" <> T.unpack ver <> ".ebuild"
  resp <- httpLbs req mgr
  pure $ responseBody resp

parseNvKernelMax :: BSL.ByteString -> Maybe Version
parseNvKernelMax raw = listToMaybe do
  l0 <- BSLC.lines raw
  Just l1 <- pure do
    BSLC.stripPrefix "NV_KERNEL_MAX=\"" l0
      >>= BSLC.stripSuffix "\""
  pure (decodeUtf8 $ BSLC.toStrict l1)

main :: IO ()
main = do
  let
  mgr <- newManager tlsManagerSettings
  forM_ watchlist \pkg -> do
    putStrLn $ "Package: " <> show pkg
    vers <- versionsFromRaw pkg <$> fetchPackageDirRaw mgr pkg
    let nvSpecial = pkg == nvidiaDrivers
    forM_ vers \ver -> do
      putStr $ " - " <> T.unpack ver
      when nvSpecial $ do
        mKVer <- parseNvKernelMax <$> fetchEbuild mgr pkg ver
        case mKVer of
          Just kVer -> putStr $ ", kernel max: " <> T.unpack kVer
          _ -> pure ()
      putStrLn ""
