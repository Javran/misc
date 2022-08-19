{-# LANGUAGE BlockArguments #-}
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
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Text.HTML.DOM as Html
import Text.XML
import Text.XML.Cursor

type Package = (String, String)

type Version = T.Text

nvidiaDrivers :: Package
nvidiaDrivers = ("x11-drivers", "nvidia-drivers")

watchlist :: [(String, String)]
watchlist =
  nvidiaDrivers :
  [ ("sys-kernel", "gentoo-sources")
  , ("media-video", "pipewire")
  ]

fetchPackageDirRaw :: Manager -> Package -> IO BSL.ByteString
fetchPackageDirRaw mgr (p1, p2) = do
  req <- parseRequest $ "https://gitweb.gentoo.org/repo/gentoo.git/plain/" <> p1 <> "/" <> p2
  resp <- httpLbs req mgr
  pure $ responseBody resp

versionsFromRaw :: Package -> BSL.ByteString -> [Version]
versionsFromRaw (_, p2) raw = do
  let magicPref = T.pack (p2 <> "-")
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

fetchEbuild :: Manager -> Package -> Version -> IO BSL.ByteString
fetchEbuild mgr (p1, p2) ver = do
  req <- parseRequest $ "https://gitweb.gentoo.org/repo/gentoo.git/plain/" <> p1 <> "/" <> p2 <> "/nvidia-drivers-" <> T.unpack ver <> ".ebuild"
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
  forM_ watchlist \pkg@(p1,p2) -> do
    putStrLn $ "Package: " <> p1 <> "/" <> p2
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
