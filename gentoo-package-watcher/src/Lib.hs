{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( main
  )
where

import qualified Algorithms.NaturalSort
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Ord
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Text.HTML.DOM as Html
import Text.XML
import Text.XML.Cursor

type Package = (String, String)

watchlist :: [(String, String)]
watchlist =
  [ ("sys-kernel", "gentoo-sources")
  , ("x11-drivers", "nvidia-drivers")
  ]

fetchPackageDirRaw :: Manager -> Package -> IO BSL.ByteString
fetchPackageDirRaw mgr (p1, p2) = do
  req <- parseRequest $ "https://gitweb.gentoo.org/repo/gentoo.git/plain/" <> p1 <> "/" <> p2
  resp <- httpLbs req mgr
  pure $ responseBody resp

listFilesFromRaw :: Package -> BSL.ByteString -> IO ()
listFilesFromRaw (_, p2) raw = do
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
      versions = sortOn (Data.Ord.Down . Algorithms.NaturalSort.sortKey) $ fmap (extractContent . node) parsed
  mapM_ print versions

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  mapM_ (\pkg -> fetchPackageDirRaw mgr pkg >>= listFilesFromRaw pkg) watchlist
