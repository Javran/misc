{-# LANGUAGE OverloadedStrings #-}
{-
  Script for wiping out something entirely

  - files.txt is prepare by calling window.getShipImgPath on poi-plugin-navy-album
    exhausting all valid parameters.

-}

module NoMore
  ( main
  ) where

import Turtle
import Prelude hiding (FilePath)
import qualified Data.ByteString as BS
-- import Filesystem.Path.CurrentOS
import Control.Monad

getPoiCachePath :: MonadIO m => m FilePath
getPoiCachePath =
  (\p -> p </> ".config" </> "poi" </> "MyCache" </> "KanColle") <$> home

main :: IO ()
main = do
  emptyPngContent <- BS.readFile "empty.png"
  resourcePaths <- lines <$> readFile "files.txt"
  poiPath <- getPoiCachePath
  forM_ resourcePaths $ \(_:rscRaw) -> do
    let curPath = poiPath </> fromString rscRaw
    mktree (directory curPath)
    BS.writeFile (encodeString curPath) emptyPngContent
