{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import System.Environment
import System.Directory
import System.IO

import Data.List
import Data.Aeson
import Codec.Compression.GZip

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BSLB

{-
  For packing poi battle-detail records into one.

  WIP:
  - read a directory and find all ".json.gz" files.
  - put them all into one file, with records merged together
    separated by newline. (the output file)
  - then we can use whatever tool we like to compress that to a smaller one.

 -}

loadAndDecompress :: FilePath -> IO BSL.ByteString
loadAndDecompress fp = do
  h <- openFile fp ReadMode
  raw <- BSL.hGetContents h
  let x = BSL.toStrict $ decompress raw
  x `seq` hClose h
  pure $ BSL.fromStrict x

combineAndCompress :: [BSL.ByteString] -> BSL.ByteString
combineAndCompress =
  compress
  . BSLB.toLazyByteString
  . foldMap (\x -> BSLB.lazyByteString x <> "\n")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [srcDirRaw, outFile] -> do
      -- get files under srcDirRaw.
      -- this also serves as a sanity check to confirm that this
      -- is actually a directory.
      files <- filter ("json.gz" `isSuffixOf`) <$> listDirectory srcDirRaw
      bOutExist <- doesFileExist outFile
      if bOutExist
        then error $ "File " <> outFile <> " already exists."
        else do
          putStrLn $ "Record count: " <> show (length files)
          contents <- mapM (\fn -> loadAndDecompress $ srcDirRaw <> "/" <> fn ) files
          let raw = combineAndCompress contents
          BSL.writeFile outFile raw
    _ -> do
      putStrLn "brp <source dir> <output file>"
      pure ()
