{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Codec.Compression.GZip
import Data.Aeson
import Data.Function
import Data.List
import System.Directory
import System.Environment
import System.IO
import System.Process

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

combine :: [BSL.ByteString] -> BSL.ByteString
combine =
  BSLB.toLazyByteString
  . foldMap (\x -> BSLB.lazyByteString x <> "\n")

xzCompressFile :: Handle -> IO (Handle, ProcessHandle)
xzCompressFile hOutp = do
  let cp =
        (proc "/usr/bin/xz" ["-9e", "-T20"])
          { std_in = CreatePipe
          , std_out = UseHandle hOutp
          }
  (Just hInp, _, _, ph) <- createProcess cp
  pure (hInp, ph)

packCommand :: [String] -> IO ()
packCommand args =
  case args of
    [srcDirRaw, outFile] -> do
      -- get files under srcDirRaw.
      -- this also serves as a sanity check to confirm that this
      -- is actually a directory.
      initFiles <- filter ("json.gz" `isSuffixOf`) <$> listDirectory srcDirRaw
      bOutExist <- doesFileExist outFile
      if bOutExist
        then error $ "File " <> outFile <> " already exists."
        else do
          putStrLn $ "Record count: " <> show (length initFiles)
          hOutp <- openFile outFile WriteMode
          (hInp, ph) <- xzCompressFile hOutp
          fix (\loop fns ->
                 case fns of
                   [] -> pure ()
                   (fn:fns') -> do
                     raw <- loadAndDecompress $ srcDirRaw <> "/" <> fn
                     BSL.hPutStr hInp raw
                     BSL.hPutStr hInp "\n"
                     loop fns')
            initFiles
          hClose hInp
          hClose hOutp
          e <- waitForProcess ph
          print e
          pure ()
    _ -> do
      putStrLn "brp pack <source dir> <output file>"
      pure ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    "pack" : as -> packCommand as
    _ -> do
      putStrLn "brp pack ..."
      pure ()
