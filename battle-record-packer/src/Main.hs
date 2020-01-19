{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Main
  ( main
  ) where

import Codec.Compression.GZip
import Control.Applicative
import Data.Aeson
import Data.Aeson.Parser
import Data.Function
import Data.List
import Data.Monoid
import System.Directory
import System.Environment
import System.IO
import System.Process
import Control.Exception

import qualified Data.Attoparsec.ByteString as Parser
import qualified Data.ByteString as BS
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

loadAndDecompress' :: FilePath -> IO BSL.ByteString
loadAndDecompress' fp = do
  h <- openFile fp ReadMode
  raw <- BSL.hGetContents h
  let x = BSL.toStrict $ decompress raw
  x `seq` hClose h
  pure $ BSL.fromStrict x

loadAndDecompress :: FilePath -> IO BSL.ByteString
loadAndDecompress fp =
  catch @SomeException (loadAndDecompress' fp) $ \e -> do
    hPutStrLn stderr $ "Error decompressing file: " <> fp
    hPutStrLn stderr $ displayException e
    pure ""

combine :: [BSL.ByteString] -> BSL.ByteString
combine =
  BSLB.toLazyByteString
  . foldMap (\x -> BSLB.lazyByteString x <> "\n")

xzCompressFile :: Handle -> IO (Handle, ProcessHandle)
xzCompressFile hOutp = do
  let cp =
        (proc "/usr/bin/xz" ["-9e", "-T20", "-vv", "--memlimit-decompress=4GiB", "--memlimit-compress=16GiB"])
          { std_in = CreatePipe
          , std_out = UseHandle hOutp
          }
  (Just hInp, _, _, ph) <- createProcess cp
  pure (hInp, ph)

xzDecompressFile :: FilePath -> IO (Handle, ProcessHandle)
xzDecompressFile xzFile = do
  -- here we use the verbose mode to check on progress.
  -- since the bottleneck is in parsing, the "decompress" speed
  -- actually indicates how fast our parser can go.
  let cp =
        (proc "/usr/bin/xz" ["-d", "-c", "-vv", xzFile])
          { std_out = CreatePipe
          }
  (_, Just hOutp, _, ph) <- createProcess cp
  hSetBinaryMode hOutp True
  hSetBuffering hOutp (BlockBuffering (Just 65536))
  pure (hOutp, ph)

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

verifyCommand :: [String] -> IO ()
verifyCommand args =
  case args of
    [packedFile] -> do
      (hInp, ph) <- xzDecompressFile packedFile
      r <- fix
        (\loop consume ->
            do
              b <- hIsEOF hInp
              if b
                then
                  pure (consume "")
                else do
                  raw <- BS.hGet hInp 65536
                  case consume raw of
                    v@(Parser.Done _ r) -> pure v
                    Parser.Fail _ _ m -> error (show m)
                    Parser.Partial consume' -> loop consume')
        (Parser.parse (some ((1 :: Sum Int) <$ json)))
      let Parser.Done _ xs = r
      putStrLn $ "Line count: " <> show (getSum $ mconcat xs)
    _ -> do
      putStrLn "brp verify <xz file>"
      pure ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    "pack" : as -> packCommand as
    "verify" : as -> verifyCommand as
    _ -> do
      putStrLn "brp <pack|verify> ..."
      pure ()
