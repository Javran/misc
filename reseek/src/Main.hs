{-# LANGUAGE BangPatterns #-}
module Main
  ( main
  ) where

{-
  Proof of concept of holding file handle without closing
  on pseudo file systems like procfs and sysfs.

  Few places that we might want to try:

  - /proc/cpuinfo
  - /proc/stat
  - /proc/net/dev
  - /proc/meminfo

  Well we don't actually need to try sysfs - it was
  required for dealing with battery, but there's no need for Senatus.
 -}

import Data.Function
import System.IO
import Control.Monad

import qualified Data.ByteString.Char8 as BSC

reseekContent :: Handle -> IO BSC.ByteString
reseekContent h = do
  hSeek h AbsoluteSeek 0
  -- we need to do this in low-level fashion because
  -- hGetContents will automatically close the file
  -- so that it needs to be re-opened every time.

  -- Looks like large buffer (> 16384-ish) is slower,
  -- presumbly there are allocation penalty involved.
  let bufSize = 512
  dlist <- fix
    (\readMore acc -> do
        b <- hIsEOF h
        if b
          then pure acc
          else do
            raw <- BSC.hGetNonBlocking h bufSize
            readMore (acc . (raw:))
          ) id
  pure (BSC.concat (dlist []))

readProc :: IO BSC.ByteString
readProc = do
  h <- openFile "/proc/cpuinfo" ReadMode
  BSC.hGetContents h -- no need of closing as hGetContents does that automatically.

mainNormal :: Int -> IO ()
mainNormal opCount = replicateM_ opCount $ do
  raw <- readProc
  putStrLn $ "Got " <> show (BSC.length raw) <> " bytes."

mainReseek :: Int -> IO ()
mainReseek opCount = do
  h <- openFile "/proc/cpuinfo" ReadMode
  replicateM_ opCount $ do
    raw <- reseekContent h
    putStrLn $ "Got " <> show (BSC.length raw) <> " bytes."
  hClose h

{-
  Note: so far mainNormal vs. mainReseek doesn't appear to have significant difference.
  but mainReseek defintely requires some tuning on bufSize to get a little bit better on performance.

  However, this situation might change once we use attoparsec to parse the data on the fly,
  for now the re-seeking method has to piece things together with concat, which still require some copy
  while we don't need to worry about that for hGetContents.
 -}
main :: IO ()
main = mainNormal 10000
-- main = mainReseek 10000
