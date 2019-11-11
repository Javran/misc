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

  Well we don't actually need to try sysfs

 -}

import Data.Function
import System.IO
import Control.Monad

import qualified Data.ByteString.Char8 as BSC

reseekContent :: Handle -> IO ()
reseekContent h = do
  hSeek h AbsoluteSeek 0
  -- we need to do this in low-level fashion because
  -- hGetContents will automatically close the file
  -- so that it needs to be re-opened every time.

  -- Looks like large buffer (> 16384-ish) is slower,
  -- presumbly there are allocation penalty involved.
  let bufSize = 512
  count <- fix
    (\readMore !acc -> do
        b <- hIsEOF h
        if b
          then pure acc
          else do
            raw <- BSC.hGetNonBlocking h bufSize
            readMore (acc + BSC.length raw)
          ) 0
  putStrLn $ "Got " <> show count <> " bytes."

readProc :: IO ()
readProc = do
  h <- openFile "/proc/cpuinfo" ReadMode
  raw <- BSC.hGetContents h -- no need of closing as hGetContents does that automatically.
  putStrLn $ "Got " <> show (BSC.length raw) <> " bytes."

mainNormal :: Int -> IO ()
mainNormal opCount = replicateM_ opCount readProc

mainReseek :: Int -> IO ()
mainReseek opCount = do
  h <- openFile "/proc/cpuinfo" ReadMode
  replicateM_ opCount (reseekContent h)
  hClose h

main :: IO ()
main = mainNormal 10000
-- main = mainReseek 10000
