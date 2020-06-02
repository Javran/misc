{-
  An experiment to create an "output sink" that can keep reading from
  a Handle and return its full content with the Handle is closed.
 -}
{-# LANGUAGE
    NumericUnderscores
  , OverloadedStrings
  #-}
module Main
  ( main
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception.Safe
import Data.Function
import System.Directory
import System.FilePath.Posix
import System.IO
import System.IO.Error
import System.Process

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL

createOutputSink :: Handle -> IO (Async BSL.ByteString, MVar BSB.Builder)
createOutputSink h = do
  -- placeholder value, `out` below is the source of truth.
  mOut <- newMVar ""
  t <- async $
    fix (\loop out -> do
          r <- tryIO $ do
            _ <- hWaitForInput h (-1)
            BSL.hGetNonBlocking h 4096
          case r of
            Left e ->
              if isEOFError e
                 then pure $ BSB.toLazyByteString out
                 else throw e
            Right xs -> do
              let out' = out <> BSB.lazyByteString xs
              -- force WHNF and swap in.
              _ <- swapMVar mOut $! out'
              loop out')
    ""
  pure (t, mOut)

main :: IO ()
main = do
  d <- getCurrentDirectory
  let cp = (proc (d </> "forever.sh") [])
           { std_out = CreatePipe }
  (_, Just hOut, _, ph) <- createProcess cp
  (t, mOut) <- createOutputSink hOut
  _task <- async $ do
    threadDelay $ 1_000_000 * 2
    terminateProcess ph
  fix $ \loop -> do
    r <- poll t
    case r of
      Nothing -> do
        raw <- readMVar mOut
        putStrLn $
          "Accumulated output size: " <> show (BSL.length (BSB.toLazyByteString raw))
        loop
      Just _ ->
        putStrLn "The process is terminated."
  pure ()
