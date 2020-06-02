{-
  An experiment to create an "output sink" that can keep reading from
  a Handle and return its full content with the Handle is closed.
 -}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import System.IO
import Control.Concurrent.Async
import Data.Function
import Control.Exception.Safe
import System.IO.Error

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB

createOutputSink :: Handle -> IO (Async BSL.ByteString)
createOutputSink h = async $
  fix (\loop out -> do
          r <- tryIO $ do
            _ <- hWaitForInput h (-1)
            BS.hGetNonBlocking h 1024
          case r of
            Left e ->
              if isEOFError e
                 then pure $ BSB.toLazyByteString out
                 else throw e
            Right xs ->
              loop (out <> BSB.byteString xs))
  ""

main :: IO ()
main = pure ()
