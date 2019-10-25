{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Prelude hiding (FilePath)

import Filesystem.Path.CurrentOS
import System.Exit
import Turtle.Prelude
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL


{-
  This program parses output from `sensors` and print out useful info
  regarding temperature readings.

  Eventually this will end up being info source to some xmonad components.

 -}
toText' :: FilePath -> T.Text
toText' = either id id . toText

main :: IO ()
main = do
  Just sensorsBinPath <- which "sensors"
  (ExitSuccess, rawOut) <- procStrict (toText' sensorsBinPath) ["-j"] ""
  let parsed :: Object
      Right parsed = eitherDecode' . BSL.fromStrict . encodeUtf8 $ rawOut
  print parsed
