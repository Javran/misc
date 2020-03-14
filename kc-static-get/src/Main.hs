{-
  Get static resources from kc servers. perferably with
  requests spreaded so we don't hammer a specific one.
 -}
{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , DeriveGeneric
  #-}
module Main
  ( main
  ) where

import Dhall
import System.Exit
import System.Environment
import Data.Aeson

import qualified Data.HashMap.Strict as HM

import qualified Config

data FrameInfo
  = FrameInfo
  { fiCoord :: (Int, Int) -- (x,y)
  , fiSize :: (Int, Int) -- (w,h)
  } deriving (Generic)

instance FromJSON FrameInfo

newtype SpriteFrames
  = SpriteFrames (HM.HashMap Text FrameInfo)
    deriving (Generic)

instance FromJSON SpriteFrames

{-
  Example resource:
  - /kcs2/img/common/common_itemicons.json?version=4.5.3.0
  - /kcs2/img/common/common_itemicons.png?version=4.5.3.0

  somehow we need an path for output and we can download all stuff to there.

  also need a dedicate module for https://github.com/twolfson/spritesmith,
  I think we can write one just for PNG format.

  structure of this json file:

  - frames: an object
    - keys are file names, values are objects
      - frame:
        - x,y,w,h
        - rotated: false
        - trimmed: false
        - spriteSourceSize: {x,y,w,h}
        - sourceSize: {w,h}
 -}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configPath, jsonFile, pngFile] -> do
      cfg <- inputFile @Config.Config auto configPath
      print cfg
    _ -> do
      putStrLn "<prog> <config path> <json file path> <png file path>"
      exitFailure
