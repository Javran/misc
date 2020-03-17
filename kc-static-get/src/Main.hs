{-
  Get static resources from kc servers. perferably with
  requests spreaded so we don't hammer a specific one.
 -}
{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
  #-}
module Main
  ( main
  ) where

import Dhall
import System.Exit
import System.Environment

import Spritesmith

import qualified Config


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

  example:
  - "common_itemicons_id_75",FrameInfo {fiCoord = (480,80), fiSize = (75,75)}
  - item: 新型砲熕兵装資材
  - examined by gimp (top-left corner is (1,1)), the corresponding icon goes from (481,81) to (555,155).
 -}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configPath, jsonFile, pngFile, outputDir] -> do
      cfg <- inputFile @Config.Config auto configPath
      print cfg
      loadSpritesmith jsonFile pngFile >>= outputImages outputDir
    _ -> do
      putStrLn "<prog> <config path> <json file path> <png file path> <output dir>"
      exitFailure
