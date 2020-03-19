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
  TODO:

  Turns out the plan does not work: version are sometimes versions in
  an inconsistent way (e.g. ship resources use only version numbers),
  instead of dealing with that mess I've decided to scrap this plan
  and this binary might be turned into just a spritesmith resource extractor
  or something of that description.

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
