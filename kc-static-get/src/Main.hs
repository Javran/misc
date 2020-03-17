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
  - Include a base output directory in the config file.
    output path will be:

    {baseOutputPath}/{version}/{resourcePath}

    e.g.
    /kcs2/img/common/common_itemicons.json?version=4.5.3.0

    becomes:
    {baseOutputPath}/4.5.3.0/kcs2/img/common/common_itemicons.json

    if this resource is a bundle of some other resources
    (in this case, json+png describes that bundle)
    we'll have a directory with '.extract' as suffix. e.g.:

    {baseOutputPath}/4.5.3.0/kcs2/img/common/common_itemicons.extract/

    Note that if version is not specified, it is default to string "current"
    and we always download the resource overwriting what's existing.

  - After we have the base output directory in config file,
    the binary simply accepts two args: <path to config> <resource>.

    where <resource> can be a url with version number, or a local file.

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
