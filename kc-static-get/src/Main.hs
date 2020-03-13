{-
  Get static resources from kc servers. perferably with
  requests spreaded so we don't hammer a specific one.
 -}
{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  #-}
module Main
  ( main
  ) where

import Dhall
import System.Exit
import System.Environment

import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T

import qualified Config


main :: IO ()
main = do
  args <- getArgs
  case args of
    [configPath] -> do
      cfg <- inputFile @Config.Config auto configPath
      print cfg
    _ -> do
      putStrLn "<prog> <config path>"
      exitFailure
