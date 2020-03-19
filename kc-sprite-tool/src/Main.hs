{-
  Get static resources from kc servers. perferably with
  requests spreaded so we don't hammer a specific one.
 -}
{-# LANGUAGE
    OverloadedStrings
  #-}
module Main
  ( main
  ) where

import System.Exit
import System.Environment
import Text.ParserCombinators.ReadP hiding (optional)
import Control.Applicative

import Spritesmith

{-
  TODO:

  Redo command line options:

  kc-sprite-tool <source> <target>

  source: if begin with "http://", treat it as online resource,
  otherwise try to read from filesystem.
  the extension name of source is ignroed, both json and png file should be available.

  target: must be an existing directory.

 -}

data ResourcePath
  = UrlPath
    { rpSourcePath :: String
    , rpVersion :: Maybe String
    }
  | LocalPath
    { rpSourcePath :: String }
  deriving Show

resourcePath :: ReadP ResourcePath
resourcePath = urlPath <++ localPath
  where
    urlPath, localPath :: ReadP ResourcePath
    urlPath = do
      http <- string "http://"
      part0 <- many1 get
      _ <- optional (string ".json" <|> string ".png")
      ver <- option Nothing $ do
        _ <- string "?version="
        Just <$> many1 get
      pure $ UrlPath (http <> part0) ver
    localPath = do
      part0 <- many1 get
      _ <- optional (string ".json" <|> string ".png")
      pure $ LocalPath part0

main :: IO ()
main = do
  args <- getArgs
  case args of
    [srcPath, tgtPath] -> do
      print $ readP_to_S (resourcePath <* eof) srcPath
      -- loadSpritesmith jsonFile pngFile >>= outputImages outputDir
    _ -> do
      putStrLn "<prog> <source> <target dir>"
      exitFailure
