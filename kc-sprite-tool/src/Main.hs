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
    consumeExtension = optional (string ".json" <|> string ".png")
    urlPath, localPath :: ReadP ResourcePath
    urlPath = do
      http <- string "http://"
      part0 <- munch1 (`notElem` ['.','?'])
      _ <- consumeExtension
      ver <- option Nothing $ do
        _ <- string "?version="
        Just <$> munch1 (const True)
      pure $ UrlPath (http <> part0) ver
    localPath = do
      part0 <- munch1 (/= '.')
      _ <- consumeExtension
      pure $ LocalPath part0

main :: IO ()
main = do
  args <- getArgs
  case args of
    [srcPath, tgtPath] -> do
      let [(parsed, "")] = readP_to_S (resourcePath <* eof) srcPath
          (jsonFile, pngFile) = case parsed of
            LocalPath fp -> (fp <> ".json", fp <> ".png")
            _ -> error "TODO"
      loadSpritesmith jsonFile pngFile >>= outputImages tgtPath
    _ -> do
      putStrLn "<prog> <source> <target dir>"
      exitFailure
