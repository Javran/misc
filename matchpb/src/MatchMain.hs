{-# LANGUAGE TypeApplications #-}
module MatchMain
  ( main
  ) where

import System.Environment
import Data.ProtoLens

import qualified Data.ByteString as BS
import qualified Proto.MatchingResult as P

main :: IO ()
main = do
  [fp] <- getArgs
  raw <- BS.readFile fp
  case decodeMessage @P.MatchingResult raw of
    Left err -> putStrLn $ "Failed to decode: " <> err
    Right msg ->
      putStrLn . showMessage $ msg
