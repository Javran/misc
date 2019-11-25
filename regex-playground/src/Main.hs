{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Text.Regex.Base.RegexLike
import Text.Regex.PCRE.ByteString
import Data.Traversable

import qualified Data.ByteString as BS

main :: IO ()
main = do
  let rawSample = "[VER:v=12.45|stuff1|stuff2|stuff3[X], extra1, extra2, extra 3]"
  Right re <- compile blankCompOpt blankExecOpt "\\[VER:v=(\\d+\\.\\d+)((\\|[a-z0-9]+(\\[X\\])?)+)(, .*)*\\]"
  Right (Just r) <- execute re rawSample
  forM r $ \(offset, len) ->
    print (BS.take len $ BS.drop offset rawSample)
  pure ()
