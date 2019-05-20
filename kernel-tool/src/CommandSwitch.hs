{-# LANGUAGE OverloadedStrings #-}
module CommandSwitch
  ( cmdSwitch
  ) where

import Turtle hiding (w)
import Text.ParserCombinators.ReadP as P
import Data.Char
import Data.Maybe

import qualified Data.Text as T

{-
  kernel info: (<item number>, (<kernel version>, <is selected>))
 -}
parseKernelInfo :: T.Text -> Maybe (Int, (String, Bool))
parseKernelInfo t = case readP_to_S (skipSpaces *> kernelInfoP <* P.eof) (T.unpack t) of
  [(r,"")] -> Just r
  _ -> Nothing

kernelInfoP :: ReadP (Int, (String,Bool))
kernelInfoP = do
  w <- read <$> (P.char '[' *> munch1 isDigit <* P.char ']' <* skipSpaces)
  kerVer <- munch1 (not . isSpace) <* skipSpaces
  selected <- P.option False (True <$ P.char '*') <* skipSpaces
  pure (w, (kerVer, selected))

cmdSwitch :: IO ()
cmdSwitch = do
  (ExitSuccess, out) <- procStrict "eselect" ["kernel", "list"] ""
  let _:rawKVers = T.lines out
      kVers = fromJust . parseKernelInfo <$> rawKVers
      latestKVer = last kVers
  unless (snd . snd $ latestKVer) $ do
    putStrLn "Switching to latest kernel version ..."
    (ExitSuccess, _) <-
        procStrict
          "eselect"
          ["kernel", "set", T.pack . show . fst $ latestKVer]
          ""
    pure ()
