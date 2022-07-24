{-# LANGUAGE
    OverloadedStrings
  #-}
module CommandSwitch
  ( cmdSwitch
  ) where

import Control.Monad
import Data.Char
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import Text.ParserCombinators.ReadP as P
import Turtle.Prelude

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Common

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
  -- get info about existing kernel versions
  (ExitSuccess, out) <- procStrict "eselect" ["kernel", "list"] ""
  let _:rawKVers = T.lines out
      kVers = fromJust . parseKernelInfo <$> rawKVers
      latestKVer = last kVers
  -- switch to latest (last) kernel version if we haven't done so.
  if snd . snd $ latestKVer
    then putStrLn "Skipping kernel version switch ..."
    else do
      putStrLn "Switching to latest kernel version ..."
      (ExitSuccess, _) <-
        procStrict
          "eselect"
          ["kernel", "set", T.pack . show . fst $ latestKVer]
          ""
      pure ()
  -- copy over .config from current kernel if the file is missing
  cfgFileExist <- doesFileExist "/usr/src/linux/.config"
  unless cfgFileExist $ do
    putStrLn "Creating .config from current running config."
    (ExitSuccess, cfgContent) <- procStrict "zcat" ["/proc/config.gz"] ""
    T.writeFile "/usr/src/linux/.config" cfgContent
    pure ()
  diveInSubshell

diveInSubshell :: IO ()
diveInSubshell = do
  putStrLn "Will now switch to subshell for `make oldconfig && make` to happen."
  putStrLn "When done, feel free to continute with `kernel-tool install`."
  cd "/usr/src/linux"
  shBin <- getEnv "SHELL"
  _ <- shDive shBin []
  pure ()
