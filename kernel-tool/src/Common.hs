{-# LANGUAGE
    OverloadedStrings
  #-}
module Common
  ( shDive
  , Bootloader(..)
  , determineBootloader
  ) where

import System.Exit
import System.Process
import Turtle.Prelude

import qualified Data.Text as T

data Bootloader = Grub1 | Grub2

determineBootloader :: IO Bootloader
determineBootloader = do
  mBl <- fmap (T.toLower . T.strip) . lookup "BOOTLOADER" <$> Turtle.Prelude.env
  case mBl of
    Just "grub1" -> pure Grub1
    Just "grub2" -> pure Grub2
    _ -> do
      putStrLn "BOOTLOADER is not set, it must be either 'grub1' or 'grub2'."
      exitFailure

shDive :: FilePath -> [String] -> IO ExitCode
shDive binPath args = do
  (_, _, _, ph) <- createProcess $
    (System.Process.proc binPath args)
      { std_in  = Inherit
      , std_out = Inherit
      , std_err = Inherit
      , delegate_ctlc = True
      }
  waitForProcess ph
