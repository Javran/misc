{-# LANGUAGE OverloadedStrings #-}
module CommandInstall
  ( cmdInstall
  ) where

import Turtle

cmdInstall :: IO ()
cmdInstall = sh $ do
  pushd "/usr/src/linux"
  (ExitSuccess, _) <- procStrict "make" ["install"] ""
  (ExitSuccess, _) <- procStrict "make" ["modules_install"] ""
  (ExitSuccess, _) <- procStrict "emerge" ["@module-rebuild"] ""
  -- TODO: update grub.conf
  pure ()
