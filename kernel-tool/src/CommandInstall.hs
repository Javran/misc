{-# LANGUAGE OverloadedStrings #-}
module CommandInstall
  ( cmdInstall
  ) where

import Turtle

cmdInstall :: IO ()
cmdInstall = do
  sh $ do
    pushd "/usr/src/linux"
    (ExitSuccess, _) <- procStrict "make" ["install"] ""
    (ExitSuccess, _) <- procStrict "make" ["modules_install"] ""
    (ExitSuccess, _) <- procStrict "emerge" ["@module-rebuild"] ""
    pure ()
  updateGrubConf

{-
  TODO: recognize existing kernels and update grub.conf using a template.

  - to recognize a valid kernel version:

    + config-XXX, System.map-XXX, vmlinux-XXX must all exist, with XXX being exactly the same
  - use http://hackage.haskell.org/package/natural-sort or something alike to sort
    kernel versions
  - use http://hackage.haskell.org/package/microstache for generating grub.conf

 -}
updateGrubConf :: IO ()
updateGrubConf = pure ()
