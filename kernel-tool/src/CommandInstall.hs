{-# LANGUAGE OverloadedStrings #-}
module CommandInstall
  ( cmdInstall
  ) where

import Data.List
import qualified Control.Foldl as Foldl
import qualified Data.Text as T
import Turtle hiding (fp)

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
updateGrubConf = do
  -- get potential options for vmlinuz-* files.
  kernelSets <- reduce Foldl.list $
    ls "/boot/" >>= \fp -> do
      let fName = encodeString . filename $ fp
          vmlzMagic = "vmlinuz-"
      True <- testfile fp
      guard $ vmlzMagic `isPrefixOf` fName
      -- we say "X.Y.Z-gentoo" is a KernelSet,
      -- if all of vmlinuz, System.map and config exists
      let kernelSet = T.pack $ drop (length vmlzMagic) fName
      True <- testfile ("/boot" </> fromText ("System.map-" <> kernelSet))
      True <- testfile ("/boot" </> fromText ("config-" <> kernelSet))
      pure kernelSet
  print kernelSets
