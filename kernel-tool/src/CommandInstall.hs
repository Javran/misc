{-# LANGUAGE OverloadedStrings #-}
module CommandInstall
  ( cmdInstall
  ) where

import Data.List
import Data.Ord
import System.Environment
import qualified Control.Foldl as Foldl
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Turtle hiding (fp)
import Algorithms.NaturalSort
import Text.Microstache
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as Vec

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
  recognize existing kernels and update grub.conf using a template.

  - to recognize a valid kernel version:

    + config-XXX, System.map-XXX, vmlinux-XXX must all exist, with XXX being exactly the same
  - use http://hackage.haskell.org/package/natural-sort or something alike to sort
    kernel versions
  - use http://hackage.haskell.org/package/microstache for generating grub.conf

 -}
updateGrubConf :: IO ()
updateGrubConf = do
  -- get potential options for vmlinuz-* files.
  kernelSets <- fmap (Data.List.sortOn (Down . sortKey)) $ reduce Foldl.list $
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
  let context =
        Aeson.Object $
          HM.singleton
            "kernel_version" $
            Aeson.Array $
              Vec.fromList (toVerObj <$> kernelSets)
      toVerObj :: T.Text -> Aeson.Value
      toVerObj t = Aeson.Object $ HM.singleton "ver" (Aeson.String t)
  tmpl <- getEnv "KERNEL_TOOL_GRUB_CONF_TEMPLATE" >>= compileMustacheFile
  let grubConfContent = renderMustache tmpl context
  putStrLn (T.unpack $ TL.toStrict grubConfContent)
  -- TODO: perform the actual update.