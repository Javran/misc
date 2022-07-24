{-# LANGUAGE
    LambdaCase
  , NoMonomorphismRestriction
  , OverloadedStrings
  , TypeApplications
  #-}
module CommandInstall
  ( cmdInstall
  , updateGrubConf
  ) where

import Algorithms.NaturalSort
import Control.Exception
import Control.Monad
import Data.List
import Data.Ord
import System.Environment
import System.Exit
import Text.Microstache
import Turtle.Prelude
import Turtle.Shell

import qualified Control.Foldl as Foldl
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vec
import qualified Filesystem.Path.CurrentOS as FP

import Common

cmdInstall :: IO ()
cmdInstall =  sh $ do
  pushd "/usr/src/linux"
  ExitSuccess <- liftIO $ shDive "make" ["install"]
  ExitSuccess <- liftIO $ shDive "make" ["modules_install"]
  liftIO $ Control.Exception.try @SomeException updateGrubConf >>= \case
    Left e -> putStrLn $ "Allowed to fail: " <> displayException e
    Right _ -> pure ()
  r <- liftIO $ shDive "emerge" ["@module-rebuild"]
  case r of
    ExitSuccess -> pure ()
    ExitFailure ec  -> liftIO $
      putStrLn $ "Exitcode=" <> show ec <> " (allowed to fail)"

updateGrubConf :: IO ()
updateGrubConf =
    determineBootloader >>= getUpdateAction
  where
    getUpdateAction :: Bootloader -> IO ()
    getUpdateAction = \case
        Grub1 -> updateGrub1Conf
        Grub2 -> updateGrub2Conf

{-
  Recognize existing kernels and update grub.conf using a template.

  To recognize a valid kernel version,
  config-XXX, System.map-XXX, vmlinux-XXX must all exist,
  with XXX being exactly the same.

 -}
updateGrub1Conf :: IO ()
updateGrub1Conf = do
  -- get potential options for vmlinuz-* files.
  kernelSets <- fmap (Data.List.sortOn (Down . sortKey)) $ reduce Foldl.list $
    ls "/boot/" >>= \fp -> do
      let fName = FP.encodeString . FP.filename $ fp
          vmlzMagic = "vmlinuz-"
      True <- testfile fp
      guard $ vmlzMagic `isPrefixOf` fName
      -- we say "X.Y.Z-gentoo" is a KernelSet,
      -- if all of vmlinuz, System.map and config exists
      let kernelSet = T.pack $ drop (T.length vmlzMagic) fName
      True <- testfile ("/boot" FP.</> FP.fromText ("System.map-" <> kernelSet))
      True <- testfile ("/boot" FP.</> FP.fromText ("config-" <> kernelSet))
      pure kernelSet
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
  putStrLn "Writing to grub.conf ..."
  T.writeFile "/boot/grub/grub.conf" $ TL.toStrict grubConfContent

updateGrub2Conf :: IO ()
updateGrub2Conf = do
  (ExitSuccess, _) <-
    procStrict "/usr/sbin/grub-mkconfig" ["-o", "/boot/grub/grub.cfg"] ""
  pure ()
