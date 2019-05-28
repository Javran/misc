{-# LANGUAGE OverloadedStrings #-}
module CommandClean
  ( cmdClean
  ) where

import Turtle.Prelude
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Text as T

{-
  environment variables:

  - KERNEL_TOOL_CLEAN_LIMIT: numbers of kernels to keep
    default=2
  - KERNEL_TOOL_CLEAN_BACKUP_DIR: backup directory.
    default=/boot/backup
 -}

{-
  TODO:
  for now, `clean` command only aims at:
  - moving stuff from backup dir into /tmp/<random dir>
  - moving old kernel into backup dir
  - moving partial kernel files into backup
    (those that have vmlinuz or System.map or config but not all of 3)
 -}
cmdClean :: IO ()
cmdClean = do
  curEnv <- env
  let kernelNumLimit :: Int
      kernelNumLimit =
        case lookup "KERNEL_TOOL_CLEAN_LIMIT" curEnv of
          Just x
            | [(v,"")] <- reads . T.unpack $ x
              -> if v < 1
                   then error "limit should not be less than one, aborted."
                   else v
          _ -> 2
      backupDir :: FP.FilePath
      backupDir =
        case lookup "KERNEL_TOOL_CLEAN_BACKUP_DIR" curEnv of
          Just p -> FP.fromText p
          Nothing -> "/boot/backup"
  putStrLn $ "Limit number of kernels: " <> show kernelNumLimit
  putStrLn $ "Backup dir: " <> FP.encodeString backupDir
  -- TODO: impl
