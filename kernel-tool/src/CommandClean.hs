module CommandClean
  ( cmdClean
  ) where

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
cmdClean = pure ()
