module CommandClean
  ( cmdClean
  ) where

{-
  for now, `clean` command only aims at:
  - moving stuff from /boot/backup/ into /tmp
  - moving old kernel into /boot/backup/
 -}
cmdClean :: IO ()
cmdClean = pure ()
