module Lib
  ( main
  )
where

watchlist :: [String]
watchlist =
  [ "sys-kernel/gentoo-sources"
  , "media-video/pipewire"
  , "x11-drivers/nvidia-drivers"
  ]

main :: IO ()
main = pure ()
