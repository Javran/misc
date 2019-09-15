module Main
  ( main
  ) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Data.List

vhLimit :: Int -> Int -> Widget a -> Widget a
vhLimit v h = vLimit v . hLimit h

ui :: Int -> Int -> Widget ()
ui v h = joinBorders $ center $ border $ vhLimit (v*8+7) (h*8+7) grid
  where
    grid = center $ vBox (intersperse hBorder (replicate 8 row))
    row = center $ hBox (intersperse vBorder (replicate 8 cell))
    cell = vhLimit v h $ center $ str "#"

main :: IO ()
main = simpleMain (ui 3 3)
