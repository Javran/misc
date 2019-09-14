module Main
  ( main
  ) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Data.List

vhLimit :: Int -> Int -> Widget a -> Widget a
vhLimit v h = vLimit v . hLimit h

ui :: Widget ()
ui = joinBorders $ center $ border $ vhLimit (v*8+7) (v*8+7) grid
  where
    (v,h) = (2,2)
    grid = center $ vBox (intersperse hBorder (replicate 8 row))
    row = center $ hBox (intersperse vBorder (replicate 8 cell))
    cell = vhLimit v h $ str "#"

main :: IO ()
main = simpleMain ui
