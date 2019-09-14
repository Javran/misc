module Main
  ( main
  ) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Data.List

ui :: Widget ()
ui = joinBorders $ center $ border $ vLimit 15 $ hLimit 15 grid
  where
    grid = center $ vBox (intersperse hBorder (replicate 8 row))
    row = center $ hBox (intersperse vBorder (replicate 8 wTest))
    wTest = vLimit 1 $ hLimit 1 $ str "#"

main :: IO ()
main = simpleMain ui
