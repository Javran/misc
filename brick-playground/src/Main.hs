module Main
  ( main
  ) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border

ui :: Widget ()
ui = center $ joinBorders $ border $ vLimit 10 $ hLimit 20 $ row <+> vBorder <+> row <+> vBorder <+> row
  where
    row = wTest <=> hBorder <=> wTest <=> hBorder <=> wTest
    wTest = vLimit 2 . hLimit 4 $ str "text"

main :: IO ()
main = simpleMain ui
