module Main
  ( main
  ) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border

ui :: Widget ()
ui = center $ joinBorders $ border $ setAvailableSize (20,10) $ row <+> vBorder <+> center row <+> vBorder <+> row
  where
    row = wTest <=> hBorder <=> center wTest <=> hBorder <=> wTest
    wTest = setAvailableSize (5,3) $ str "text"

main :: IO ()
main = simpleMain ui
