module Main
  ( main
  ) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

ui :: Widget ()
ui =
  withBorderStyle unicode $
    center $ vBox (replicate 6 wLine)
  where
    wLine = hBox $ fmap (border . str . pure) ['A'..'Z']

main :: IO ()
main = simpleMain ui
