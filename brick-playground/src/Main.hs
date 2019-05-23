module Main
  ( main
  ) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Data.List

ui :: Widget ()
ui = center $ vBox (replicate 4 wLine)
  where
    wLine =
      hBox $ intersperse (str "|")
        $ vLimit 5 . hLimit 10 . str <$> words "AAAA BBBB CCCC DDDD"

main :: IO ()
main = simpleMain ui
