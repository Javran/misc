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
ui v h = joinBorders $ center $ vhLimit fullV fullH grid
  where
    (fullV, fullH) = ((v+1)*8+1, (h+1)*8+1)
    grid = center $ vBox (intersperse hBorder $ firstRow : rows)
      where
        firstRow = vLimit 1 $
          center $ hBox $ intersperse vBorder $
            tl : (hLimit h . center . str <$> (show <$> [1 :: Int ..8]))
        tl = vhLimit 1 1 $ fill ' '
        rows = row <$> ['a'..'h']
    row hd = center $ hBox (intersperse vBorder (hdW : replicate 8 cell))
      where
        hdW = vhLimit v 1 $ center $ str [hd]
    cell = vhLimit v h $ center $ str "#"

main :: IO ()
main = simpleMain (ui 3 3)
