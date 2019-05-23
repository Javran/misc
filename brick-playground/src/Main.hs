module Main
  ( main
  ) where

import Brick

ui :: Widget ()
ui = str "Saluton!"

main :: IO ()
main = simpleMain ui
