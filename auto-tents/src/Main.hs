module Main
  ( main
  ) where

{-
  Automatic solver for Android game "tents & trees"
 -}

{-
  Design: for now I think the most difficult part will be,
  given an image, to recognize board position and
  extract board configuration from it.
  good old matchTemplate should work, but here I want to see
  if there are better ways that is not sensitive to scaling
  so that a method works for 10x10 board will also work for 20x20 board.

  Questions:

  - where is the board?

  - how to extract and recognize trees and empty spaces?

  - how to extract and recognize digits on board sides?

 -}

main :: IO ()
main = pure ()
