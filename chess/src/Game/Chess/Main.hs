module Game.Chess.Main where

import Game.Chess.Types
import Graphics.SVGFonts
import Graphics.SVGFonts.ReadFont
import Paths_chess

testTexts =
  [ "tmvwlvmt"
  , "oooooooo"
  , "pppppppp"
  , "rnbqkbnr"
  ]

main :: IO ()
main = do
  fp <- getDataFileName "data/ChessMerida.svg"
  font@(fd, _) <- loadFont fp
  print (fontDataFamily fd)
  pure ()
