module Game.Chess.Main where

import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding (font)
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

chessPieces :: PreparedFont Double -> Diagram B
chessPieces font = stroke (textSVG' opts (concat testTexts)) # lc blue # bg white
  where
    opts = TextOpts font INSIDE_H KERN False 1 70

main :: IO ()
main = do
  fp <- getDataFileName "data/ChessMerida.svg"
  font <- loadFont fp
  mainWith (chessPieces font)
  pure ()
