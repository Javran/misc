module Game.Chess.Fen where


import Data.Attoparsec.ByteString.Char8
import Game.Chess.Types
import Game.Chess.Coord

{-
  https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
 -}


data Record = Record
  { placement :: [[Maybe (Color, PieceType)]]
  , activeColor :: Color
  , castling :: ([Side], [Side])
  , enPassantTarget :: Maybe LinearCoord
  , halfMove :: Int
  , fullMove :: Int
  }

rawStandardBoard = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
