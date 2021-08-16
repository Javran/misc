module Game.Chess.Fen where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Monoid
import Data.Word
import Game.Chess.Coord
import Game.Chess.Types

{-
  https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
 -}

data Record = Record
  { placement :: EightElems (EightElems (Maybe (Color, PieceType)))
  , activeColor :: Color
  , castling :: ([Side], [Side])
  , enPassantTarget :: Maybe LinearCoord
  , halfMove :: Int
  , fullMove :: Int
  }

rawStandardBoard = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

pElemP :: Parser (Sum Word8, [Maybe (Color, PieceType)])
pElemP = "Pp" ~> Pawn
  where
    (~>) :: [Char] -> PieceType -> Parser (Sum Word8, [Maybe (Color, PieceType)])
    [wRaw, bRaw] ~> pt = do
      color <- (White <$ char wRaw) <|> (Black <$ char bRaw)
      pure (1, [Just (color, pt)])
    _ ~> _ = error "unreachable"
