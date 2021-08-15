{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module Game.Chess.Main where

import qualified Data.Vector.Unboxed as VU
import Data.Word
import Game.Chess.Coord
import Game.Chess.Fen
import Game.Chess.TH

{-
  TODO: use newtype ideally - having trouble here as Vector Word64 and Vector Bitboard
  are not exactly the same.
 -}
type Bitboard = Word64

{-
  pawns and kings might not be considered pieces,
  but let's not make it more complicated than needed.
 -}
data PieceType
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Enum, Eq, Ord, Bounded)

pieceTypeSize :: Int
pieceTypeSize = fromEnum (maxBound @PieceType) - fromEnum (minBound @PieceType) + 1

{-
  A halfboard contains exactly 6 elements for 6 piece types.
 -}
newtype Halfboard = Halfboard (VU.Vector Bitboard)

hbAt :: Halfboard -> PieceType -> Bitboard
hbAt (Halfboard hb) pt = hb VU.! fromEnum pt

emptyHalfboard :: Halfboard
emptyHalfboard = Halfboard $ VU.replicate pieceTypeSize 0

{-
  (<white side>, <black side>)
 -}
newtype Board = Board (Halfboard, Halfboard)

data Color = White | Black

data Side = KingSide | QueenSide

main :: IO ()
main = pure ()
