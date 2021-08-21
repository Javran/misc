{-# LANGUAGE TypeApplications #-}

module Game.Chess.Types where

import qualified Data.Vector.Unboxed as VU
import Data.Word

{-
  TODO: use newtype ideally - having trouble here as Vector Word64 and Vector Bitboard
  are not exactly the same.
 -}
type Bitboard = Word64

{-
  Mostly for documenting purpose, a list with exactly 8 elements
  (which is for now not checked).
 -}
type EightElems = []

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
  deriving (Enum, Eq, Ord, Bounded, Show)

{-
  A halfboard contains exactly 6 elements for 6 piece types.
 -}
newtype Halfboard = Halfboard (VU.Vector Bitboard)

hbAt :: Halfboard -> PieceType -> Bitboard
hbAt (Halfboard hb) pt = hb VU.! fromEnum pt

pieceTypeSize :: Int
pieceTypeSize = fromEnum (maxBound @PieceType) - fromEnum (minBound @PieceType) + 1

emptyHalfboard :: Halfboard
emptyHalfboard = Halfboard $ VU.replicate pieceTypeSize 0

{-
  (<white side>, <black side>)
 -}
newtype Board = Board (Halfboard, Halfboard)

data Color = White | Black deriving (Show)

data Side = KingSide | QueenSide deriving (Eq, Ord)
