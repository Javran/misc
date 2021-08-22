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

  TODO: we can probably use https://hackage.haskell.org/package/fixed-vector
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

  TODO: probably also use fixed-vector for this.
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

{-
  Plan to implement all legal moves:

  If we ignore absolute pins and checks

  - Pawn:

    + can move forward one square if the target square is empty
    + can move forward two squares if on home square
    + normal capture rule
    + en passant

  - Knight:

    + normal knight move rule, target square just need to not be occupied
      by anything of own color.

  - Bishop & Rook:

    + anything non-empty stops it
    + can capture if the blocking square is opposite color.

  - Queen: just pretend it's both a Bishop and a Rook.

  - King: should be easy to implement.

  Now to consider absolute pins and checks,
  we just need to exclude moves that would result in King being checked.

 -}

data Color = White | Black deriving (Show)

data Side = KingSide | QueenSide deriving (Eq, Ord, Show)
