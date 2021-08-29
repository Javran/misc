{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Game.Chess.Types where

import qualified Data.Vector as V
import qualified Data.Vector.Fixed as VF
import Game.Chess.Bitboard

{-
  A list with exactly 8 elements.
 -}
type EightElems = VF.VecList 8

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

  Note: We could also use fixed-vector for this
  but for now I'd prefer just the standard vector interface
  rather than having to dealing with type-level dimension stuff
  which is never going to change.

 -}
newtype Halfboard = Halfboard (V.Vector Bitboard)

hbAt :: Halfboard -> PieceType -> Bitboard
hbAt (Halfboard hb) pt = hb V.! fromEnum pt

pieceTypeSize :: Int
pieceTypeSize = fromEnum (maxBound @PieceType) - fromEnum (minBound @PieceType) + 1

emptyHalfboard :: Halfboard
emptyHalfboard = Halfboard $ V.replicate pieceTypeSize (Bitboard 0)

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
    + promotion

  - Knight:

    + normal knight move rule, target square just need to not be occupied
      by anything of own color.

  - Bishop & Rook:

    + anything non-empty stops it
    + can capture if the blocking square is opposite color.

  - Queen: just pretend it's both a Bishop and a Rook.

  - King:
    + normal moves
    + castling

  Now to consider absolute pins and checks,
  we just need to exclude moves that would result in King being checked.

 -}

data Color = White | Black deriving (Show)

data Side = KingSide | QueenSide deriving (Eq, Ord, Show)
