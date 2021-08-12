module Lib
  ( main
  )
where

import qualified Data.Vector.Unboxed as VU
import Data.Word

newtype Bitboard
  = Bitboard Word64

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
  deriving (Enum, Eq, Ord)

{-
  A halfboard contains exactly 6 elements for 6 piece types.
 -}
newtype Halfboard = Halfboard (VU.Vector Bitboard)

{-
  (<white side>, <black side>)
 -}
newtype Board = Board (Halfboard, Halfboard)

main :: IO ()
main = pure ()
