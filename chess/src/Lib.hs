module Lib
  ( main
  )
where

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

main :: IO ()
main = pure ()
