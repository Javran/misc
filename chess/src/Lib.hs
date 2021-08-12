{-# LANGUAGE DerivingVia #-}

module Lib
  ( main
  )
where

import qualified Data.Vector.Unboxed as VU
import Data.Word

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
  deriving (Enum, Eq, Ord)

{-
  A halfboard contains exactly 6 elements for 6 piece types.
 -}
newtype Halfboard = Halfboard (VU.Vector Bitboard)

hbAt :: Halfboard -> PieceType -> Bitboard
hbAt (Halfboard hb) pt = hb VU.! fromEnum pt

{-
  (<white side>, <black side>)
 -}
newtype Board = Board (Halfboard, Halfboard)

main :: IO ()
main = pure ()
