{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module Lib
  ( main
  )
where

import Data.List.Split
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

{-
  Word8 but only 0~63 are valid.
 -}
newtype LinearCoord = LinearCoord Word8

{-
  TODO: there must be a better way, right?
 -}
[ [a1, b1, c1, d1, e1, f1, g1, h1]
  , [a2, b2, c2, d2, e2, f2, g2, h2]
  , [a3, b3, c3, d3, e3, f3, g3, h3]
  , [a4, b4, c4, d4, e4, f4, g4, h4]
  , [a5, b5, c5, d5, e5, f5, g5, h5]
  , [a6, b6, c6, d6, e6, f6, g6, h6]
  , [a7, b7, c7, d7, e7, f7, g7, h7]
  , [a8, b8, c8, d8, e8, f8, g8, h8]
  ] = chunksOf 8 $ LinearCoord <$> [0 .. 63]

main :: IO ()
main = pure ()
