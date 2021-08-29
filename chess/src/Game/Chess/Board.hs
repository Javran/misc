{-# LANGUAGE DataKinds #-}

module Game.Chess.Board where

import qualified Data.Vector.Fixed as VF
import qualified Data.Vector.Fixed.Boxed as VFB
import Game.Chess.Bitboard
import Game.Chess.Types

{-
  There are multiple ways to represent a board with Bitboard as elemenets:

  - we can do a vector of 12 elements
  - or a pair of vectors of 6 elements in each

  And when it comes to the choice of boxed / unboxed vector, we can do both.

  For now performance doesn't matter, so let's just pick one and stick with it
  unless there are blocking problems along the way.

  I choose to use a pair of boxed vectors so to allow more sharing.
 -}
type Halfboard = VFB.Vec 6 Bitboard

emptyHb :: Halfboard
emptyHb = VF.replicate (Bitboard 0)

hbAt :: Halfboard -> PieceType -> Bitboard
hbAt hb pt = hb VF.! fromEnum pt

{-
  (<white side>, <black side>)
 -}
type Board = (Halfboard, Halfboard)
