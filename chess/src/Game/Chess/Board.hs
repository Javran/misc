{-# LANGUAGE DataKinds #-}

module Game.Chess.Board where

import qualified Data.Vector.Fixed as VF
import qualified Data.Vector.Fixed.Boxed as VFB
import Game.Chess.Bitboard
import Game.Chess.Types

type Halfboard = VFB.Vec 6 Bitboard

emptyHb :: Halfboard
emptyHb = VF.replicate (Bitboard 0)

hbAt :: Halfboard -> PieceType -> Bitboard
hbAt hb pt = hb VF.! fromEnum pt

{-
  (<white side>, <black side>)
 -}
type Board = (Halfboard, Halfboard)
