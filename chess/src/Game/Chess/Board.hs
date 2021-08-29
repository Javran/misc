{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Game.Chess.Board where

import Control.Monad
import Control.Monad.ST.Strict
import Data.Bits
import Data.Maybe
import qualified Data.Vector.Fixed as VF
import qualified Data.Vector.Fixed.Boxed as VFB
import qualified Data.Vector.Fixed.Mutable as VFM
import Debug.Trace
import Game.Chess.Bitboard
import Game.Chess.Coord
import Game.Chess.Fen
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
emptyHb = VF.replicate $! (Bitboard 0)

hbAt :: Halfboard -> PieceType -> Bitboard
hbAt hb pt = hb VF.! fromEnum pt

bd =
  let Right r = fenParseTest
   in fromPlacement (placement r)

{-
  (<white side>, <black side>)
 -}
type Board = (Halfboard, Halfboard)

-- fromPlacement :: Placement -> Board
fromPlacement ps2d = runST $ do
  whiteHb <- VFM.new
  blackHb <- VFM.new
  forM_ [0 .. 5] $ \i -> do
    VFM.write whiteHb i (Bitboard 0)
    VFM.write blackHb i (Bitboard 0)
  let pairs :: [] (Coord, (Color, PieceType))
      pairs =
        catMaybes
          (zipWith
             (\mcp c -> (c,) <$> mcp)
             (concat $ VF.toList $ fmap VF.toList ps2d)
             (concat fenCoords))
  forM_ pairs $ \e@(coord, (c, pt)) -> do
    let hb = case c of
          White -> whiteHb
          Black -> blackHb
        pInd = fromEnum pt
    Bitboard v <- traceShow e $ VFM.read hb pInd
    VFM.write hb (traceShow v pInd) $! Bitboard (v .|. toBit coord)
  (w :: Halfboard) <- VFM.unsafeFreeze whiteHb
  (b :: Halfboard) <- VFM.unsafeFreeze blackHb
  pure b
