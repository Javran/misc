{-
  Solver for https://en.wikipedia.org/wiki/Takuzu

  Credit to game "0h h1" by Q42 for introducting me to this game.
  If you see a lot of mention of color blue and red instead
  of 0 and 1, that's why.

  The decision that blue=0 and red=1 is made because 'b' < 'r' alphabetically,
  this allows us to accept any raw table with ' ' and two more other characters
  in it, and assign 0,1 to these two characters in order.
  e.g. "101 " will result in the exact same input representation as "rbr ".
 -}
{-# LANGUAGE
    PartialTypeSignatures
  , RecordWildCards
  #-}
module Main
  ( main
  ) where

import Control.Monad.Primitive

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Set as S

exampleRaw :: [] ([] Char)
exampleRaw =
  [ "    rr  br  "
  , "      r  b b"
  , "  br    r  b"
  , " r r        "
  , "b     r b b "
  , "  b b     b "
  , " r  br  r   "
  , "r    r      "
  , "r   r   bb  "
  , "  r     b   "
  , "      r   rb"
  , "  r  r      "
  ]

type Cell = Bool

cBlue, cRed :: Cell
[cBlue, cRed] = [False, True]

-- NOTE: more compact representation can be achieved through a bit vector impl,
-- or just Word64 or something shorter
-- since we've never planned to solve a large scale problem
-- by preprocessing a large table.
-- but for now let's focus on correctness first.
type CompleteLine = VU.Vector Cell

type Coord = (Int, Int) -- (row, col), 0-based

data Board vec
  = Board
  { bdHalfLen :: Int -- n, half of the total length of the board.
  , bdTodos :: S.Set Coord -- coords of those not yet filled cells.
  , bdCells :: vec (Maybe Cell) -- vector of size n * n, use Data.Ix for indexing.
    -- candidates that can be filled to that row, size=n
  , bdRowCandidates :: vec (S.Set CompleteLine)
    -- same as bdRowCandidates but for columns.
  , bdColCandidates :: vec (S.Set CompleteLine)
  }

mkBoard :: PrimMonad m => Int -> [[Maybe Cell]] -> m (Board _)
mkBoard n rawMatPre = do
  let bdHalfLen = n
      -- making it n x n, filling in Nothing.
      rawMat =
        take n $
          fmap (take n . (<> repeat Nothing)) rawMatPre
          <> repeat (replicate n Nothing)
      bdCellsPre = V.fromListN (n*n) (concat rawMat)
      bdTodos = S.fromList $ do
        let coords = [(r,c) | r <- [0..n], c <- [0..n]]
        (coord, Nothing) <- zip coords (concat rawMat)
        pure coord
  bdCells <- V.unsafeThaw bdCellsPre
  pure Board {..}

{-
  Total number of valid cell placements in a single line
  can be calculated following https://oeis.org/A177790

  "Digits" are generated backwards since that's the most efficient way,
  this does not effect correctness given this problem's symmetric nature.
 -}
genLineAux :: Int -> Int -> [Bool] -> [] [Bool]
genLineAux 0 0 xs = [xs]
genLineAux rCount bCount xs = case xs of
    x0:x1:_
      | x0 == x1 ->
        if x0
          then newR
          else newB
    _ -> newB <> newR
  where
    newB = do
      True <- [bCount > 0]
      genLineAux rCount (bCount-1) (cRed : xs)
    newR = do
      True <- [rCount > 0]
      genLineAux (rCount-1) bCount (cBlue : xs)

mkTable :: Int -> [] CompleteLine
mkTable n = VU.fromListN (n+n)  <$> genLineAux n n []

-- match a complete line against a partial line.
matchLine :: CompleteLine -> [Maybe Cell] -> Bool
matchLine cl inp = and $ zipWith matches (VU.toList cl) inp
  where
    matches _ Nothing = True
    matches b0 (Just b1) = b0 == b1

-- extra common features from lines.
-- input must be non-empty, and all elements are assumed to have the same length.
summarizeLines :: [] CompleteLine -> [] (S.Set Cell)
summarizeLines ls = extractInd <$> [0 .. size-1]
  where
    extractInd :: Int -> S.Set Cell
    extractInd i = S.fromList ((VU.! i) <$> ls)
    size = VU.length (head ls)

main :: IO ()
main = do
  let tbl = mkTable 2
      ln = [Just cBlue, Just cRed, Nothing, Nothing]
      r0 = filter (flip matchLine ln) tbl
      r1 = summarizeLines r0
  print r0
  print r1
