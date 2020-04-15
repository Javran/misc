module Main
  ( main
  ) where

import Control.Monad
import Data.List

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- TODO: specify rows and cols, and colors
exampleRaw :: [String]
exampleRaw =
  [ "? ? 3 2 ? ? r 1 1"
  , "r 4 ? r ? ? ? ? ?"
  , "? ? ? ? ? ? 7 ? ?"
  , "? ? ? ? 8 8 ? 9 8"
  , "5 ? ? 6 ? ? ? ? 4"
  , "5 r ? ? ? r 5 ? ?"
  , "? ? 6 ? 3 ? ? ? r"
  , "? r 5 ? r ? 4 r 1"
  , "? ? ? 3 ? 5 ? 3 ?"
  ]

type Cell = Bool

cBlue, cRed :: Cell
[cBlue, cRed] = [False, True]

type Coord = (Int, Int) -- (<row>, <col>), 0-based.

data Board
  = Board
    { bdDims :: (Int, Int) -- (rows, cols)
    , bdTodos :: S.Set Coord -- not yet filled cells
    , bdCells :: M.Map Coord Cell -- known cells
      -- Every unsolved number cell is listed here.
      -- each of the value is a list of possible "overlaps"
      -- with the board. This allows us to:
      -- - answer the question of what's common in all possible candidates
      -- - eliminate candidates that are not possible.
    , bdCandidates :: M.Map Coord [M.Map Coord Cell]
    }

{-
  The following type defines a building block for building "blueprint"s given a number
  and the original coordinate.

  Assume a cell is at (0,0), Placement 1 2 3 0 will mean the following setup:

  - (0,0): blue
  - (-1,0): blue, (-2,0): red
  - (0,1) & (0,2): blue, (0,3): red
  - (1,0) & (2,0) & (3,0): blue, (4,0): red
  - (0,-1): red

  note that we have negative numbers, which is fine since we are only building the blueprint,
  but the generated candidates (i.e. sets of coord-color pairs) will be less at runtime.

  also note that there's another way of eliminating an candidate: if it's dimension is too long
  to be fit in a board (e.g. a 9x9 board can have maximum number of 16,
  but a 1x17 Placement is simply impossible to fit). This kind of elimination can be done when building up the blueprint.

 -}
data Placement =
  Placement Int Int Int Int {- up, right, down, left. in this order -}
  deriving Show

{-
  pick up items in that order. one item can be pick up multiple times.
 -}
pickInOrder' :: [a] -> [] (a,[a])
pickInOrder' = fmap (\(x:xs) -> (x,x:xs)) . init . tails

gen _ _ 0 cur = pure [cur]
gen (rows, cols) mods todoCount cur = do
  (f, mods') <- pickInOrder' mods
  let cur'@(Placement u r d l) = f cur
  guard $ u + d < rows && l + r < cols
  gen (rows, cols) mods' (todoCount-1) cur'

main :: IO ()
main = do
  let mods =
        [ \(Placement u r d l) -> Placement (u+1) r d l
        , \(Placement u r d l) -> Placement u (r+1) d l
        , \(Placement u r d l) -> Placement u r (d+1) l
        , \(Placement u r d l) -> Placement u r d (l+1)
        ]
  mapM_ print $ gen (9,9) mods 16 (Placement 0 0 0 0)
