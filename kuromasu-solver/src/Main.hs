module Main
  ( main
  ) where

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

main :: IO ()
main = pure ()
