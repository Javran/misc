module Main
  ( main
  ) where

{-
  Implmentation of Reversi (https://en.wikipedia.org/wiki/Reversi)
 -}

import qualified Data.Map.Strict as M

type Coord = (Int {- row -}, Int {- col -}) -- note that this is 0-based index
type Disk = Bool -- False for light, True for dark

type Board = M.Map Coord Disk

initBoard :: Board
initBoard = M.fromList [((3,3),False), ((4,4),False), ((3,4),True), ((4,3),True)]

main :: IO ()
main = pure ()
