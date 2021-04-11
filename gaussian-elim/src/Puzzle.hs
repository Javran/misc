module Puzzle where

import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Coord = (Int, Int)

sqCoords :: Int -> M.Map Coord [Coord]
sqCoords sz = M.fromDistinctAscList $ fmap (\c -> (c, surrounding c)) allCoords
  where
    allCoords = [(i, j) | i <- [0 .. sz -1], j <- [0 .. sz -1]]
    allCoords' = S.fromDistinctAscList allCoords
    surrounding (x, y) = do
      i <- [x -1 .. x + 1]
      j <- [y -1 .. y + 1]
      let c = (i, j)
      c <$ guard (S.member c allCoords')

coords :: [Coord]
coords = [(i, j) | i <- [0 .. 3], j <- [0 .. 3]]

vars :: M.Map Coord Char
vars = M.fromList $ zip coords (concat vs)
  where
    vs = ["ABCD", "EFGH", "IJKL", "MNOP"]

targets :: M.Map Coord Int
targets = M.fromList $ zip coords $ concatMap (fmap tr) initVals
  where
    tr x = (4 - x) `rem` 4
    initVals = [[3, 1, 3, 2], [0, 0, 3, 3], [3, 2, 3, 0], [3, 0, 2, 0]]

{-
[ [2, 2, 3, 3]
, [3, 2, 3, 3]
, [1, 0, 3, 0]
, [3, 2, 2, 0]
]-}

type Eqn = ([] Char, Int)

type MatRow = [Int]

mkRow :: Eqn -> MatRow
mkRow (lhs, rhs) = fmap tr ['A' .. 'P'] <> [rhs]
  where
    tr ch = if ch `elem` lhs then 1 else 0

eqn :: Coord -> Eqn
eqn c@(x, y) = (vs, targets M.! c)
  where
    vs = do
      dx <- [-1 .. 1]
      dy <- [-1 .. 1]
      let coord = (x + dx, y + dy)
      Just v <- pure $ vars M.!? coord
      pure v

pprEqn :: Eqn -> String
pprEqn (xs, v) = intercalate " + " (fmap tr ['A' .. 'P']) <> " = " <> show v
  where
    tr ch = if S.member ch cs then [ch] else " "
    cs = S.fromList xs

-- main :: IO ()
-- main = mapM_ (putStrLn . pprEqn . eqn) coords
