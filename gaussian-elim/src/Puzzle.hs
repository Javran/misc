module Puzzle where

import Control.Monad
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Coord = (Int, Int)

-- the resulting pair is: LHS coefficients of the puzzle matrix,
-- and a list of Coords xs, such that its i-th element is the i-th variable of the equation.
sqCoords :: Int -> ([[Int]], [Coord])
sqCoords sz = (fmap mkEqn allCoords, allCoords)
  where
    mkEqn :: Coord -> [Int]
    mkEqn c =
      -- TODO: we might want to do something more efficient than this.
      fmap (\c' -> if c' `elem` xs then 1 else 0) allCoords
      where
        xs :: [Coord]
        xs = coordEqns M.! c

    coordEqns :: M.Map Coord [Coord]
    coordEqns = M.fromDistinctAscList $ fmap (\c -> (c, surrounding c)) allCoords

    coordsToLinears :: M.Map Coord Int
    coordsToLinears = M.fromDistinctAscList $ zip allCoords [0 ..]

    allCoords = [(i, j) | i <- [0 .. sz -1], j <- [0 .. sz -1]]
    allCoords' = S.fromDistinctAscList allCoords
    surrounding (x, y) = do
      i <- [x -1 .. x + 1]
      j <- [y -1 .. y + 1]
      let c = (i, j)
      c <$ guard (S.member c allCoords')

pprLhsMat :: [[Int]] -> [String]
pprLhsMat = fmap pprLine
  where
    pprLine coeffs = intercalate " + " (zipWith combine coeffs cs) <> " = ?"
      where
        combine coeff ch = case coeff of
          1 -> [ch]
          0 -> " "
          _ -> error "unexpected"
        cs = ['A'..]

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
