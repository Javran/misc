module Puzzle where

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Solver

type Coord = (Int, Int)

-- the resulting pair is: LHS coefficients of the puzzle matrix,
-- and a nested list of Coords xs, such that, when flatten this list,
-- its i-th element is the i-th variable of the equation.
sqCoords :: Int -> ([[Int]], [[Coord]])
sqCoords sz = (fmap mkEqn allCoords, nestedAllCoords)
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

    nestedAllCoords = [[(r, c) | c <- [0 .. sz -1]] | r <- [0 .. sz -1]]
    allCoords = concat nestedAllCoords
    allCoords' = S.fromDistinctAscList allCoords
    surrounding (x, y) = do
      i <- [x -1 .. x + 1]
      j <- [y -1 .. y + 1]
      let c = (i, j)
      c <$ guard (S.member c allCoords')

-- https://www.redblobgames.com/grids/hexagons/#coordinates-cube
type CubeCoord = (Int, Int, Int)

hexCoords sz = (fmap mkEqn allCoords, nestedAllCoords)
  where
    mx = sz -1
    nestedAllCoords :: [[CubeCoord]]
    nestedAllCoords =
      [[(x, y, z) | y <- [3, 2 .. (-3 - z)], let x = - y - z] | z <- [- mx .. 0]]
        <> [[(x, y, z) | x <- [-3 .. 3 - z], let y = - x - z] | z <- [1 .. mx]]
    allCoords = concat nestedAllCoords
    allCoords' = S.fromList allCoords
    surrounding c@(x, y, z) =
      c :
      filter
        (`S.member` allCoords')
        [ (x, y + 1, z -1)
        , (x, y -1, z + 1)
        , (x + 1, y, z -1)
        , (x -1, y, z + 1)
        , (x + 1, y -1, z)
        , (x - 1, y + 1, z)
        ]
    coordEqns :: M.Map CubeCoord [CubeCoord]
    coordEqns = M.fromList $ fmap (\c -> (c, surrounding c)) allCoords
    mkEqn :: CubeCoord -> [Int]
    mkEqn c =
      -- TODO: we might want to do something more efficient than this.
      fmap (\c' -> if c' `elem` xs then 1 else 0) allCoords
      where
        xs :: [CubeCoord]
        xs = coordEqns M.! c

hexExample =
  let inp =
        (fmap . fmap)
          (\v -> (1 - v) `mod` 6)
          [ [1, 2, 4, 5]
          , [5, 5, 3, 2, 4]
          , [1, 1, 3, 3, 2, 4]
          , [5, 6, 2, 4, 2, 2, 6]
          , [5, 1, 3, 5, 2, 6]
          , [1, 4, 2, 1, 3]
          , [4, 5, 6, 4]
          ]

      (matLhs, _) = hexCoords 4
   in zipWith (\xs rhs -> foldr (:) [rhs] xs) matLhs (concat inp)

main :: IO ()
main = do
  let fallback _ eqns =
        {-
          TODO:
          - div by gcd then fill in underdetermined.
          - partition by whether hd is zero
          - Q: but what if hd column are nothing but zero?
            + need to insert one row with [1 0 0 0 ... 0]
            + if there are all-zero rows, drop one
            + otherwise just take diff.
            + or shuffle a non-zero row to front, solve it and shuffle back?
        -}
        traceShow ("UNDER", eqns) $ Left Underdetermined
      hexSplit = splitPlaces [4 :: Int, 5, 6, 7, 6, 5, 4]
      ud =
        [ [0, 0, 0, 0, 0, 0]
        , [3, 0, 0, 3, 0, 3]
        , [0, 0, 0, 0, 0, 0]
        , [3, 0, 0, 3, 0, 3]
        , [0, 0, 0, 0, 0, 0]
        ]
      r = solveMat' fallback 6 hexExample
  print (foldr gcd 6 (concat ud))

pprLhsMat :: [[Int]] -> [String]
pprLhsMat = fmap pprLine
  where
    pprLine coeffs = intercalate " + " (zipWith combine coeffs cs) <> " = ?"
      where
        combine coeff ch = case coeff of
          1 -> [ch]
          0 -> " "
          _ -> error "unexpected"
        cs = filter isAlpha ['A' ..]

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
