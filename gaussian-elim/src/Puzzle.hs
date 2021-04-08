module Puzzle where

import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Coord = (Int, Int)

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
    initVals =
      [ [2, 2, 3, 3]
      , [3, 2, 3, 3]
      , [1, 0, 3, 0]
      , [3, 2, 2, 0]
      ]

type Eqn = ([] Char, Int)

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
pprEqn (xs, v) = intercalate " + " (fmap tr ['A'..'P']) <> " = " <> show v
 where
   tr ch = if S.member ch cs then [ch] else " "
   cs = S.fromList xs

-- main :: IO ()
-- main = mapM_ (putStrLn . pprEqn . eqn) coords
