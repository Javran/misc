{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module Board where

import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data PuzzleShape
  = Square
  | Hexagon

class CoordSystem (k :: PuzzleShape) where
  type Coord k
  getCoords :: forall p. p k -> Int -> ([[Int]], [[Coord k]])
  surrounding :: forall p. p k -> Coord k -> [Coord k]

instance CoordSystem 'Square where
  type Coord 'Square = (Int, Int)
  getCoords _ = sqCoords
  surrounding _ = sqSurrounding

sqCoords sz = (fmap mkEqn allCoords, nestedAllCoords)
  where
    mkEqn c =
      fmap (\c' -> if c' `elem` xs then 1 else 0) allCoords
      where
        xs = coordEqns M.! c

    coordEqns = M.fromDistinctAscList $ fmap (\c -> (c, surrounding' c)) allCoords

    nestedAllCoords = [[(r, c) | c <- [0 .. sz -1]] | r <- [0 .. sz -1]]
    allCoords = concat nestedAllCoords
    allCoords' = S.fromDistinctAscList allCoords
    surrounding' (x, y) = do
      c <- sqSurrounding (x, y)
      c <$ guard (S.member c allCoords')

sqSurrounding (x, y) = do
  i <- [x -1 .. x + 1]
  j <- [y -1 .. y + 1]
  pure (i, j)

type CubeCoord = (Int, Int, Int)

instance CoordSystem 'Hexagon where
  type Coord 'Hexagon = CubeCoord
  getCoords _ = hexCoords
  surrounding _ = hexSurrounding

hexCoords :: Int -> ([[Int]], [[CubeCoord]])
hexCoords sz = (fmap mkEqn allCoords, nestedAllCoords)
  where
    mx = sz -1
    nestedAllCoords :: [[CubeCoord]]
    nestedAllCoords =
      [[(x, y, z) | y <- [3, 2 .. (-3 - z)], let x = - y - z] | z <- [- mx .. 0]]
        <> [[(x, y, z) | x <- [-3 .. 3 - z], let y = - x - z] | z <- [1 .. mx]]
    allCoords = concat nestedAllCoords
    allCoords' = S.fromList allCoords
    surrounding' c@(x, y, z) =
      filter
        (`S.member` allCoords')
        $ hexSurrounding c
    coordEqns :: M.Map CubeCoord [CubeCoord]
    coordEqns = M.fromList $ fmap (\c -> (c, surrounding' c)) allCoords
    mkEqn :: CubeCoord -> [Int]
    mkEqn c =
      fmap (\c' -> if c' `elem` xs then 1 else 0) allCoords
      where
        xs :: [CubeCoord]
        xs = coordEqns M.! c

hexSurrounding c@(x, y, z) =
  c :
  [ (x, y + 1, z -1)
  , (x, y -1, z + 1)
  , (x + 1, y, z -1)
  , (x -1, y, z + 1)
  , (x + 1, y -1, z)
  , (x - 1, y + 1, z)
  ]
