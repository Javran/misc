{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  surrounding :: forall p. p k -> Coord k -> [Coord k]
  shapedCoords :: forall p. p k -> Int -> [[Coord k]]

instance CoordSystem 'Square where
  type Coord 'Square = (Int, Int)
  surrounding _ (x, y) = do
    i <- [x -1 .. x + 1]
    j <- [y -1 .. y + 1]
    pure (i, j)

  shapedCoords _ sz = [[(r, c) | c <- [0 .. sz -1]] | r <- [0 .. sz -1]]

type CubeCoord = (Int, Int, Int)

instance CoordSystem 'Hexagon where
  type Coord 'Hexagon = CubeCoord
  surrounding _ c@(x, y, z) =
    c :
    [ (x, y + 1, z -1)
    , (x, y -1, z + 1)
    , (x + 1, y, z -1)
    , (x -1, y, z + 1)
    , (x + 1, y -1, z)
    , (x - 1, y + 1, z)
    ]

  shapedCoords _ sz =
    [[(x, y, z) | y <- [3, 2 .. (-3 - z)], let x = - y - z] | z <- [- mx .. 0]]
      <> [[(x, y, z) | x <- [-3 .. 3 - z], let y = - x - z] | z <- [1 .. mx]]
    where
      mx = sz -1

gCoords
  :: forall p cs.
  (CoordSystem cs, Ord (Coord cs))
  => p cs
  -> Int
  -> ([[Int]], [[Coord cs]])
gCoords ty sz = (fmap mkEqn allCoords, nestedAllCoords)
  where
    nestedAllCoords :: [[Coord cs]]
    nestedAllCoords = shapedCoords ty sz
    allCoords = concat nestedAllCoords
    allCoords' = S.fromList allCoords
    surrounding' c =
      filter
        (`S.member` allCoords')
        $ surrounding ty c
    coordEqns :: M.Map (Coord cs) [Coord cs]
    coordEqns = M.fromList $ fmap (\c -> (c, surrounding' c)) allCoords
    mkEqn c =
      fmap (\c' -> if c' `elem` xs then 1 else 0) allCoords
      where
        xs = coordEqns M.! c
