{-# LANGUAGE MultiWayIf #-}
module Main
  ( main
  ) where

import Control.Monad.State
import Control.Monad
import System.Random.TF
import System.Random.TF.Instances
import qualified Data.Map.Strict as IM
import qualified Data.Set as S

{-
  Use Willson's algorithm to generate mazes.
 -}

type Coord = (Int, Int)

-- undirected, Edge a b where a <= b
data Edge = Edge Coord Coord

mkEdge :: Coord -> Coord -> Edge
mkEdge a b = if a > b then Edge b a else Edge a b

-- initial set consists of all nodes.
initMaze :: Int -> Int -> S.Set Coord
initMaze rows cols = S.fromList $ (,) <$> [0..rows-1] <*> [0..cols-1]

-- cellSet: set of nodes contained in the maze
-- curPathRev: current path in reversed order.
-- return: Left c if path later than c need to be erased, Right if a random walk is found.
randomWalk :: Int -> Int -> S.Set Coord -> [Coord] -> State TFGen (Either Coord [Coord])
randomWalk rows cols cellSet curPathRev = do
  -- INVARIANT: always non-empty.
  let (r,c):_ = curPathRev
  g <- get
  let alts = do
        (dr,dc) <- [(-1,0),(1,0),(0,-1),(0,1)]
        let (r'',c'') = (r + dr, c + dc)
        guard $ r'' >= 0 && r'' < rows && c'' >= 0 && c'' < cols
        pure (r'',c'')
      altsR = (0, length alts - 1)
      (altInd, g') = randomR altsR g
      cell = alts !! altInd
  put g'
  if
    | S.member cell cellSet ->
        -- next step walks into the maze, we are done.
        pure (Right $ cell:curPathRev)
    | elem cell curPathRev ->
        -- walks into current, need elimination
        pure (Left cell)
    | otherwise -> do
        result <- randomWalk rows cols cellSet (cell:curPathRev)
        case result of
          Right _ -> pure result
          Left cell' ->
            if cell' == cell
              then
                -- retry again
                randomWalk rows cols cellSet curPathRev
              else
                -- current cell need to be erased, keep back tracking.
                pure result

main :: IO ()
main = pure ()
