{-# LANGUAGE MultiWayIf #-}
module Main
  ( main
  ) where

import Control.Monad.State.Strict
import System.Random.TF
import System.Random.TF.Instances
import qualified Data.Set as S

{-
  Use Willson's algorithm to generate mazes.
 -}

type Coord = (Int, Int)

-- undirected, Edge a b where a <= b
data Edge = Edge Coord Coord
  deriving Show

mkEdge :: Coord -> Coord -> Edge
mkEdge a b = if a > b then Edge b a else Edge a b

-- initial set consists of all nodes.
initMaze :: Int -> Int -> S.Set Coord
initMaze rows cols = S.fromList $ (,) <$> [0..rows-1] <*> [0..cols-1]

genNext :: Random a => (a, a) -> State TFGen a
genNext range =
  state $ \g -> let (v, g') = randomR range g in (v, g')

pickOneFromSet :: Ord a => S.Set a -> State TFGen (a, S.Set a)
pickOneFromSet s = do
  ind <- genNext (0, S.size s - 1)
  let x = S.toAscList s !! ind
  pure (x, S.delete x s)

-- cellSet: set of nodes contained in the maze
-- curPathRev: current path in reversed order.
-- return: Left c if path later than c need to be erased, Right if a random walk is found.
randomWalk :: Int -> Int -> S.Set Coord -> [Coord] -> State TFGen (Either Coord [Coord])
randomWalk rows cols cellSet curPathRev = do
  -- INVARIANT: always non-empty.
  let (r,c):_ = curPathRev
  let alts = do
        (dr,dc) <- [(-1,0),(1,0),(0,-1),(0,1)]
        let (r'',c'') = (r + dr, c + dc)
        guard $ r'' >= 0 && r'' < rows && c'' >= 0 && c'' < cols
        pure (r'',c'')
      altsR = (0, length alts - 1)
  altInd <- genNext altsR
  let cell = alts !! altInd
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
                -- current cell need to be erased, keep backtracking.
                pure result

genMaze :: TFGen -> Int -> Int -> [] Edge
genMaze g rows cols = (`evalState` g) $ do
    let allCells = initMaze rows cols
    (x, initUnused) <- pickOneFromSet allCells
    fix (\loop curUnused curCellSet curEdges ->
      if S.null curUnused
        then pure curEdges
        else fix $ \loop2 -> do
          (c, curUnused') <- pickOneFromSet curUnused
          mPath <- randomWalk rows cols curCellSet [c]
          case mPath of
            Right path -> do
              let edges = zipWith mkEdge path (tail path)
                  cells = S.fromList path
                  curUnused'' = S.difference curUnused' cells
                  curCellSet' = S.union curCellSet cells
                  curEdges' = curEdges <> edges
              loop curUnused'' curCellSet' curEdges'
            Left {} -> loop2
        )
      initUnused
      (S.singleton x)
      []

main :: IO ()
main = do
  g <- newTFGen
  let es = genMaze g 5 6
  print es
