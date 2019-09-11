module Main
  ( main
  ) where

import qualified Data.Map.Strict as IM
import qualified Data.Set as S

{-
  Use Willson's algorithm to generate mazes.
 -}

-- initial set consists of all nodes.
initMaze :: Int -> Int -> [] (Int, Int)
initMaze rows cols = (,) <$> [0..rows-1] <*> [0..cols-1]

main :: IO ()
main = pure ()
