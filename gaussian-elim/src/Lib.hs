{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.List
import Data.List.Split
import qualified Puzzle as Pz
import Solver

mainDemo :: IO ()
mainDemo = do
  let input :: [[Int]]
      input =
        [ [3, 4, 7, 2]
        , [4, 11, 2, 8]
        , [16, 7, 3, 3]
        ]
      [a, b, c] = [4, 15, 7 :: Int]
  2 <- pure $ (a * 3 + b * 4 + c * 7) `rem` 17
  8 <- pure $ (a * 4 + b * 11 + c * 2) `rem` 17
  3 <- pure $ (a * 16 + b * 7 + c * 3) `rem` 17
  putStrLn "input:"
  mapM_ print input
  case solveMat 17 input of
    Right sols -> putStrLn $ "Solution: " <> show sols
    Left (NoMultInv i) ->
      putStrLn $
        "Cannot solve equations as " <> show i <> " does not have a multiplicative inverse."
    Left Underdetermined ->
      putStrLn
        "Cannot solve equations, underdetermined."

main :: IO ()
main = do
  let mat :: [[Int]]
      mat = map (Pz.mkRow . Pz.eqn) Pz.coords
      Right r = solveMat 6 Pz.hexExample
  -- print (solveMat 4 mat)
  -- Pz.main
  -- mapM_ print (zipWith (\pd xs -> replicate pd 0 <> xs) [0..] z)
  -- print z
  -- let sol = reverse $ unfoldr (solveStep 6) ([], reverse u)
  mapM_ print (splitPlaces [4 :: Int, 5, 6, 7, 6, 5, 4] r)
  pure ()
