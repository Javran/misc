{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.List
import Data.List.Split
import qualified Gaussian as G
import Parser
import qualified Puzzle as Pz
import Solver
import System.Environment
import System.Exit

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
main =
  getArgs >>= \case
    "stdin" : _ -> do
      raw <- getContents
      let parsed = fromRawString raw
      case parsed of
        Just Puzzle {opMod = 6, pzType = PHexagon 4, grid} -> do
          let inp =
                (fmap . fmap)
                  (\v -> (- v) `mod` 6)
                  grid
              (matLhs, _) = Pz.hexCoords 4
              mat = zipWith (\xs rhs -> foldr (:) [rhs] xs) matLhs (concat inp)
          case solveMatOne 6 mat of
            Left e -> print e
            Right xs ->
              mapM_ (putStrLn . unwords . fmap show) $ Pz.hexSplit xs
        _ -> error "TODO"
    xs -> do
      putStrLn $ "Unknown: " <> show xs
      exitFailure
