module Main
  ( main
  ) where

{-
  Implmentation of Reversi (https://en.wikipedia.org/wiki/Reversi)
 -}

import qualified Data.Map.Strict as M

type Coord = (Int {- row -}, Int {- col -}) -- note that this is 0-based index
type Disk = Bool -- False for light, True for dark

type Board = M.Map Coord Disk

initBoard :: Board
initBoard = M.fromList [((3,3),False), ((4,4),False), ((3,4),True), ((4,3),True)]

renderBoard :: Board -> [String]
renderBoard bd =
    topAxis
    : zipWith (:)
        (concat ((\v -> ' ': show v) <$> [1..8 :: Int]) <> " ")
        rendered
  where
    topAxis = ' ' : (['a'..'h'] >>= \c -> [' ',c])
    -- rendered board without axis
    rendered =
      zipWith (:)
        leftBorder
        (topBorder : ([0..7] >>= renderRow))

    dark = 'X'
    light = 'O'
    leftBorder = '┌' : concat (replicate 7 "│├") <> "│└"
    topBorder = concat (replicate 7 "─┬") <> "─┐"
    renderRow :: Int -> [String]
    renderRow r = [firstLine, secondLine]
      where
        (cross, cross') = if r == 7 then ('┴','┘') else ('┼','┤')
        firstLine :: String
        firstLine = concatMap (\c -> render c : "│") [0..7]
          where
            render c = case bd M.!? (r,c) of
              Nothing -> ' '
              Just d -> if d then dark else light
        secondLine :: String
        secondLine = concat (replicate 7 ['─',cross]) <> ['─',cross']

main :: IO ()
main = mapM_ putStrLn (renderBoard initBoard)
