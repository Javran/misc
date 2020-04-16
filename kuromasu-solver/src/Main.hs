{-# LANGUAGE NamedFieldPuns #-}
module Main
  ( main
  ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import Data.Char
import Data.Semigroup

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

-- TODO: specify rows and cols, and colors
exampleRaw :: [String]
exampleRaw =
  [ "? ? 3 2 ? ? r 1 1"
  , "r 4 ? r ? ? ? ? ?"
  , "? ? ? ? ? ? 7 ? ?"
  , "? ? ? ? 8 8 ? 9 8"
  , "5 ? ? 6 ? ? ? ? 4"
  , "5 r ? ? ? r 5 ? ?"
  , "? ? 6 ? 3 ? ? ? r"
  , "? r 5 ? r ? 4 r 1"
  , "? ? ? 3 ? 5 ? 3 ?"
  ]

example :: ([(Coord, Cell)], [(Coord, Int)])
example =
    ( mapMaybe validCellOnly rawWithCoords
    , mapMaybe validHintOnly rawWithCoords
    )
  where
    coords = [(r,c) | r <- [0..8], c <- [0..8]]
    rawWithCoords = zip coords (concatMap words exampleRaw)
    validCellOnly (_, "?") = Nothing
    validCellOnly (coord, "r") = Just (coord, cRed)
    validCellOnly (coord, xs)
      | all isDigit xs = Just (coord, cBlue)
      | otherwise = error "unreachable (for now)"
    validHintOnly (coord, xs)
      | all isDigit xs = Just (coord, read xs)
      | otherwise = Nothing

type Cell = Bool

cBlue, cRed :: Cell
[cBlue, cRed] = [False, True]

type Coord = (Int, Int) -- (<row>, <col>), 0-based.

type Candidate = M.Map Coord Cell

data Board
  = Board
    { bdDims :: (Int, Int) -- (rows, cols)
    , bdTodos :: S.Set Coord -- not yet filled cells
    , bdCells :: M.Map Coord Cell -- known cells
      -- Every unsolved number cell is listed here.
      -- each of the value is a list of possible "overlaps"
      -- with the board. This allows us to:
      -- - answer the question of what's common in all possible candidates
      -- - eliminate candidates that are not possible.
    , bdCandidates :: M.Map Coord [M.Map Coord Cell]
    } deriving Show

{-
  The following type defines a building block for building "blueprint"s given a number
  and the original coordinate.

  Assume a cell is at (0,0), Placement 1 2 3 0 will mean the following setup:

  - (0,0): blue
  - (-1,0): blue, (-2,0): red
  - (0,1) & (0,2): blue, (0,3): red
  - (1,0) & (2,0) & (3,0): blue, (4,0): red
  - (0,-1): red

  note that we have negative numbers, which is fine since we are only building the blueprint,
  but the generated candidates (i.e. sets of coord-color pairs) will be less at runtime.

  also note that there's another way of eliminating an candidate: if it's dimension is too long
  to be fit in a board (e.g. a 9x9 board can have maximum number of 16,
  but a 1x17 Placement is simply impossible to fit). This kind of elimination can be done when building up the blueprint.

 -}
data Placement =
  Placement Int Int Int Int {- up, right, down, left. in this order -}
  deriving Show

{-
  pick up items in that order. one item can be pick up multiple times.
 -}
pickInOrder' :: [a] -> [] (a,[a])
pickInOrder' = fmap (\(x:xs) -> (x,x:xs)) . init . tails

gen _ _ 0 cur = [cur]
gen (rows, cols) mods todoCount cur = do
  (f, mods') <- pickInOrder' mods
  let cur'@(Placement u r d l) = f cur
  guard $ u + d < rows && l + r < cols
  gen (rows, cols) mods' (todoCount-1) cur'

{-
  Create an empty board with candidates populate by clues.
 -}
mkBoard :: (Int, Int) -> [(Coord, Int)] -> Board
mkBoard bdDims@(rows, cols) clues = Board
    { bdDims
    , bdTodos = S.fromList [(r,c) | r <- [0..rows-1], c <- [0..cols-1]]
    , bdCells = M.empty
    , bdCandidates = M.fromList $ uncurry mkCandidate <$> clues
    }
  where
    mkCandidate :: Coord -> Int -> (Coord, [M.Map Coord Cell])
    mkCandidate cCoord@(row,col) count =
        (cCoord, mapMaybe placementToCandidate ps)
      where
        -- generate initial possible placements
        -- without knowing the location of the center coord
        ps = gen bdDims mods count (Placement 0 0 0 0)
        placementToCandidate :: Placement -> Maybe (M.Map Coord Cell)
        placementToCandidate (Placement u r d l) = do
          let centerPair = (cCoord, cBlue)
              pUpCells =
                [ ((row-df, col), cBlue) | df <- [1..u]]
              pRightCells =
                [ ((row, col+df), cBlue) | df <- [1..r]]
              pDownCells =
                [ ((row+df, col), cBlue) | df <- [1..d]]
              pLeftCells =
                [ ((row, col-df), cBlue) | df <- [1..l]]
              pRedCells =
                [ (c, cRed)
                | c <-
                    [ (row-u-1, col)
                    , (row, col+r+1)
                    , (row+d+1, col)
                    , (row, col-l-1)
                    ]
                ]
              pairs = centerPair : concat [pUpCells, pRightCells, pDownCells, pLeftCells, pRedCells]
          let checkPair ((r',c'), color) =
                color == cRed || (r' >= 0 && r' < rows && c' >= 0 && c' < cols)
          guard $ all checkPair pairs
          pure $ M.fromList pairs

mods =
  [ \(Placement u r d l) -> Placement (u+1) r d l
  , \(Placement u r d l) -> Placement u (r+1) d l
  , \(Placement u r d l) -> Placement u r (d+1) l
  , \(Placement u r d l) -> Placement u r d (l+1)
  ]

pprBoard :: Board -> IO ()
pprBoard Board{bdDims, bdTodos, bdCells, bdCandidates} = do
  putStrLn $ "Board dimensions: " <> show bdDims
  putStrLn "++++ Board Begin"
  let (rows, cols) = bdDims
  forM_ [0..rows-1] $ \r -> do
    putStr "|"
    let coordToChar coord = case bdCells M.!? coord of
          Nothing -> ' '
          Just c -> if c == cBlue then 'B' else 'R'
    putStr ((\c -> coordToChar (r,c)) <$> [0..cols-1])
    putStrLn "|"
  putStrLn "---- Board End"
  putStrLn $ "Todos: " <> show (length bdTodos)
  putStrLn "Candidates:"
  forM_ (M.toAscList bdCandidates) $ \(coord, xs) -> do
    putStrLn $ "- " <> show coord <> ": " <> show (length xs)
    -- the following output is noisy. only enable when debugging.
    -- forM_ xs $ \cs -> pprCandidate "  " cs

pprCandidate :: String -> Candidate -> IO ()
pprCandidate padding cs =
  case NE.nonEmpty (M.keys cs) of
    Nothing -> putStrLn $ padding <> "<empty>"
    Just cs' -> do
      let getMinMax getter = sconcat $ fmap (((,) <$> Min <*> Max) . getter) cs'
          (Min rMin, Max rMax) = getMinMax fst
          (Min cMin, Max cMax) = getMinMax snd
          cGet coord = case cs M.!? coord of
            Nothing -> ' '
            Just c -> if c == cBlue then 'B' else 'R'
      putStrLn $ padding <> "Range: " <> show (rMin, cMin) <> " - " <> show (rMax, cMax)
      forM_ [rMin .. rMax] $ \r ->
        putStrLn $ padding <> [ cGet (r,c) | c <- [cMin..cMax] ]

updateCell :: Coord -> Cell -> Board -> Maybe Board
updateCell coord color Board{bdDims, bdTodos, bdCells, bdCandidates} = do
  guard $ coord `S.member` bdTodos
  let bdTodos' = S.delete coord bdTodos
      bdCells' = M.insert coord color bdCells
      checkAndElim :: Candidate -> Maybe Candidate
      checkAndElim cs = case cs M.!? coord of
        Nothing -> Just cs
        Just c -> do
          guard $ c == color
          pure $ M.delete coord cs
      bdCandidates' = M.map (mapMaybe checkAndElim) bdCandidates
  guard $ all (not . null) bdCandidates'
  pure Board
    { bdDims
    , bdTodos = bdTodos'
    , bdCells = bdCells'
    , bdCandidates = bdCandidates'
    }

main :: IO ()
main = do
  let bd = mkBoard (9,9) (snd example)
      bd' = foldl go bd (fst example)
        where
          -- TODO: we should probably not recover from Nothing.
          go curBd (coord,cell) = fromMaybe curBd (updateCell coord cell curBd)
  pprBoard bd
  pprBoard bd'

{-
  We need 2 basic operations here:

  - updateCell:

    + update a cell with red/blue
    + remove its coordinate from todo list,
    + eliminate conflicting candidates
    + remove the cell itself from candidate maps (so that we don't need to check for that repeatedly)

  - improveCell:

    + look at one particular (coord, candidates) pair
    + find what's common in all candidates (a list of (coord, cell)s)
    + updateCell to make those finding concrete.

 -}
