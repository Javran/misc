{-# LANGUAGE NamedFieldPuns, TypeApplications #-}
module Solver where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import Data.Char
import Data.Semigroup
import System.Console.Terminfo

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
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
    } deriving (Show, Eq)

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
          let isInRange (r',c') = r' >= 0 && r' < rows && c' >= 0 && c' < cols
              checkPair (coord', color) =
                color == cRed || isInRange coord'
          guard $ all checkPair pairs
          pure $ M.fromList (filter (isInRange . fst) pairs)

mods =
  [ \(Placement u r d l) -> Placement (u+1) r d l
  , \(Placement u r d l) -> Placement u (r+1) d l
  , \(Placement u r d l) -> Placement u r (d+1) l
  , \(Placement u r d l) -> Placement u r d (l+1)
  ]

pprBoard :: Terminal -> [(Coord, Int)] -> Board -> IO ()
pprBoard term hints Board{bdDims, bdTodos, bdCells, bdCandidates} = do
  putStrLn $ "Board dimensions: " <> show bdDims
  putStrLn "++++ Board Begin"
  let mRenderFs :: TermStr s => Maybe (Color -> s -> s, Color -> s -> s)
      mRenderFs = (,)
        <$> getCapability term withForegroundColor
        <*> getCapability term withBackgroundColor
      (rows, cols) = bdDims
  case mRenderFs @TermOutput of
    Nothing ->
      forM_ [0..rows-1] $ \r -> do
        let coordToChar coord = case bdCells M.!? coord of
              Nothing -> ' '
              Just c -> if c == cBlue then 'B' else 'R'
        putStr ((\c -> coordToChar (r,c)) <$> [0..cols-1])
    Just (fg, bg) ->
      forM_ [0..rows-1] $ \r -> do
        let ln = foldMap render [0..cols-1]
            sp = termText " "
            render :: Int -> TermOutput
            render c = case bdCells M.!? coord of
                Nothing -> sp
                Just color ->
                  if color == cRed
                    then bg Red sp
                    else bg Blue $ case lookup coord hints of
                      Nothing -> sp
                      Just v -> fg White $
                        if v > 9
                          then termText "."
                          else termText (show v)
              where
                coord = (r,c)
        runTermOutput term $ ln <> termText "\n"
  putStrLn "---- Board End"
  putStrLn $ "Todos: " <> show (length bdTodos)
  unless (M.null bdCandidates) $ do
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

{-
  Basic operation on a board that fills one cell,
  simplifies candidates, and remove contradiction candidates.
 -}
updateCell :: Coord -> Cell -> Board -> Maybe Board
updateCell coord color Board{bdDims, bdTodos, bdCells, bdCandidates} = do
  guard $ coord `S.member` bdTodos
  let bdTodos' = S.delete coord bdTodos
      bdCells' = M.insert coord color bdCells
      -- check current candidate, simplify or remove it
      checkAndElim :: Candidate -> Maybe Candidate
      checkAndElim cs = case cs M.!? coord of
        Nothing -> Just cs -- coord have nothing to do with this candidate, move on.
        Just c -> do
          -- color must not contradict.
          guard $ c == color
          -- remove this coord from candidate list.
          -- this removal is not necessary but it reduces the amount of cells we need to visit for each update.
          pure $ M.delete coord cs
      bdCandidates' =
        M.filter (not . all M.null)
        . M.map (mapMaybe checkAndElim)
        $ bdCandidates
  guard $ all (not . null) bdCandidates'
  pure Board
    { bdDims
    , bdTodos = bdTodos'
    , bdCells = bdCells'
    , bdCandidates = bdCandidates'
    }

improve :: Coord -> Board -> Maybe Board
improve coord bd@Board{bdCandidates} = do
  cs <- bdCandidates M.!? coord
  let commons =
        concatMap (\(k, mv) -> case mv of
                      Nothing -> []
                      Just v -> [(k,v)]
                  )
        . M.toList
        -- note that cs shouldn't be empty if the result comes from "updateCell",
        -- therefore the use of foldl1 is safe.
        -- TODO: good time to explore Data.Map.Merge
        . foldl1 (M.intersectionWith compareMerge)
        . (fmap . M.map) Just
        $ cs
        where
          compareMerge lm rm = do
            l <- lm
            r <- rm
            guard $ l == r
            lm
  guard $ not . null $ commons
  foldM (\curBd (coord',cell) -> updateCell coord' cell curBd) bd commons

improveStep :: Board -> Maybe Board
improveStep bd@Board{bdCandidates} = do
  let bds = mapMaybe (\c -> improve c bd) $ M.keys bdCandidates
  -- choose first successful improvement
  (bd':_) <- pure bds
  pure bd'

solve :: Board -> Board
solve bd = case improveStep bd of
  Just bd' -> if bd == bd' then bd else solve bd'
  Nothing -> bd

loadExample :: [String] -> ([(Coord, Cell)], [(Coord, Int)])
loadExample exampleRaw =
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

solveAndShow :: Terminal -> [String] -> IO ()
solveAndShow term exampleRaw = do
  let example = loadExample exampleRaw
      hints = snd example
      bd = mkBoard (9,9) hints
      Just bd' =
        foldM (\curBd (coord, cell) -> updateCell coord cell curBd) bd (fst example)
  pprBoard term hints bd'
  pprBoard term hints (solve bd')