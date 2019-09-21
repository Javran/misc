{-# LANGUAGE RecordWildCards #-}
module Game.Reversi.GameState
  ( GameState
  , Coord
  , Color
  , Board
  , initGameState
  , applyMove
  , possibleMoves
  , gsBoard
  , gsTurn
  , gsNextMoves
  , gameConcluded
  , switchSide
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Bool

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Game.Reversi.Core as Core
import Game.Reversi.Core
  ( Coord
  , Color
  , Board
  , allCoords
  , initBoard
  )

{-
  This module defines GameState and operations around it.
 -}

data GameState
  = GameState
  { gsBoard :: Board
  , gsFreeCells :: S.Set Coord -- not yet occupied cells
  , gsTurn :: Color -- who's turn
  , gsNextMoves :: (M.Map Coord Board, M.Map Coord Board)
    {-
      TODO: note that using Map might not be the most efficient idea here,
      this is because Map expects a finite set and to test the emptiness of a Map,
      all keys must be visited
      (e.g. `M.null $ M.fromList [1..]` will not terminate despite
      that the result looks obvious)
    -}
    -- (<next possible moves for light>, <next possible moves for dark>)
  }

initGameState :: GameState
initGameState = GameState {..}
  where
    gsBoard = initBoard
    gsFreeCells = S.difference allCoords (M.keysSet gsBoard)
    gsTurn = True -- dark always moves first.
    gsNextMoves = bimap pm pm (False, True)
      where
        pm = Core.possibleMoves allCoords gsBoard

possibleMoves :: GameState -> M.Map Coord Board
possibleMoves gs = bool fst snd (gsTurn gs) . gsNextMoves $ gs

gameConcluded :: GameState -> Bool
gameConcluded GameState { gsNextMoves = (movesLight, movesDark)} =
  M.null movesLight && M.null movesDark

applyMove :: GameState -> Coord -> Maybe GameState
applyMove gs coord = do
  let who = gsTurn gs
      nextMoves = possibleMoves gs
  bd' <- nextMoves M.!? coord
  let freeCells = S.delete coord (gsFreeCells gs)
  pure GameState
    { gsBoard = bd'
    , gsFreeCells = freeCells
    , gsTurn = not who
    , gsNextMoves =
        let pm = Core.possibleMoves freeCells bd'
        in bimap pm pm (False, True)
    }

switchSide :: GameState -> Maybe GameState
switchSide gs = do
  guard $ M.null $ possibleMoves gs
  pure (gs {gsTurn = not (gsTurn gs)})
