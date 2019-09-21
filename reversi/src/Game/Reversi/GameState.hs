{-# LANGUAGE RecordWildCards #-}
module Game.Reversi.GameState
  ( GameState
  , Coord
  , Color
  , Board
  , initGameState
  , possibleMoves
  , applyMoveOnGs
  , possibleMovesGs
  , gsBoard
  , gsTurn
  , gsNextMoves
  , gameConcludedGs
  , switchSide
  , Core.applyMove
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
  , allCoords
  , initBoard
  )

type Board = M.Map Coord Color

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

possibleMoves :: Board -> Color -> M.Map Coord Board
possibleMoves = Core.possibleMoves allCoords

possibleMovesGs :: GameState -> M.Map Coord Board
possibleMovesGs gs = bool fst snd (gsTurn gs) . gsNextMoves $ gs

gameConcludedGs :: GameState -> Bool
gameConcludedGs GameState { gsNextMoves = (movesLight, movesDark)} =
  M.null movesLight && M.null movesDark

applyMoveOnGs :: GameState -> Coord -> Maybe GameState
applyMoveOnGs gs coord = do
  let who = gsTurn gs
      nextMoves = possibleMovesGs gs
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
  guard $ M.null $ possibleMovesGs gs
  pure (gs {gsTurn = not (gsTurn gs)})
