module Main
  ( main
  ) where

{-
  Implmentation of Reversi (https://en.wikipedia.org/wiki/Reversi)
 -}

import Data.Maybe
import Control.Monad.State
import qualified Data.Map.Strict as M

import Game.Reversi.Core

readMove :: String -> Maybe Coord
readMove raw = coordTable M.!? raw
  where
    coordTable = M.fromList
      [ ([colCh,rowCh], (r,c))
      | (c,colCh) <- zip [0..] ['a'..'h']
      , (r,rowCh) <- zip [0..] ['1'..'8']
      ]

renderBoard :: Board -> (Board -> Coord -> Maybe Char) -> [String]
renderBoard bd renderEx =
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
              Nothing ->
                fromMaybe ' ' (renderEx bd (r,c))
              Just d ->
                fromMaybe (if d then dark else light) (renderEx bd (r,c))
        secondLine :: String
        secondLine = concat (replicate 7 ['─',cross]) <> ['─',cross']

proceedGame :: StateT GameState IO ()
proceedGame = do
  gs <- get
  let bd = gsBoard gs
      who = gsTurn gs
  let renderEx _ (r,c) =
        case applyMove bd who (r,c) of
          Nothing -> Nothing
          Just _ -> Just '?'
  liftIO $ do
    putStrLn $ (if who then "Dark (X)" else "Light (O)") <> "'s turn."
    mapM_ putStrLn $ renderBoard bd renderEx
    let showMove (r',c') = [['a' .. 'h'] !! c', ['1'..'8'] !! r']
    liftIO $ putStrLn $ "Possible moves: " <>
      unwords (showMove <$> M.keys (possibleMovesGs gs))
  mMove <- readMove <$> liftIO getLine
  case mMove of
    Just coord |
      Just gs' <- applyMoveOnGs gs coord ->
        if gameConcludedGs gs'
          then liftIO $ do
            putStrLn "Game over."
            let (darks, lights) = M.partition id (gsBoard gs')
            putStrLn $ "  Dark: " <> show (M.size darks)
            putStrLn $ "  Light: " <> show (M.size lights)
          else
            -- switchSide is a possible move only if no other move can be performed.
            case switchSide gs' of
              Just gs'' -> do
                let who' = gsTurn gs'
                liftIO $ putStrLn $
                  (if who' then "Dark" else "Light")
                  <> " does not have any valid move, passing."
                put gs''
                proceedGame
              Nothing -> do
                put gs'
                proceedGame
    _ -> do
      liftIO $ putStrLn "Invalid move."
      proceedGame

main :: IO ()
main = evalStateT proceedGame initGameState
