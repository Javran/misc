module Main
  ( main
  ) where

{-
  Implmentation of Reversi (https://en.wikipedia.org/wiki/Reversi)
 -}

import Data.Maybe
import Control.Monad
import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Coord = (Int {- row -}, Int {- col -}) -- note that this is 0-based index
type Color = Bool -- False for light, True for dark

type Board = M.Map Coord Color

type GameState = (Board, Color) -- current board & who's turn

initBoard :: Board
initBoard = M.fromList [((3,3),False), ((4,4),False), ((3,4),True), ((4,3),True)]

initGameState :: GameState
initGameState = (initBoard, True)

readMove :: String -> Maybe Coord
readMove raw = coordTable M.!? raw
  where
    coordTable = M.fromList
      [ ([colCh,rowCh], (r,c))
      | (c,colCh) <- zip [0..] ['a'..'h']
      , (r,rowCh) <- zip [0..] ['1'..'8']
      ]

type Dir = (Int, Int)

dirs :: [] Dir
dirs = ds <> ((\(r,c) -> (-r,-c)) <$> ds)
  where
    ds = [(1,0),(0,1),(1,1),(1,-1)]

-- get the list of disks starting from coord, along dir.
getDisks :: Board -> Coord -> Dir -> [(Coord, Maybe Color)]
getDisks bd coord (dr,dc) = (\k -> (k, M.lookup k bd)) <$> coords
  where
    coords = iterate (\(r,c) -> (r+dr,c+dc)) coord

-- get the list of coordinates of disks that will be filpped
-- because of next moving being coord :: Coord.
applyMoveOnDir :: Board -> Color -> Coord -> Dir -> [Coord]
applyMoveOnDir bd who coord dir =
    if null owns
      then []
      else fst <$> oppos
  where
    (oppos, owns) = span ((/= who) . snd) consecutives
    consecutives =
      fmap (\(k, Just v) -> (k, v))
      -- only take consecutive non-empty values
      . takeWhile (isJust . snd)
      -- skip first element, which is coord itself.
      . tail $ getDisks bd coord dir

applyMove :: Board -> Color -> Coord -> Maybe (S.Set Coord, Board)
applyMove bd who coord = do
  guard $ M.notMember coord bd
  let flipCoords = dirs >>= \dir -> applyMoveOnDir bd who coord dir
      flipCoordsSet = S.fromList flipCoords
      bd' =
        S.foldr
          (\coord' -> M.insert coord' who)
          (M.insert coord who bd)
          flipCoordsSet
  guard . not . null $ flipCoords
  pure (flipCoordsSet, bd')

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

possibleMoves :: Board -> Color -> [] Coord
possibleMoves bd who =
    filter (\c -> isJust $ applyMove bd who c) allCoords
  where
    allCoords = [(r,c) | r <- [0..7], c <- [0..7]]

proceedGame :: StateT GameState IO ()
proceedGame = do
  (bd, who) <- get
  let renderEx _ (r,c) =
        case applyMove bd who (r,c) of
          Nothing -> Nothing
          Just _ -> Just '?'
  liftIO $ do
    putStrLn $ (if who then "Dark (X)" else "Light (O)") <> "'s turn."
    mapM_ putStrLn $ renderBoard bd renderEx
    let showMove (r',c') = [['a' .. 'h'] !! c', ['1'..'8'] !! r']
    liftIO $ putStrLn $ "Possible moves: " <> unwords (showMove <$> possibleMoves bd who)
  mMove <- readMove <$> liftIO getLine
  case mMove of
    Just coord |
      Just (_,bd') <- applyMove bd who coord -> do
        -- check whether opponent has any possible moves
        let who' = not who
        if null (possibleMoves bd' who')
          then
            if null (possibleMoves bd' who)
              then do
                liftIO $ putStrLn "Game over."
                let (darks, lights) = M.partition id bd'
                liftIO $ putStrLn $ " Dark: " <> show (M.size darks)
                liftIO $ putStrLn $ " Light: " <> show (M.size lights)
              else do
                liftIO $ putStrLn $
                  (if who' then "Dark" else "Light")
                  <> " does not have any valid move, passing."
                put (bd', who)
                proceedGame
          else do
            -- next move is possible, proceed.
            put (bd', who')
            proceedGame
    _ -> do
      liftIO $ putStrLn "Invalid move."
      proceedGame


main :: IO ()
main = evalStateT proceedGame initGameState
