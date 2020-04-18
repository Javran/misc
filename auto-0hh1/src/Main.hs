{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}
module Main
  ( main
  ) where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.Console.Terminfo
import System.Exit
import System.Process
import System.Random.Shuffle

import qualified Data.Map.Strict as M
import qualified Graphics.Image as HIP
import qualified Data.ByteString as BS
import qualified Graphics.Image.Processing.Binary as HIP

import qualified Game.Takuzu.Solver as Solver

-- https://stackoverflow.com/a/13587203/315302
-- adb exec-out "screencap -p"
-- https://stackoverflow.com/a/5392547/315302
-- adb shell input tap x y

type Pixel = HIP.Pixel HIP.RGBA HIP.Word8
type Image = HIP.Image HIP.VS HIP.RGBA HIP.Word8

coords :: [[(Int, Int)]]
coords = do
  r <- take 12 $ iterate (+87) 446
  pure [(r,c) | c <- take 12 $ iterate (+88) 51]

extractCellValues :: Image -> [[Pixel]]
extractCellValues img = (fmap.fmap) (HIP.index img) coords

pixelToChar :: Pixel -> Maybe Char
pixelToChar p =
    fmap snd . listToMaybe $ filter (HIP.eqTolPx 2 p . fst) table
  where
    rgb r g b = HIP.PixelRGBA r g b 255
    table =
      [ (rgb 42 42 42, ' ')
      , (rgb 213 83 54, 'r')
      , (rgb 194 75 49, 'r')
      , (rgb 48 167 194, 'b')
      , (rgb 53 184 213, 'b')
      ]

imageToCharTable :: Image -> Maybe [[Char]]
imageToCharTable img =
    (mapM . mapM) pixelToChar pixels
  where
    pixels = extractCellValues img

{-
  Known colors:

  grey: 42,42,42
  red: 194,75,49 or 213,83,54
  blue: 48,167,194 or 53,184,213
 -}

screenCapture :: IO Image
screenCapture = do
  let cp =
        (proc "/usr/bin/adb" ["exec-out", "screencap", "-p"])
          { std_out = CreatePipe
          }
  (_, Just hOut, _, ph) <- createProcess cp
  imgRaw <- BS.hGetContents hOut
  ExitSuccess <- waitForProcess ph
  let Right img = HIP.decode HIP.PNG imgRaw
  pure img

screenTap :: (Int, Int) -> IO ProcessHandle
screenTap (r,c) = do
    let (screenR, screenC) = tapMap M.! (r,c)
        cp =
          proc "/usr/bin/adb" ["exec-out", "input", "tap", show screenC, show screenR]
    (_, _, _, ph) <- createProcess cp
    threadDelay $ 1000 * 300
    pure ph
  where
    tapMap :: M.Map (Int,Int) (Int,Int)
    tapMap = M.fromList $ zip [(r',c') | r' <- [0..11], c' <- [0..11]] (concat coords)

solveIt :: Terminal -> [[Char]] -> IO ()
solveIt term tblRaw = do
  let Just bd = Solver.mkBoard 6 (Solver.translateRaw tblRaw)
      bdAfter = Solver.trySolve bd
  appendFile "puzzles.txt" $ unlines (Solver.boardToInput bd <> ["===="])
  Solver.pprBoard term bd
  Solver.pprBoard term bdAfter
  let moves = Solver.genMoves bd bdAfter
      taps = concatMap (\(coord,m) -> if m then [coord] else [coord,coord]) moves
  randomTaps <- shuffleM taps
  phs <- mapM screenTap randomTaps
  mapM_ waitForProcess phs

main :: IO ()
main = do
  term <- setupTermFromEnv
  Right (btn12 :: Image) <- HIP.readImageExact HIP.PNG "sample.png"
  forever $ do
    putStrLn "New round started."
    img <- screenCapture
    let mCharTable = imageToCharTable img
    case mCharTable of
      Just tbl ->
        solveIt term tbl
      _ -> do
        -- see if we can find this "12" button
        let btn12Sample = HIP.crop (986,500) (72,81) img :: Image
        when (HIP.eqTol 2 btn12 btn12Sample) $ do
          putStrLn "Tapping '12'."
          let cp = proc "/usr/bin/adb" ["exec-out", "input", "tap", "540", "1030"]
          (_, _, _, ph) <- createProcess cp
          _ <- waitForProcess ph
          pure ()
    putStrLn "Current round done."
    threadDelay $ 1000 * 3000
