{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import System.Process
import System.Exit
import Data.Maybe
import System.Console.Terminfo
import Control.Monad
import Control.Concurrent

import qualified Data.Map.Strict as M
import qualified Graphics.Image as HIP
import qualified Data.ByteString as BS

import qualified Solver

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
    threadDelay $ 1000 * 200
    pure ph
  where
    tapMap :: M.Map (Int,Int) (Int,Int)
    tapMap = M.fromList $ zip [(r',c') | r' <- [0..11], c' <- [0..11]] (concat coords)

solveIt :: Terminal -> [[Char]] -> IO ()
solveIt term tblRaw = do
  let Just bd = Solver.mkBoard 6 (Solver.translateRaw tblRaw)
      bdAfter = Solver.trySolve bd
  Solver.pprBoard term bd
  Solver.pprBoard term bdAfter
  let moves = Solver.genMoves bd bdAfter
  phs <- fmap concat . forM moves $ \(coord,m) ->
    if m
      then (:[]) <$> screenTap coord
      else sequence [screenTap coord, screenTap coord]
  mapM_ waitForProcess phs
  pure ()

main :: IO ()
main = do
  term <- setupTermFromEnv
  img <- screenCapture
  let mCharTable = imageToCharTable img
  case mCharTable of
    Just tbl -> solveIt term tbl
    _ -> pure ()
