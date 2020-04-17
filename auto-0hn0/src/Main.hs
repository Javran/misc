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
import qualified Graphics.Image.IO as HIP
import qualified Data.ByteString as BS
import qualified Graphics.Image.Processing.Binary as HIP

type Pixel = HIP.Pixel HIP.RGBA HIP.Word8
type Image = HIP.Image HIP.VS HIP.RGBA HIP.Word8

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

main :: IO ()
main = do
  imgFull <- screenCapture
  let samples :: [((Int, Int), Image)]
      samples =
        concat
        . (fmap . fmap) (\coord@(r,c') -> (coord, HIP.crop (413+111*r+30, c'+37) (39,28) imgFull))
        $ [ [ (r,c) | c <- [41,152,263,374,486,598,710,822,934]] | r <- [0..8] ]
  forM_ samples $ \((r,c), img) ->
    let fName = "samples/sample_" <> show r <> "_" <> show c <> ".png"
    in HIP.writeImageExact HIP.PNG [] fName img
  -- _ <- getLine
  pure ()
