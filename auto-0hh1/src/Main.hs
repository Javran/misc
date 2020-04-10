{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import System.Process
import System.Exit

import qualified Graphics.Image as HIP
import qualified Data.ByteString as BS

-- https://stackoverflow.com/a/13587203/315302
-- adb exec-out "screencap -p"
-- https://stackoverflow.com/a/5392547/315302
-- adb shell input tap x y

type Image = HIP.Image HIP.VS HIP.RGBA HIP.Word8

coords :: [[(Int, Int)]]
coords = do
  r <- take 12 $ iterate (+87) 446
  pure [(r,c) | c <- take 12 $ iterate (+88) 51]

extractCellValues :: Image -> [[HIP.Pixel HIP.RGBA HIP.Word8]]
extractCellValues img = (fmap.fmap) (HIP.index img) coords

screenCapture :: IO Image
screenCapture = do
  let cp =
        (proc "/usr/bin/adb" ["exec-out", "screencap -p"])
          { std_out = CreatePipe
          }
  (_, Just hOut, _, ph) <- createProcess cp
  imgRaw <- BS.hGetContents hOut
  ExitSuccess <- waitForProcess ph
  let Right img = HIP.decode HIP.PNG imgRaw
  pure img

main :: IO ()
main = do
  img <- screenCapture
  let ps = extractCellValues img
      pixels =
        foldr1 HIP.topToBottom
        . fmap (foldr1 HIP.leftToRight)
        . (fmap . fmap)
          (\c -> HIP.makeImage (50,50) (const c) :: Image)
        $ ps
  mapM_ print ps
  pure ()
