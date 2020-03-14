{-
  Get static resources from kc servers. perferably with
  requests spreaded so we don't hammer a specific one.
 -}
{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , DeriveGeneric
  , ScopedTypeVariables
  #-}
module Main
  ( main
  ) where

import Dhall
import System.Exit
import System.Environment
import Data.Aeson
import Data.Aeson.Types
import Control.Monad.Fail
import Control.Monad

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M

import qualified Config

data FrameInfo
  = FrameInfo
  { fiCoord :: (Int, Int) -- (x,y)
  , fiSize :: (Int, Int) -- (w,h)
  } deriving (Generic, Show)

instance FromJSON FrameInfo where
  parseJSON = withObject "FrameInfo" $ \v -> do
    rotated <- v .: "rotated"
    when rotated $
      Control.Monad.Fail.fail "rotated shouldn't be False"
    trimmed <- v .: "trimmed"
    when trimmed $
      Control.Monad.Fail.fail "trimmed shouldn't be False"
    (frame :: Object) <- v .: "frame"
    let parseFrame vf = do
          x <- vf .: "x"
          y <- vf .: "y"
          w <- vf .: "w"
          h <- vf .: "h"
          pure $ FrameInfo (x,y) (w,h)
    withObject "FrameInfo.frame" parseFrame (Object frame)

newtype SpriteFrames
  = SpriteFrames (M.Map Text FrameInfo)
    deriving (Generic, Show)

instance FromJSON SpriteFrames

{-
  Example resource:
  - /kcs2/img/common/common_itemicons.json?version=4.5.3.0
  - /kcs2/img/common/common_itemicons.png?version=4.5.3.0

  somehow we need an path for output and we can download all stuff to there.

  also need a dedicate module for https://github.com/twolfson/spritesmith,
  I think we can write one just for PNG format.

  structure of this json file:

  - frames: an object
    - keys are file names, values are objects
      - frame:
        - x,y,w,h
        - rotated: false
        - trimmed: false
        - spriteSourceSize: {x,y,w,h}
        - sourceSize: {w,h}
 -}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configPath, jsonFile, pngFile] -> do
      cfg <- inputFile @Config.Config auto configPath
      print cfg
      Right (Object hm) <- eitherDecodeFileStrict @Value jsonFile
      let frames = hm HM.! "frames"
          Success (SpriteFrames result) = parse @_ parseJSON frames
      mapM_ print (M.toAscList result)
    _ -> do
      putStrLn "<prog> <config path> <json file path> <png file path>"
      exitFailure
