module Main where

import Data.Bits

import Graphics.UI.WX
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.Frame
import Graphics.UI.WXCore.WxcClassesAL
import Graphics.UI.WXCore.WxcClassesMZ
import Graphics.UI.WXCore.WxcTypes
import Text.Printf

main :: IO ()
main = start simple

-- http://stackoverflow.com/questions/15001883/wxhaskell-initial-frame-size-too-small

simple :: IO ()
simple = do
    gSizer <- gridSizerCreate 3 4 4 4
    w <- frame [text := "@dev Grid tests"]
    windowSetSizer w gSizer
    let fmt :: Int -> Int -> String
        fmt = printf "(%d,%d)"
    mapM_ (\(r,c) -> button w [ text := fmt r c
                              , on command := close w
                              ]
           >>= \b -> sizerAddWindow gSizer b 1 (wxEXPAND .|. wxALL) 5 ptrNull)
          [ (r,c) | r <- [1..3], c <- [1..4] ]

    frameCenter w
    windowReLayout w
