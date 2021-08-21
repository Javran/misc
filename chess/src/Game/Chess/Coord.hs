{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
  Coordinations on a Chess board.
 -}
{-# LANGUAGE TemplateHaskell #-}

module Game.Chess.Coord where

import Data.Word
import Game.Chess.TH

{-
  Word8 but only 0~63 are valid.

  - low bits (0~2) represents file
  - high bits (3~5) represents rank
 -}
newtype LinearCoord = LinearCoord Word8 deriving (Num, Enum, Show)

$(bindList 64 linearCoordName [|[0 .. 63]|] [t|LinearCoord|])
