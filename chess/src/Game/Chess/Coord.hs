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
 -}
newtype LinearCoord = LinearCoord Word8 deriving (Num, Enum, Show)

$(bindList 64 linearCoordName [|[0 .. 63]|] [t|LinearCoord|])
