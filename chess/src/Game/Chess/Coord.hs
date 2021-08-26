{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-
  Coordinations on a Chess board.
 -}

module Game.Chess.Coord where

import Data.Bits
import Data.Word
import Game.Chess.TH

{-
  Word8 but only 0~63 are valid.

  - low bits (0~2) represents file
  - high bits (3~5) represents rank
 -}
newtype Coord = Coord Word8 deriving (Num, Enum, Show)

$(bindList 64 linearCoordName [|[0 .. 63]|] [t|Coord|])

{-
  Rank and file both are both expected to be in [0..7]
 -}
unsafeFromRankAndFile :: Integral i => i -> i -> Coord
unsafeFromRankAndFile
  (fromIntegral -> rInd)
  (fromIntegral -> fInd) =
    Coord $ shiftL rInd 3 .|. (7 .&. fInd)
