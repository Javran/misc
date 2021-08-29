{-# LANGUAGE DerivingVia #-}

module Game.Chess.Bitboard
  ( Bitboard (..)
  )
where

import Data.Bits
import Data.Word

newtype Bitboard = Bitboard
  { getBitboard :: Word64
  }
  deriving (Eq, Bits) via Word64
