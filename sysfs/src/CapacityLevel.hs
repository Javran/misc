module CapacityLevel
  ( CapacityLevel (..)
  , capacityLevelP
  )
where

import Text.ParserCombinators.ReadP

data CapacityLevel
  = Unknown
  | Critical
  | Low
  | Normal
  | High
  | Full
  deriving (Read, Show)

capacityLevelP :: ReadP CapacityLevel
capacityLevelP = readS_to_P reads
