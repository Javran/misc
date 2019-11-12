{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module ProcStatReader where

{-
  (WIP)

  This module provides parsing logic to get CPU stat out of /proc/stat.
  Note that this is by no means a complete parsing of that file (for now) -
  the sole mission for this module is just to get enough info out
  for calculating utilization of each individual cores.

 -}

import Control.Monad
import Data.Attoparsec.ByteString.Char8 as P
import Data.Word
import System.IO

import qualified Data.ByteString.Char8 as BSC

data CpuStatRow a = CpuStatRow
  { user :: a
  , nice :: a
  , system :: a
  , idle :: a -- count as idle time
  , ioWait :: a -- count as idle time
  , irq :: a
  , softIrq :: a
  , steal :: a
    -- there are actually 2 extra fields called "guest" and "guest_nice"
    -- in a recent version of kernel,
    -- but no word is given on how to deal with these two fields - guess we'll just ignore them.
  } deriving (Show)

-- return type: (<cpu id>, (<parsed>, <leftovers of that line>))
type Parsed = (Maybe Word8, (CpuStatRow Word64, BSC.ByteString))

parseCpuStatRow :: Bool -> Parser Parsed
parseCpuStatRow isSummaryRow = do
  _ <- "cpu"
  mCpuId <- if isSummaryRow
    then pure Nothing
    else Just <$> decimal
  [user, nice, system, idle, ioWait, irq, softIrq, steal] <-
    replicateM 8 (skipSpace >> decimal)
  leftover <- P.takeWhile (/= '\n')
  _ <- "\n"
  pure (mCpuId, (CpuStatRow {..}, leftover))

parseProcStat :: Parser (Parsed, [Parsed])
parseProcStat = do
  cpuSummary <- parseCpuStatRow True
  xs <- many1 (parseCpuStatRow False)
  pure (cpuSummary, xs)

main :: IO ()
main = do
  raw <- BSC.readFile "/proc/stat"
  let Right (v, vs) = parseOnly parseProcStat raw
  mapM_ print (v : vs)
  
