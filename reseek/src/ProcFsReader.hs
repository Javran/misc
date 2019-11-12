{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module ProcFsReader where

{-
  This module provides parsing logic to various info out of /proc. get CPU stat out of /proc/stat.
  Note that this is by no means a complete parsing of that file (for now) -
  the sole mission for this module is just to get enough info out.

  Current features:

  - extract cpu utilizations for each core from /proc/stat
  - extract cpu frequencies for each core from /proc/cpuinfo

  TODO:

  - /proc/net/dev for net stat
  - /proc/meminfo for mem usage

 -}

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Attoparsec.ByteString.Char8 as P
import Data.Scientific
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
type ParsedCpuStatRow = (Maybe Word8, (CpuStatRow Word64, BSC.ByteString))

parseCpuStatRow :: Bool -> Parser ParsedCpuStatRow
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

parseProcStat :: Parser (ParsedCpuStatRow, [ParsedCpuStatRow])
parseProcStat = do
  cpuSummary <- parseCpuStatRow True
  xs <- many1 (parseCpuStatRow False)
  pure (cpuSummary, xs)

parseCpuFreqs :: Parser [Scientific]
parseCpuFreqs =
    catMaybes <$>
      many1 ((Just <$> parseCpuFreqLine) <|> (Nothing <$ P.takeWhile (/= '\n') <* "\n"))
  where
    parseCpuFreqLine =
      "cpu MHz" >> skipSpace
      >> ":" >> skipSpace
      >> P.scientific
      <* P.takeWhile (/= '\n') <* "\n"

testParseProcStat :: IO ()
testParseProcStat = do
  raw <- BSC.readFile "/proc/stat"
  let Right (v, vs) = parseOnly parseProcStat raw
  mapM_ print (v : vs)

main :: IO ()
main = do
  raw <- BSC.readFile "/proc/cpuinfo"
  case parseOnly parseCpuFreqs raw of
    Right freqs ->
      mapM_ print freqs
    Left err ->
      putStrLn err
