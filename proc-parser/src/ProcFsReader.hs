{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module ProcFsReader where

{-
  This module provides parsing logic to various info out of /proc. get CPU stat out of /proc/stat.
  Note that this is by no means a complete parsing of that file (for now) -
  the sole mission for this module is just to get enough info out.

  Current features:

  - extract cpu utilizations for each core from /proc/stat
  - extract cpu frequencies for each core from /proc/cpuinfo
  - extract network device stats from /proc/net/dev for net stat
  - extract info about memory utilization from /proc/meminfo.

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

-- consume rest of the current line, '\n' is also consumed but removed from the result.
restOfCurrentLine :: Parser BSC.ByteString
restOfCurrentLine = P.takeWhile (/= '\n') <* anyChar -- this one must be newline, no check necessary.

parseCpuStatRow :: Bool -> Parser ParsedCpuStatRow
parseCpuStatRow isSummaryRow = do
  _ <- "cpu"
  mCpuId <- if isSummaryRow
    then pure Nothing
    else Just <$> decimal
  [user, nice, system, idle, ioWait, irq, softIrq, steal] <-
    replicateM 8 (skipSpace >> decimal)
  leftover <- restOfCurrentLine
  pure (mCpuId, (CpuStatRow {..}, leftover))

parseProcStat :: Parser (ParsedCpuStatRow, [ParsedCpuStatRow])
parseProcStat = do
  cpuSummary <- parseCpuStatRow True
  xs <- many1 (parseCpuStatRow False)
  pure (cpuSummary, xs)


data NetDevStat a
  = NetDevStat
  { ndRxBytes :: a
  , ndRxPackets :: a
  , ndRxErrs :: a
  , ndRxDrop :: a
  , ndRxFifo :: a
  , ndRxFrame :: a
  , ndRxCompressed :: a
  , ndRxMulticast :: a
  , ndTxBytes :: a
  , ndTxPackets :: a
  , ndTxErrs :: a
  , ndTxDrop :: a
  , ndTxFifo :: a
  , ndTxColls :: a
  , ndTxCarrier :: a
  , ndTxCompressed :: a
  } deriving Show

-- kernel source: net/core/net-procfs.c
parseProcNetDev :: Parser [(BSC.ByteString, NetDevStat Word64)]
parseProcNetDev = do
    -- skip first two lines which are hard-coded header.
    restOfCurrentLine
    restOfCurrentLine
    many1 ifLine
  where
    ifLine = do
      skipSpace
      -- https://git.kernel.org/pub/scm/network/iproute2/iproute2.git/tree/lib/utils.c?id=1f420318bda3cc62156e89e1b56d60cc744b48ad#n827
      ifName <- P.takeWhile (\c -> not (c == ':' || c == '\\' || isSpace c))
      skipSpace
      ":"
      [ ndRxBytes, ndRxPackets, ndRxErrs, ndRxDrop
        , ndRxFifo, ndRxFrame, ndRxCompressed, ndRxMulticast
        , ndTxBytes, ndTxPackets, ndTxErrs, ndTxDrop
        , ndTxFifo, ndTxColls, ndTxCarrier, ndTxCompressed
        ] <- replicateM 16 (skipSpace *> decimal)
      -- the table is hard-coded, so I'd prefer to be picky and insists that it ends right here.
      "\n"
      pure (ifName, NetDevStat {..})

parseCpuFreqs :: Parser [Scientific]
parseCpuFreqs =
    catMaybes <$>
      many1 ((Just <$> parseCpuFreqLine) <|> (Nothing <$ restOfCurrentLine))
  where
    parseCpuFreqLine =
      "cpu MHz" >> skipSpace
      >> ":" >> skipSpace
      >> P.scientific <* restOfCurrentLine

parseMemInfo :: Parser (Word64, Word64, Word64)
parseMemInfo =
    (,,)
      <$> parseRowKb "MemTotal:"
      <*> parseRowKb "MemFree:"
      <*> parseRowKb "MemAvailable:"
  where
    parseRowKb fieldNameP =
      fieldNameP *> skipSpace *> decimal <* " kB\n"

testParseProcStat :: IO ()
testParseProcStat = do
  raw <- BSC.readFile "/proc/stat"
  let Right (v, vs) = parseOnly parseProcStat raw
  mapM_ print (v : vs)

testParseCpuFreqs :: IO ()
testParseCpuFreqs = do
  raw <- BSC.readFile "/proc/cpuinfo"
  case parseOnly parseCpuFreqs raw of
    Right freqs ->
      mapM_ print freqs
    Left err ->
      putStrLn err

testParseProcNetDev :: IO ()
testParseProcNetDev = do
  raw <- BSC.readFile "/proc/net/dev"
  case parseOnly parseProcNetDev raw of
    Right v ->
      mapM_ print v
    Left err ->
      putStrLn err

testParseMemInfo :: IO ()
testParseMemInfo = do
  raw <- BSC.readFile "/proc/meminfo"
  case parseOnly parseMemInfo raw of
    Right v ->
      print v
    Left err ->
      putStrLn err

main :: IO ()
main = do
  testParseProcStat
  testParseCpuFreqs
  testParseProcNetDev
  testParseMemInfo
