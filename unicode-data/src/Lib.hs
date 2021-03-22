{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib
  ( main
  )
where

import qualified Codec.Compression.Lzma as Lzma
import ConstructDatabase
import Control.Monad
import qualified Data.Array.Unboxed as A
import Data.Binary
import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Ix
import Data.Text.Encoding (decodeUtf8)
import PrepareDatabase

{-
  TODO: which way is faster?
 -}
type PackedGCDatabase = A.UArray Int Word64

{-
  low: 0~23
  high: 24~47
  gc: 48~
 -}
packTuple :: (Word32, Word32, Word8) -> Word64
packTuple (lo, high, gc) = fromIntegral lo .|. high' .|. gc'
  where
    high' = fromIntegral high `unsafeShiftL` 24
    gc' = fromIntegral gc `unsafeShiftL` 48

unpackTuple :: Word64 -> (Word32, Word32, Word8)
unpackTuple payload = (lo, high, gc)
  where
    lo, high :: Word32
    lo = fromIntegral (0xFF_FFFF .&. payload)
    high = fromIntegral (0xFF_FFFF .&. (payload `unsafeShiftR` 24))
    gc = fromIntegral (0xFF .&. (payload `unsafeShiftR` 48))

_isLetter :: GCDatabase -> Char -> Bool
_isLetter db ch =
  query db ch
    `elem` [ UppercaseLetter
           , LowercaseLetter
           , TitlecaseLetter
           , ModifierLetter
           , OtherLetter
           ]

_isNumber :: GCDatabase -> Char -> Bool
_isNumber db ch =
  query db ch
    `elem` [ DecimalNumber
           , LetterNumber
           ]

_isJavaIdentifierStart :: GCDatabase -> Char -> Bool
_isJavaIdentifierStart db ch =
  _isLetter db ch || gc
    `elem` [ LetterNumber
           , CurrencySymbol
           , ConnectorPunctuation
           ]
  where
    gc = query db ch

_isJavaIdentifierPart :: GCDatabase -> Char -> Bool
_isJavaIdentifierPart db ch =
  _isLetter db ch
    || _isNumber db ch
    || gc
    `elem` [ CurrencySymbol
           , ConnectorPunctuation
           , SpacingCombiningMark
           , NonSpacingMark
           ]
    || _isIdentifierIgnorable db ch
  where
    gc = query db ch

_isIdentifierIgnorable :: GCDatabase -> Char -> Bool
_isIdentifierIgnorable db ch =
  inRange ('\x0000', '\x0008') ch
    || inRange ('\x000E', '\x0001B') ch
    || inRange ('\x007F', '\x009F') ch
    || gc == Format
  where
    gc = query db ch

main :: IO ()
main = mainExperiments

mainExperiments :: IO ()
mainExperiments = do
  let needPrepareDatabase = False
  when needPrepareDatabase $ do
    xsr <- BSL.readFile "UnicodeData.txt.xz"
    revGroups <- verifyAndProcess . decodeUtf8 . BSL.toStrict $ Lzma.decompress xsr
    BSL.writeFile "u13.raw" (encode $ mkDatabase revGroups)
  gcDb <- decode @GCDatabase <$> BSL.readFile "u13.raw"
  validateDatabase gcDb
  let allChars :: [Char]
      allChars = [minBound .. maxBound]

      verifyFn f fn = do
        rawStart <- readFile fn
        let truth :: [Char]
            truth = chr <$> read rawStart
            xs =
              -- those recognized by function
              filter (f gcDb) allChars
        putStrLn $ "xs: " <> show (length xs)
        putStrLn $ "truth: " <> show (length truth)
        -- let extra =  S.fromList xs `S.difference` S.fromList truth
        putStrLn $ "same: " <> show (truth == xs)
  verifyFn _isJavaIdentifierStart "start.txt"
  verifyFn _isJavaIdentifierPart "part.txt"

