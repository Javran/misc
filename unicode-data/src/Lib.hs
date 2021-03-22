{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Char (GeneralCategory (..), chr)
import Data.Functor.Contravariant
import Data.Ix
import Data.Text.Encoding (decodeUtf8)
import GeneralCategoryPredicates
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

mkJavaIdentifierPreds :: GCDatabase -> (Char -> Bool, Char -> Bool)
mkJavaIdentifierPreds gcDb = (isJavaIdentifierStart, isJavaIdentifierPart)
  where
    GeneralCategoryPredicates {..} = contramap (query gcDb) predicates
    isJavaIdentifierStart ch =
      isLetter ch || generalCategory ch
        `elem` [ LetterNumber
               , CurrencySymbol
               , ConnectorPunctuation
               ]
    isIdentifierIgnorable :: Char -> Bool
    isIdentifierIgnorable ch =
      inRange ('\x0000', '\x0008') ch
        || inRange ('\x000E', '\x0001B') ch
        || inRange ('\x007F', '\x009F') ch
        || gc == Format
      where
        gc = generalCategory ch

    isJavaIdentifierPart :: Char -> Bool
    isJavaIdentifierPart ch =
      isLetter ch
        || (gc /= OtherNumber && isNumber ch)
        || gc
        `elem` [ CurrencySymbol
               , ConnectorPunctuation
               , SpacingCombiningMark
               , NonSpacingMark
               ]
        || isIdentifierIgnorable ch
      where
        gc = generalCategory ch

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
              filter f allChars
        putStrLn $ "xs: " <> show (length xs)
        putStrLn $ "truth: " <> show (length truth)
        -- let extra =  S.fromList xs `S.difference` S.fromList truth
        putStrLn $ "same: " <> show (truth == xs)
      (isJavaIdentifierStart, isJavaIdentifierPart) = mkJavaIdentifierPreds gcDb
  verifyFn isJavaIdentifierStart "start.txt"
  verifyFn isJavaIdentifierPart "part.txt"
