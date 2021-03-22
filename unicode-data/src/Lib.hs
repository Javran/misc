{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Lib
  ( main
  , benchmarker
  )
where

import qualified Codec.Compression.Lzma as Lzma
import ConstructDatabase
import Control.Monad
import Data.Bits
import Data.Binary
import qualified Data.ByteString.Lazy as BSL
import Data.Char (GeneralCategory (..), chr)
import Data.Functor.Contravariant
import Data.Ix
import Data.Text.Encoding (decodeUtf8)
import GeneralCategoryPredicates
import PrepareDatabase
import qualified UnicodeV13 as U13
import qualified UnicodeV13Packed as U13P

mkJavaIdentifierPreds :: (Char -> GeneralCategory) -> (Char -> Bool, Char -> Bool)
mkJavaIdentifierPreds gcx = (isJavaIdentifierStart, isJavaIdentifierPart)
  where
    GeneralCategoryPredicates {..} = contramap gcx predicates
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
        || gc /= OtherNumber && isNumber ch
        || gc
        `elem` [ CurrencySymbol
               , ConnectorPunctuation
               , SpacingCombiningMark
               , NonSpacingMark
               ]
        || isIdentifierIgnorable ch
      where
        gc = generalCategory ch

benchmarker :: (Char -> GeneralCategory) -> Word8
benchmarker gc = foldr (\ch acc -> fromIntegral (fromEnum (gc ch)) `xor` acc) 0 allChars
  where
    allChars :: [Char]
    allChars = [minBound .. maxBound]

main :: IO ()
main = mainExperiments

mainExperiments :: IO ()
mainExperiments = do
  let needPrepareDatabase = False
  when needPrepareDatabase $ do
    xsr <- BSL.readFile "UnicodeData.txt.xz"
    revGroups <- verifyAndProcess . decodeUtf8 . BSL.toStrict $ Lzma.decompress xsr
    BSL.writeFile "u13-packed.raw" (encode $ mkDatabasePacked revGroups)
  validateDatabase U13.generalCategory
  validateDatabase U13P.generalCategory
  let allChars :: String
      allChars = [minBound .. maxBound]

      verifyFn f fn = do
        rawStart <- readFile fn
        let truth :: String
            truth = chr <$> read rawStart
            xs =
              -- those recognized by function
              filter f allChars
        putStrLn $ "xs: " <> show (length xs)
        putStrLn $ "truth: " <> show (length truth)
        -- let extra =  S.fromList xs `S.difference` S.fromList truth
        putStrLn $ "same: " <> show (truth == xs)
      (isJavaIdentifierStart, isJavaIdentifierPart) = mkJavaIdentifierPreds U13.generalCategory
  -- verifyFn isJavaIdentifierStart "start.txt"
  -- verifyFn isJavaIdentifierPart "part.txt"
  print (benchmarker U13.generalCategory)
  print (benchmarker U13P.generalCategory)
