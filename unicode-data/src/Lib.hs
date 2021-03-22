{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Codec.Compression.Lzma as Lzma
import Control.Monad
import qualified Data.Array.Unboxed as A
import Data.Bifunctor
import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Function
import Data.Ix
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import Numeric

gcTable :: M.Map T.Text GeneralCategory
gcTable = M.fromList $ zip (T.words abbrs) [minBound .. maxBound]
  where
    abbrs =
      "Lu Ll Lt Lm Lo \
      \Mn Mc Me \
      \Nd Nl No \
      \Pc Pd Ps Pe Pi Pf Po \
      \Sm Sc Sk So \
      \Zs Zl Zp \
      \Cc Cf Cs Co Cn"

type GCDatabase = (A.UArray Int Word32, A.UArray Int Word32, A.UArray Int Word8)

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

query :: GCDatabase -> Char -> GeneralCategory
query (loArr, hiArr, valArr) ch = toEnum . fromIntegral $ search lo hi
  where
    needle :: Word32
    needle = fromIntegral $ ord ch
    (lo, hi) = A.bounds loArr
    -- compare <needle> <range at index>
    cmp' :: Int -> Ordering
    cmp' i
      | needle < rangeL = LT
      | needle > rangeR = GT
      | rangeL <= needle && needle <= rangeR = EQ
      | otherwise = error "unreachable"
      where
        rangeL = loArr A.! i
        rangeR = hiArr A.! i
    search l r =
      if l <= r
        then
          let mid = (l + r) `quot` 2
           in case cmp' mid of
                EQ -> valArr A.! mid
                LT -> search l (mid -1)
                GT -> search (mid + 1) r
        else fromIntegral $ fromEnum NotAssigned

main :: IO ()
main = do
  xsr <- BSL.readFile "UnicodeData.txt.xz"
  let rawLines = T.lines . decodeUtf8 . BSL.toStrict $ Lzma.decompress xsr
      rows = fmap extract rawLines
        where
          extract rawLine = (code :: Int, desc, gc)
            where
              [(code, "")] = readHex (T.unpack rawCode)
              rawCode : desc : gc : _ = T.splitOn ";" rawLine
      groupped :: [(T.Text, Either (Int, Int) Int)]
      groupped = norm <$> groupBy zCmp rows
        where
          norm [(c, _, gc)] = (gc, Right c)
          norm [(c0, _, gc0), (c1, _, gc1)]
            | gc0 == gc1
                && T.dropEnd (T.length "First>") gc0
                == T.dropEnd (T.length "Last>") gc1 =
              (gc0, Left (c0, c1))
          norm _ = error "invalid"
          zCmp (_, desc0, _) (_, desc1, _) =
            "First>" `T.isSuffixOf` desc0
              && "Last>" `T.isSuffixOf` desc1
      gpMinus (Left (_a, b)) (Left (c, _d)) = b - c
      gpMinus (Left (_a, b)) (Right c) = b - c
      gpMinus (Right a) (Left (b, _c)) = a - b
      gpMinus (Right a) (Right b) = a - b
      isIncr@True = and $ zipWith isStrictIncr gs (tail gs)
        where
          isStrictIncr l r = gpMinus l r < 0
          gs = fmap snd groupped
      gcGroupped :: [(T.Text, [Either (Int, Int) Int])]
      gcGroupped =
        (\ts -> (fst . head $ ts, fmap snd ts)) <$> groupBy ((==) `on` fst) groupped
      merge acc [] = reverse acc
      merge [] (x : xs) = merge [x] xs
      merge (u : us) (x : xs) = case (u, x) of
        (Left (a, b), Left (c, d)) ->
          if b + 1 == c then merge (Left (a, d) : us) xs else merge (x : u : us) xs
        (Left (a, b), Right c) ->
          if b + 1 == c then merge (Left (a, c) : us) xs else merge (x : u : us) xs
        (Right a, Left (b, c)) ->
          if a + 1 == b then merge (Left (a, c) : us) xs else merge (x : u : us) xs
        (Right a, Right b) ->
          if a + 1 == b then merge (Left (a, b) : us) xs else merge (x : u : us) xs
      gcGroupped' = (fmap . second) (merge []) gcGroupped

  putStrLn $ "raw rows in total: " <> show (length rows)
  putStrLn $ "rows after range groupping: " <> show (sum (fmap (length . snd) gcGroupped))
  putStrLn $ "No character is Cn: " <> show (S.notMember "Cn" (S.fromList $ fmap fst gcGroupped'))
  putStrLn $
    "verify that codepoint values are given in strictly increasing order: " <> show isIncr
  putStrLn $ "consecutive ranges in total: " <> show (sum (fmap (length . snd) gcGroupped'))
  let revGroups :: [(Either (Int, Int) Int, GeneralCategory)]
      revGroups = concatMap (\(gc, xs) -> [(x, gcTable M.! gc) | x <- xs]) gcGroupped'
  gcDb <-
    do
      let l = length revGroups
          mkArr prj =
            A.listArray
              (0, l -1)
              (fmap prj revGroups)
          loArr, hiArr :: A.UArray Int Word32
          loArr =
            mkArr
              (\(e, _) -> case e of
                 Left (i, _) -> fromIntegral i
                 Right i -> fromIntegral i)
          hiArr =
            mkArr
              (\(e, _) -> case e of
                 Left (_, i) -> fromIntegral i
                 Right i -> fromIntegral i)
          valArr :: A.UArray Int Word8
          valArr =
            mkArr
              (\(_, gc) -> fromIntegral (fromEnum gc))
          gcDb :: GCDatabase
          gcDb = (loArr, hiArr, valArr)
      pure gcDb
  let allChars :: [Char]
      allChars = [minBound .. maxBound]

  do
    let notDefined :: [Char]
        notDefined = filter ((== NotAssigned) . query gcDb) allChars
        inconsistents :: [(Char, GeneralCategory, GeneralCategory)]
        inconsistents = concatMap getInconsistent allChars
          where
            getInconsistent ch =
              if libGc == NotAssigned
                then []
                else
                  let u13 = query gcDb ch
                   in [(ch, libGc, u13) | query gcDb ch /= libGc]
              where
                libGc = generalCategory ch
        newItems :: [(Char, GeneralCategory)]
        newItems = concatMap go allChars
          where
            go ch =
              [(ch, u13) | libGc == NotAssigned && u13 /= NotAssigned]
              where
                libGc = generalCategory ch
                u13 = query gcDb ch
    putStrLn $ "U13 Query failures: " <> show (length notDefined)
    putStrLn $ "all failures are NotAssigned?: " <> show (all ((== NotAssigned) . generalCategory) notDefined)
    putStrLn $ "newly assigned chars: " <> show (length newItems)
    putStrLn "Inconsistent chars:"
    mapM_ print inconsistents
  let dump = False
  when dump $
    do
      -- writeFile "u13.raw" (show gcDb)
      raw2 <- readFile "u13.raw"
      let gcDb' :: GCDatabase
          gcDb' = read raw2
      print $ gcDb' == gcDb
  let verifyFn f fn = do
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

{-
- The Glorious Glasgow Haskell Compilation System, version 8.8.4
- Unicode 13.0.0

5 inconsistent characters:

('\5741',OtherPunctuation,Just OtherSymbol)
https://unicode.org/reports/tr44/
The Terminal_Punctuation property of U+166D CANADIAN SYLLABICS CHI SIGN was changed to No

('\43453',SpacingCombiningMark,Just NonSpacingMark)
https://unicode.org/reports/tr44/
The classification of the dependent form of the Javanese vocalic r, U+A9BD JAVANESE CONSONANT SIGN KERET, was corrected to a below-base mark

('\72146',NonSpacingMark,Just SpacingCombiningMark)
https://www.unicode.org/L2/L2019/19047-script-adhoc-recs.pdf

('\72162',OtherLetter,Just OtherPunctuation)
? - not sure about this one, it's already Po in Unicode 12.0.0 and Unicode 12.1.0.

('\123215',OtherLetter,Just OtherSymbol)
https://www.unicode.org/L2/L2019/19008.htm
"Update the general category of U+1E14F NYIAKENG PUACHUE HMONG CIRCLED CA from gc="Lo" to "So", for Unicode version 12.0."

GHC's table:
https://github.com/ghc/ghc/commits/ghc-8.10.4-release/libraries/base/cbits/WCsubst.c

 -}
