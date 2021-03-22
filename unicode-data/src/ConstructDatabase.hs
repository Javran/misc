module ConstructDatabase where

import qualified Data.Array.Unboxed as A
import Data.Char
import Data.Word
import PrepareDatabase

type GCDatabase = (A.UArray Int Word32, A.UArray Int Word32, A.UArray Int Word8)

mkDatabase :: [(Ranged, GeneralCategory)] -> GCDatabase
mkDatabase gs = gcDb
  where
    l = length gs
    mkArr prj =
      A.listArray
        (0, l -1)
        (fmap prj gs)
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

-- this also serves as verifying that query is implemented correctly.
validateDatabase :: GCDatabase -> IO ()
validateDatabase gcDb = do
  let allChars :: [Char]
      allChars = [minBound .. maxBound]
      notDefined :: [Char]
      notDefined = filter ((== NotAssigned) . query gcDb) allChars
      inconsistents
        :: [ ( Char
             , GeneralCategory -- general category from base
             , GeneralCategory -- general category from UnicodeData.txt
             )
           ]
      inconsistents = concatMap getInconsistent allChars
        where
          getInconsistent ch =
            [(ch, libGc, u13) | libGc /= NotAssigned, u13 /= libGc]
            where
              libGc = generalCategory ch
              u13 = query gcDb ch
      newItems :: [(Char, GeneralCategory)]
      newItems = concatMap go allChars
        where
          go ch =
            [(ch, u13) | libGc == NotAssigned && u13 /= NotAssigned]
            where
              libGc = generalCategory ch
              u13 = query gcDb ch
  putStrLn $ "Number of NotAssigned in database: " <> show (length notDefined)
  putStrLn $ "Newly assigned since base: " <> show (length newItems)
  putStrLn "Inconsistent chars:"
  mapM_ print inconsistents

{-

Notes are based on the results of following setup:

- The Glorious Glasgow Haskell Compilation System, version 8.8.4
- Unicode 13.0.0

5 known inconsistent characters:

+ ('\5741',OtherPunctuation,Just OtherSymbol)
  https://unicode.org/reports/tr44/
  The Terminal_Punctuation property of U+166D CANADIAN SYLLABICS CHI SIGN was changed to No

+ ('\43453',SpacingCombiningMark,Just NonSpacingMark)
  https://unicode.org/reports/tr44/
  The classification of the dependent form of the Javanese vocalic r,
  U+A9BD JAVANESE CONSONANT SIGN KERET, was corrected to a below-base mark

+ ('\72146',NonSpacingMark,Just SpacingCombiningMark)
  https://www.unicode.org/L2/L2019/19047-script-adhoc-recs.pdf

+ ('\72162',OtherLetter,Just OtherPunctuation)
  not sure about this one, it's already Po in Unicode 12.0.0 and Unicode 12.1.0.

+ ('\123215',OtherLetter,Just OtherSymbol)
  https://www.unicode.org/L2/L2019/19008.htm
  "Update the general category of U+1E14F NYIAKENG PUACHUE HMONG CIRCLED CA
  from gc="Lo" to "So", for Unicode version 12.0."

GHC's table:

  https://github.com/ghc/ghc/commits/ghc-8.10.4-release/libraries/base/cbits/WCsubst.c

 -}
