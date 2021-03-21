{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Codec.Compression.Lzma as Lzma
import qualified Data.Array.Unboxed as A
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Ord
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

query :: GCDatabase -> Char -> Maybe GeneralCategory
query (loArr, hiArr, valArr) ch = toEnum . fromIntegral <$> search lo hi
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
                 EQ -> Just (valArr A.! mid)
                 LT -> search l (mid-1)
                 GT -> search (mid+1) r
        else Nothing

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
  putStrLn $
    "verify that codepoint values are given in strictly increasing order: " <> show isIncr
  putStrLn $ "consecutive ranges in total: " <> show (sum (fmap (length . snd) gcGroupped'))
  let revGroups :: [(Either (Int, Int) Int, GeneralCategory)]
      revGroups = concatMap (\(gc, xs) -> [(x, gcTable M.! gc) | x <- xs]) gcGroupped'
  print (fmap fst revGroups)
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
    print loArr
    print hiArr
    print valArr
    print (query gcDb '\x7c4')
