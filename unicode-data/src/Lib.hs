{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( main
  )
where

import qualified Codec.Compression.Lzma as Lzma
import qualified Data.ByteString.Lazy as BSL
import Data.Function
import Data.List
import Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Numeric
import Data.Bifunctor

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
      gpMinus (Left (a, b)) (Left (c, d)) = b - c
      gpMinus (Left (a, b)) (Right c) = b - c
      gpMinus (Right a) (Left (b, c)) = a - b
      gpMinus (Right a) (Right b) = a - b
      -- yes, strictly increasing.
      _isIncr@True = and $ zipWith isStrictIncr gs (tail gs)
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
  putStrLn $ "consecutive ranges in total: " <> show (sum (fmap (length . snd) gcGroupped'))
  print $ S.size $ S.fromList (fmap fst gcGroupped)
