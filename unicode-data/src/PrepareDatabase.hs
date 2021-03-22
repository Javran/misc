{-# LANGUAGE OverloadedStrings #-}

module PrepareDatabase where

{-
  Parses related part of UnicodeData.txt,
  and prepare a representation for efficient querying.
 -}

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Numeric
import System.Exit

type Ranged =
  Either
    (Int, Int) -- [l .. r] (both inclusive)
    Int

gcLitTable :: M.Map T.Text GeneralCategory
gcLitTable = M.fromList $ zip (T.words abbrs) [minBound .. maxBound]
  where
    abbrs =
      "Lu Ll Lt Lm Lo \
      \Mn Mc Me \
      \Nd Nl No \
      \Pc Pd Ps Pe Pi Pf Po \
      \Sm Sc Sk So \
      \Zs Zl Zp \
      \Cc Cf Cs Co Cn"

verifyAndProcess :: T.Text -> IO [(Ranged, GeneralCategory)]
verifyAndProcess raw = do
  let dieIf flag reason = when flag $ do
        putStrLn reason
        exitFailure
      rawLines = T.lines raw
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
      isIncr = and $ zipWith isStrictIncr gs (tail gs)
        where
          isStrictIncr l r = gpMinus l r < 0
          gs = fmap snd groupped
  dieIf
    (not isIncr)
    "Data rows are not strictly ascending."
  let gcGroupped :: [(T.Text, [Either (Int, Int) Int])]
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
      gcGroupped' :: [(T.Text, [Either (Int, Int) Int])]
      gcGroupped' = (fmap . second) (merge []) gcGroupped
  dieIf
    (S.member "Cn" (S.fromList $ fmap fst gcGroupped'))
    "No character should be in 'Cn' category"
  putStrLn $ "Raw rows in total: " <> show (length rows)
  putStrLn $ "Rows after range groupping: " <> show (sum (fmap (length . snd) gcGroupped))
  let revGroups :: [(Ranged, GeneralCategory)]
      revGroups = concatMap (\(gc, xs) -> [(x, gcLitTable M.! gc) | x <- xs]) gcGroupped'
  putStrLn $
    "Final item count (consecutive ranges with same category): " <> show (length revGroups)
  pure revGroups
