{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( main
  )
where

import Control.Monad
import Data.Foldable
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Ord
import qualified Data.Set as S

data Result = Result
  { rCorrects :: [Maybe Char]
  , rWrongPlaces :: M.Map Char IS.IntSet
  , rNotIn :: S.Set Char
  }
  deriving (Show)

makeGuess :: String -> String -> Result
makeGuess answer guess = Result {rCorrects, rWrongPlaces, rNotIn}
  where
    gSet = S.fromList guess
    ansSet = S.fromList answer
    rCorrects = zipWith (\x y -> x <$ guard (x == y)) answer guess
    wrongIndices =
      catMaybes $
        zipWith
          (\i m -> case m of
             Nothing -> Just i
             Just _ -> Nothing)
          [0 :: Int ..]
          rCorrects
    rWrongPlaces = M.fromListWith (<>) do
      i <- wrongIndices
      let gCh = guess !! i
      guard $ S.member gCh ansSet
      pure (gCh, IS.singleton i)
    rNotIn = gSet S.\\ ansSet

couldMatch :: Result -> String -> Bool
couldMatch Result {rCorrects, rWrongPlaces, rNotIn} candidate =
  condCorrect && condWrongPlaces && condNotIn
  where
    condCorrect =
      and $
        zipWith
          (\m ch -> case m of
             Nothing -> True
             Just v -> v == ch)
          rCorrects
          candidate
    condWrongPlaces = and do
      (w, indices) <- M.toList rWrongPlaces
      i <- IS.toList indices
      guard $ w `elem` candidate
      pure (candidate !! i /= w)
    condNotIn = not $ any (\c -> S.member c rNotIn) candidate

tryElim actualAnswer guess searchSpace = do
  let crit = makeGuess actualAnswer guess
  candidate <- searchSpace
  guard $ couldMatch crit candidate
  pure candidate

{-

  TODO:
  correct answer: crimp
  guess: m(yellow) i(yellow) m(grey) i(grey) c(yellow)

 -}
main :: IO ()
main = do
  answers <- lines <$> readFile "wordle-answers-alphabetical.txt"
  allowedGuesses <- lines <$> readFile "wordle-allowed-guesses.txt"
  let crit0 =
        Result
          { rCorrects = [Nothing, Nothing, Nothing, Nothing, Nothing]
          , rWrongPlaces = M.empty
          , rNotIn = S.fromList "fault"
          }
      crit1 =
        Result
          { rCorrects = [Nothing, Nothing, Nothing, Nothing, Nothing]
          , rWrongPlaces =
              M.fromList
                [ ('m', IS.fromList [0])
                , ('i', IS.fromList [1])
                , ('c', IS.fromList [4])
                ]
          , rNotIn = S.empty
          }
      crit2 =
        Result
          { rCorrects = [Nothing, Nothing, Nothing, Just 'm', Nothing]
          , rWrongPlaces =
              M.fromList
                [ ('p', IS.fromList [0])
                ]
          , rNotIn = S.fromList "yg"
          }
      crit3 =
        Result
          { rCorrects = [Nothing, Nothing, Just 'i', Just 'm', Just 'p']
          , rWrongPlaces = M.empty
          , rNotIn = S.fromList "sk"
          }
      searchSpace = do
        a <- answers
        guard $ couldMatch crit0 a
        guard $ couldMatch crit1 a
        guard $ couldMatch crit2 a
        -- guard $ couldMatch crit3 a
        pure a
      experiment = do
        guess <- searchSpace
        let alts = do
              answer <- searchSpace
              pure (length $ tryElim answer guess searchSpace)
        pure (guess, product (fmap fromIntegral alts :: [Integer]))
  let (_, z) = maximumBy (comparing snd) experiment
  print $ filter ((== z) . snd) experiment
