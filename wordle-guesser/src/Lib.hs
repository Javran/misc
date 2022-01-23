{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( main
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import Data.Either
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord
import Debug.Trace
import GHC.Stack.Types (HasCallStack)
import System.Random
import System.Random.Shuffle
import Text.ParserCombinators.ReadP hiding (get)

data Result = Result
  { rCorrects :: [Maybe Char]
  , rWrongPlaces :: [(Char, Int)]
  , rNotIn :: [] Char
  }
  deriving (Show, Eq)

makeGuess :: String -> String -> Result
makeGuess answer guess = Result {rCorrects, rWrongPlaces, rNotIn}
  where
    rCorrects = zipWith (\x y -> x <$ guard (x == y)) answer guess
    corrects = catMaybes rCorrects
    -- discharge exact matches
    answer' = answer \\ corrects

    collectStep :: State ([] Int, [] Char) (Maybe (Either Char (Char, Int)))
    collectStep = do
      (curWrongIndices, curAns) <- get
      case curWrongIndices of
        [] -> pure Nothing
        wi : wis -> do
          let wCh = guess !! wi
          if wCh `elem` curAns
            then do
              put (wis, delete wCh curAns)
              pure $ Just (Right (wCh, wi))
            else do
              put (wis, curAns)
              pure $ Just (Left wCh)

    collect acc = do
      r <- collectStep
      case r of
        Nothing -> pure (partitionEithers $ reverse acc)
        Just a -> collect (a : acc)
    (rNotIn, rWrongPlaces) = evalState (collect []) (wrongIndices, answer')
    wrongIndices =
      catMaybes $
        zipWith
          (\i m -> case m of
             Nothing -> Just i
             Just _ -> Nothing)
          [0 :: Int ..]
          rCorrects

couldMatch :: Result -> String -> String -> Bool
couldMatch result guess candidate = result == makeGuess candidate guess

tryElim :: [Char] -> String -> [] String -> [] String
tryElim actualAnswer guess searchSpace = do
  let crit = makeGuess actualAnswer guess
  candidate <- searchSpace
  guard $ couldMatch crit guess candidate
  pure candidate

data Attempted
  = Good Char
  | Misp Char
  | Nope Char

couldMatch' :: [Attempted] -> String -> Bool
couldMatch' attemptedResult candidate = couldMatch r attemptedWord candidate
  where
    r = Result {rCorrects, rWrongPlaces, rNotIn}
    rNotIn =
      mapMaybe
        (\case
           Nope v -> Just v
           _ -> Nothing)
        attemptedResult
    rWrongPlaces =
      catMaybes $
        zipWith
          (\i m -> case m of
             Misp c -> Just (c, i)
             _ -> Nothing)
          [0 ..]
          attemptedResult
    rCorrects =
      fmap
        (\case
           Good c -> Just c
           _ -> Nothing)
        attemptedResult
    attemptedWord =
      fmap
        (\case
           Good c -> c
           Misp c -> c
           Nope c -> c)
        attemptedResult

consumeAllWithReadP :: ReadP a -> String -> Maybe a
consumeAllWithReadP p xs = case readP_to_S (p <* eof) xs of
  [(v, "")] -> pure v
  _ -> Nothing

consumeOrDie :: HasCallStack => ReadP a -> String -> a
consumeOrDie p = fromJust . consumeAllWithReadP p

attemptedP :: ReadP [Attempted]
attemptedP = do
  cs <- replicateM 5 (satisfy isAsciiLower)
  _ <- between skipSpaces skipSpaces (char '|')
  let resultP =
        (Good <$ char 'g')
          <++ (Misp <$ char 'm')
          <++ (Nope <$ char 'n')
  mk <- replicateM 5 resultP
  pure $ zipWith ($) mk cs

{-
  Current best initial guesses:

  - [("roted",247957)]

 -}
main :: IO ()
main = do
  answers <- lines <$> readFile "wordle-answers-alphabetical.txt"
  allowedGuesses <- lines <$> readFile "wordle-allowed-guesses.txt"
  g <- newStdGen

  let p = consumeOrDie attemptedP
      _guessesExample =
        [ p "roted | mnnnn"
        , p "cairn | gngmn"
        ]
      guesses = []
      isInitialGuess = null guesses
      searchSpace = do
        a <- answers
        forM_ guesses \guess -> do
          guard $ couldMatch' guess a
        pure a
      allGuessSpace = answers <> allowedGuesses
      allGuessSpaceLen = length allGuessSpace
      experiment = do
        let guessSpace = take 40 $ shuffle' allGuessSpace allGuessSpaceLen g
        (i, guess) <- zip [0 :: Int ..] do
          if isInitialGuess
            then guessSpace
            else searchSpace
        let alts = traceShow i do
              answer <- searchSpace
              pure (length $ tryElim answer guess searchSpace)
        pure (guess, sum (fmap fromIntegral alts :: [Integer]))
  let (_, z) = minimumBy (comparing snd) experiment
  print $ filter ((== z) . snd) experiment
