{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Lib
  ( main
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Debug.Trace
import GHC.Stack.Types (HasCallStack)
import System.Directory
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

type Checkpoint = ([(String, Integer)], [String])

type Bests = ([(String, Integer)], S.Set String)

lookForBestInitialGuesses :: Int -> IO Bests
lookForBestInitialGuesses n = do
  answersPre <- lines <$> readFile "wordle-answers-alphabetical.txt"
  let answers = filter uniqueLetterOnly answersPre
      uniqueLetterOnly a = S.size (S.fromList a) == length a
      focusOnAnswers = True
  allowedGuesses <-
    if focusOnAnswers
      then pure []
      else lines <$> readFile "wordle-allowed-guesses.txt"
  let fullSearchSpace = S.fromList $ answers <> allowedGuesses
      checkpointFile = "best-initials.txt"
  (curBest, alreadyGuessed) <- do
    e <- doesFileExist checkpointFile
    if e
      then read @Checkpoint <$> readFile checkpointFile
      else do
        let emptyCp :: Checkpoint
            emptyCp = ([], [])
        writeFile checkpointFile (show emptyCp)
        pure emptyCp
  let alreadyGuessed' = S.fromDistinctAscList alreadyGuessed
      initBests :: Bests
      initBests = (curBest, alreadyGuessed')
  g <- newStdGen
  let guessSpace = fullSearchSpace S.\\ alreadyGuessed'
      actualGuessSpace = take n (shuffle' (S.toList guessSpace) (S.size guessSpace) g)
      writeBests bests = do
        let checkpoint' :: Checkpoint
            checkpoint' = second S.toAscList bests
        writeFile checkpointFile (show checkpoint')

  putStrLn $ "Search space: " <> show (S.size guessSpace)
  fix
    (\loop bests guesses -> case guesses of
       [] -> pure bests
       guess : guesses' -> do
         putStrLn $ "Trying " <> guess <> ", " <> show (length guesses') <> " more to go."
         let alts = do
               answer <- answers
               pure (length $ tryElim answer guess answers)
             score = sum (fmap fromIntegral alts :: [Integer])
             bests' = case bests of
               ([], xs) -> ([(guess, score)], S.insert guess xs)
               (bestAlts@((_, sc) : _), xs) -> case compare score sc of
                 LT -> ([(guess, score)], S.insert guess xs)
                 EQ -> ((guess, score) : bestAlts, S.insert guess xs)
                 GT -> (bestAlts, S.insert guess xs)
         writeBests $! bests'
         putStrLn $ "Score: " <> show score
         loop bests' guesses')
    initBests
    actualGuessSpace

{-
  Current best initial guesses:

  - [("taler",156811)]

 -}
main :: IO ()
main = do
  answers <- lines <$> readFile "wordle-answers-alphabetical.txt"
  allowedGuesses <- lines <$> readFile "wordle-allowed-guesses.txt"
  g <- newStdGen

  let p = consumeOrDie attemptedP
      _guessesExample =
        fmap
          p
          [ "roted | mnnnn"
          , "cairn | gngmn"
          ]
      guesses =
        fmap
          p
          []
      isInitialGuess = null guesses
      searchSpace = do
        a <- answers
        forM_ guesses \guess -> do
          guard $ couldMatch' guess a
        pure a
      allGuessSpace = answers <> allowedGuesses
      allGuessSpaceLen = length allGuessSpace
      experiment = do
        let guessSpace = take 200 $ shuffle' allGuessSpace allGuessSpaceLen g
        (i, guess) <- zip [0 :: Int ..] do
          if isInitialGuess
            then guessSpace
            else searchSpace
        let alts = traceShow i do
              answer <- searchSpace
              pure (length $ tryElim answer guess searchSpace)
        pure (guess, sum (fmap fromIntegral alts :: [Integer]))
  if isInitialGuess
    then do
      (bestAlts, searched) <- lookForBestInitialGuesses 30000
      putStrLn $ "Searched: " <> show (S.size searched)
      print bestAlts
    else do
      let (_, z) = minimumBy (comparing snd) experiment
      print $ filter ((== z) . snd) experiment
