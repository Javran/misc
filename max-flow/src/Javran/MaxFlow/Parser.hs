{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.MaxFlow.Parser
  ( NetworkRep (..)
  , parseFromRaw
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Attoparsec.Text as P
import Data.Char
import qualified Data.Text as T
import Javran.MaxFlow.Types

data RawLine
  = RComment T.Text
  | RProblem Int Int
  | RNodeDesc Int Bool {- true for source -}
  | RArc (Int, Int) Int
  deriving (Show)

{-
  The format is specified in: http://lpsolve.sourceforge.net/5.5/DIMACS_maxf.htm

  in addition, lines with just spaces are treated as if it's an empty comment line,
  this behavior is not specified by the format, but we do have those lines from generated examples.
 -}
{-
  parsing a line without consuming the '\n' in the end
  which might or might not (if it is the last line) present.
 -}
rawLine :: P.Parser RawLine
rawLine =
  emptyLine
    <|> commentLine
    <|> problemLine
    <|> nodeDescLine
    <|> arcLine
  where
    emptyLine = do
      _ <- P.takeWhile (\ch -> isSpace ch && ch /= '\n')
      next <- P.peekChar
      case next of
        Just '\n' -> pure $ RComment ""
        _ -> fail "invalid line"
    commentLine = do
      _ <- "c"
      next <- P.peekChar
      case next of
        Just '\n' -> pure $ RComment ""
        Just ' ' ->
          " "
            *> (RComment <$> P.takeWhile (/= '\n'))
        _ -> fail "invalid lookahead char."
    problemLine = do
      _ <- "p max "
      RProblem <$> (P.decimal <* " ") <*> P.decimal
    nodeDescLine = do
      _ <- "n "
      nId <- P.decimal <* " "
      isSource <- (True <$ "s") <|> (False <$ "t")
      pure $ RNodeDesc nId isSource
    arcLine = do
      _ <- "a "
      src <- P.decimal <* " "
      dst <- P.decimal <* " "
      cap <- P.decimal
      pure $ RArc (src, dst) cap

parseContent :: T.Text -> Either String [RawLine]
parseContent =
  P.parseOnly (rawLine `P.sepBy1` "\n" <* P.takeWhile isSpace)

parseNetwork :: StateT [RawLine] (Except String) NetworkRep
parseNetwork = do
  -- drop comments
  modify
    (filter
       (\case
          RComment {} -> False
          _ -> True))
  {-
    according to spec, first 3 lines (ignoring comments), must be:
    - one "p" line
    - two "n" lines marking a source and a sink.
   -}
  xs <- state $ splitAt 3
  case xs of
    [RProblem nrNodeCount nrArcCount, RNodeDesc v0 isSrc0, RNodeDesc v1 isSrc1]
      | isSrc0 /= isSrc1 -> do
        let (nrSource, nrSink) = if isSrc0 then (v0, v1) else (v1, v0)
        arcDescs <- state (\s -> (s, []))
        -- those must be arc lines
        let convert (RArc p cap) = pure (p, cap)
            convert t = throwError $ "not an arc: " <> show t
        nrArcs <- mapM convert arcDescs
        pure NetworkRep {nrNodeCount, nrArcCount, nrSource, nrSink, nrArcs}
    _ -> throwError $ "invalid initial input lines (comments ignored): " <> show xs

parseFromRaw :: T.Text -> Either String NetworkRep
parseFromRaw raw = runExcept $ do
  xs <- liftEither $ parseContent raw
  r <- runStateT parseNetwork xs
  case r of
    (v, []) -> pure v
    _ -> throwError "lines not fully consumed"
