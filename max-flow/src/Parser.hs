{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Attoparsec.Text as P
import Data.Char
import qualified Data.Text as T

data RawLine
  = RComment T.Text
  | RProblem Int Int
  | RNodeDesc Int Bool {- true for source -}
  | RArc (Int, Int) Int
  deriving (Show)

data Network = Network
  { nNodeCount :: Int
  , nArcCount :: Int
  , nSource :: Int
  , nSink :: Int
  , nArcs :: [((Int, Int), Int)]
  }
  deriving (Show)

{-
  parsing a line without consuming the '\n' in the end
  which might or might not (if it is the last line) present.
 -}
rawLine :: P.Parser RawLine
rawLine =
  commentLine <|> problemLine <|> nodeDescLine <|> arcLine
  where
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

parseNetwork :: StateT [RawLine] (Except String) Network
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
    [RProblem nNodeCount nArcCount, RNodeDesc v0 isSrc0, RNodeDesc v1 isSrc1]
      | isSrc0 /= isSrc1 -> do
        let (nSource, nSink) = if isSrc0 then (v0, v1) else (v1, v0)
        arcDescs <- state (\s -> (s, []))
        -- those must be arc lines
        let convert (RArc p cap) = pure (p, cap)
            convert t = throwError $ "not an arc: " <> show t
        nArcs <- mapM convert arcDescs
        pure Network {nNodeCount, nArcCount, nSource, nSink, nArcs}
    _ -> throwError "invalid initial input lines"

parseFromRaw :: T.Text -> Either String Network
parseFromRaw raw = runExcept $ do
  xs <- liftEither $ parseContent raw
  r <- runStateT parseNetwork xs
  case r of
    (v, []) -> pure v
    _ -> throwError "lines not fully consumed"
