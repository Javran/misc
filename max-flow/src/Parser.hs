{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
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
      nc <- P.decimal <* " "
      ac <- P.decimal
      pure $ RProblem nc ac
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
