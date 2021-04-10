{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parser where

import Control.Monad
import Data.Char
import Data.Functor
import Text.ParserCombinators.ReadP

{-

raw input format design:

- first line: modulo
- seccond line: "square" | "hexagon" followed by side length
- following lines are all space-separated integers

 -}

data PuzzleType
  = PSquare Int
  | PHexagon Int

data Puzzle = Puzzle
  { opMod :: Int
  , pzType :: PuzzleType
  , grid :: [[Int]]
  }

newlineP :: ReadP ()
newlineP = void (char '\n')

intP :: ReadP Int
intP = read <$> munch1 isDigit

puzzleTypeP :: ReadP PuzzleType
puzzleTypeP = do
  mk <- (PSquare <$ string "square") <++ (PHexagon <$ string "hexagon")
  _ <- char ' '
  side <- intP <* newlineP
  pure $ mk side

gridP :: PuzzleType -> ReadP [[Int]]
gridP = \case
  PSquare sz ->
    replicateM sz (intsOfLen sz)
  PHexagon sz ->
    let lens = [sz .. sz * 2 - 1] <> [sz * 2 -2, sz * 2 -3 .. sz]
     in mapM intsOfLen lens
  where
    intsOfLen l = do
      xs <- intP `sepBy` munch1 (== ' ')
      guard $ length xs == l
      munch (== ' ') *> newlineP
      pure xs

puzzleP :: ReadP Puzzle
puzzleP = do
  opMod <- intP <* newlineP
  pzType <- puzzleTypeP
  grid <- gridP pzType
  pure $ Puzzle {opMod, pzType, grid}

