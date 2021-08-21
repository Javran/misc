{-# LANGUAGE OverloadedStrings #-}

module Game.Chess.Fen where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import Data.Char
import Data.Monoid
import Data.Word
import Game.Chess.Coord
import Game.Chess.Types

{-
  https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
 -}

data Record = Record
  { placement :: EightElems (EightElems (Maybe (Color, PieceType)))
  , activeColor :: Color
  , castling :: ([Side], [Side])
  , enPassantTarget :: Maybe LinearCoord
  , halfMove :: Int
  , fullMove :: Int
  }

{-
  Information of one sqaure: empty or there's something on it.
 -}
type Square = Maybe (Color, PieceType)

rawStandardBoard = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

justBoard = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

parseTest = parseOnly placementP justBoard

pElemP :: Parser (Sum Word8, [Square])
pElemP =
  choice
    [ "Pp" ~> Pawn
    , "Nn" ~> Knight
    , "Bb" ~> Bishop
    , "Rr" ~> Rook
    , "Qq" ~> Queen
    , "Kk" ~> King
    , do
        c <- satisfy (\ch -> ch >= '1' && ch <= '8')
        let cnt = ord c - ord '0'
        pure (Sum (fromIntegral cnt), replicate cnt Nothing)
    ]
  where
    [wRaw, bRaw] ~> pt = do
      color <-
        (White <$ char wRaw)
          <|> (Black <$ char bRaw)
      pure (1, [Just (color, pt)])
    _ ~> _ = error "unreachable"

rankP :: Parser (EightElems Square)
rankP = do
  (Sum 8, es) <- mconcat <$> many1 pElemP
  pure es

placementP :: Parser (EightElems (EightElems Square))
placementP = (:) <$> rankP <*> replicateM 7 (char '/' *> rankP)
