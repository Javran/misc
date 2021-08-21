{-# LANGUAGE OverloadedStrings #-}

module Game.Chess.Fen where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Char
import Data.Monoid
import Data.Word
import Game.Chess.Coord
import Game.Chess.Types
import qualified Data.ByteString as BS
import Control.Monad

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

rawStandardBoard = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
justBoard =  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

parseTest = parseOnly (do
                      l0 <- rankP
                      ls <- replicateM 7 (char '/' *> rankP)
                      pure $ l0 : ls
                      ) justBoard

pElemP :: Parser (Sum Word8, [Maybe (Color, PieceType)])
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

rankP :: Parser (EightElems (Maybe (Color, PieceType)))
rankP = do
  (Sum 8, es) <- mconcat <$> many1 pElemP
  pure es
