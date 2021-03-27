module Lexer where

import Control.Monad.Except
import Data.Char
import Text.ParserCombinators.ReadP

type P = ExceptT String ReadP

data Token
  = TokenLet
  | TokenIn
  | TokenInt Int
  | TokenVar String
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenOB
  | TokenCB
  | TokenEOF
  deriving (Show)

lexerP :: ReadP Token
lexerP =
  (TokenEOF <$ eof)
    <++ TokenEq <~ '='
    <++ TokenPlus <~ '+'
    <++ TokenMinus <~ '-'
    <++ TokenTimes <~ '*'
    <++ TokenDiv <~ '/'
    <++ TokenOB <~ '('
    <++ TokenCB <~ ')'
    <++ (TokenInt . read <$> munch1 isDigit)
    <++ (TokenLet <$ string "let")
    <++ (TokenIn <$ string "in")
    <++ (
         -- quick and dirty. if variable starts with "let" or "in" this won't work.
         TokenVar <$> munch1 isAlpha)
  where
    t <~ ch = t <$ char ch

lexer :: (Token -> P a) -> P a
lexer k =
  lift (lexerP <* skipSpaces) >>= k

parseError :: Token -> P a
parseError _ = throwError "parse error"
