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
    <++ (do
           xs <- munch1 isAlpha
           case xs of
             "let" -> pure TokenLet
             "in" -> pure TokenIn
             _ -> pure $ TokenVar xs)
  where
    t <~ ch = t <$ char ch

lexer :: (Token -> P a) -> P a
lexer k =
  lift (lexerP <* skipSpaces) >>= k

parseError :: Token -> P a
parseError _ = throwError "parse error"
