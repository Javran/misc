{
module Parser where

import Lexer
}

%name calc
%tokentype { Token }
%error { parseError }

-- comments are suppored like this.

-- mapping between an identifier and Haskell value.
%token
  let  { TokenLet }
  in   { TokenIn }
  int  { TokenInt $$ }
  var  { TokenVar $$ }
  '='  { TokenEq }
  '+'  { TokenPlus }
  '-'  { TokenMinus }
  '*'  { TokenTimes }
  '/'  { TokenDiv }
  '('  { TokenOB }
  ')'  { TokenCB }

-- without those %% it will fail to compile,
-- so they actually play some roles here.

%%

-- low precedence goes first it seems.

Exp
  : let var '=' Exp in Exp
    { \p -> $6 (($2,$4 p):p)  } -- note that p :: [(str, num)], which is applied to the $variable.
  | Exp1
    { $1 }

-- no "let" in Exp1

Exp1
  : Exp1 '+' Term
    { \p -> $1 p + $3 p }
  | Exp1 '-' Term
    { \p -> $1 p - $3 p }
  | Term
    { $1 }

-- no "let", "+", or "-" in Term

Term
  : Term '*' Factor
    { \p -> $1 p * $3 p }
  | Term '/' Factor
    { \p -> $1 p `div` $3 p }
  | Factor
    { $1 }

-- leaving only literal, varable and parentheses in Factor.

Factor
  : int
    { \p -> $1 }
  | var
    { \p -> case lookup $1 p of
              Nothing -> error "no var"
              Just i -> i
    }
  | '(' Exp ')'
    { $2 }

{

parseError :: [Token] -> a
parseError _ = error "parse error"

data Exp
  = Let String Exp Exp
  | Exp1 Exp1
  deriving (Show)

data Exp1
  = Plus Exp1 Term
  | Minus Exp1 Term
  | Term Term
  deriving (Show)

data Term
  = Times Term Factor
  | Div Term Factor
  | Factor Factor
  deriving (Show)

data Factor
  = Int Int
  | Var String
  | Brack Exp
  deriving (Show)

}
