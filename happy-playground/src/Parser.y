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
%right in -- this one actually looks weird, probably for resolving `Exp in Exp`?
  -- example: let x = 1 in let y = 2 in 2
  -- which should mean: let x = 1 in (let y = 2 in 2), so it's right-associative?
  -- hm, need to look into shift/reduce a bit ...
  -- https://en.wikipedia.org/wiki/Shift-reduce_parser ?
%left '+' '-'
%left '*' '/'
%%


Exp :: { Exp }
  : let var '=' Exp in Exp
    { Let $2 $4 $6 }
  | Exp '+' Exp
    { Plus $1 $3 }
  | Exp '-' Exp
    { Minus $1 $3 }
  | Exp '*' Exp
    { Times $1 $3 }
  | Exp '/' Exp
    { Div $1 $3 }
  | '(' Exp ')'
    { Brack $2 }
  | int
    { Int $1 }
  | var
    { Var $1 }

{

parseError :: [Token] -> a
parseError _ = error "parse error"

data Exp
  = Let String Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Brack Exp
  | Int Int
  | Var String
  deriving (Show)

}
