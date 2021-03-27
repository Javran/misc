{
module Parser where

import Lexer
}

%name calc
%tokentype { Token }
%error { parseError }
%monad { P }
%lexer { lexer } { TokenEOF }

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
-- %right in -- this one actually looks weird, probably for resolving `Exp in Exp`?
-- example: let x = 1 in let y = 2 in 2
-- which should mean: let x = 1 in (let y = 2 in 2), so it's right-associative?
-- hm, need to look into shift/reduce a bit ...
-- https://en.wikipedia.org/wiki/Shift-reduce_parser ?
{-
  Update: the answer to "why do we need this `%right in`" stuff is in info table.
  If we comment it out, we'll see this in State 21:

State 21

	Exp -> let var '=' Exp in Exp .                     (rule 1)
	Exp -> Exp . '+' Exp                                (rule 2)
	Exp -> Exp . '-' Exp                                (rule 3)
	Exp -> Exp . '*' Exp                                (rule 4)
	Exp -> Exp . '/' Exp                                (rule 5)

	in             reduce using rule 1
	'+'            shift, and enter state 8
			(reduce using rule 1)

	'-'            shift, and enter state 9
			(reduce using rule 1)

	'*'            shift, and enter state 10
			(reduce using rule 1)

	'/'            shift, and enter state 11
			(reduce using rule 1)

	')'            reduce using rule 1
	%eof           reduce using rule 1

  Notice there are some "(reduce using rule 1)", which I assume are exactly the shift/reduce conflict
  Happy is talking about.

  say we have input: "let x = 1 in 2 _", it could have two interpretations:

  1. "(let x = 1 in 2) _"
  2. "let x = 1 in (2 _ ...)"

  here we want the second interpretation (think about "let x = 1 in 2 + x"),
  so we need "in" to have a lower precedence than '+' '-' '*' '/' in order to resolve those 4 conflicts.

 -}

%expect 0 -- assert on shift/reduce conflict
%right in
%left '+' '-'
%left '*' '/'
%left NEG
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
  | '-' Exp %prec NEG
    { Negate $2 }
  | int
    { Int $1 }
  | var
    { Var $1 }

{

data Exp
  = Let String Exp Exp
  | Negate Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Brack Exp
  | Int Int
  | Var String
  deriving (Show)

}
