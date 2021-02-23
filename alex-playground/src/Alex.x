{
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Alex
 ( alexScanTokens
 ) where

import Token
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+ 
    ;
  "--".* 
    ;
  let
    { \s -> Let }
  in
    { \s -> In }
  $digit+
    { \s -> Int (read s) }
  [\=\+\-\*\/\(\)]
    { \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*
    { \s -> Var s }

{
-- Token definition is from another module so this part is empty.
}
