{
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Alex where

import Token
}

%wrapper "monadUserState-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+ 
    ;
  "--".* 
    ;
  let
    { \_ _ -> pure Let }
  in
    { \_ _ -> pure In }
  $digit+
    { \_ _ -> pure (Int 1) }
  [\=\+\-\*\/\(\)]
    { \_ _ -> pure (Sym ' ') }
  $alpha [$alpha $digit \_ \']*
    { \_ _ -> pure (Var "") }

{
type AlexUserState = ()

alexInitUserState :: AlexUserState
alexInitUserState = ()

alexEOF = pure EOF
}
