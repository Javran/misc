{
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module AlexNoWrapper where

import Token
import qualified Data.ByteString.Lazy.Char8 as BSLC

}

$digit = 0-9
$alpha = [a-zA-Z]


-- looks like this is how to comment in alex

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
    { \(_, _, xs, _) l -> pure (Int $ read $ BSLC.unpack $ BSLC.take l xs) }
  [\=\+\-\*\/\(\)]
    { \(_, _, xs, _) 1 -> pure (Sym $ BSLC.head xs) }
  $alpha [$alpha $digit \_ \']*
    { \(_, _, xs, _) l -> pure $ (Var $ BSLC.unpack $ BSLC.take l xs) }

{
type AlexInput = ()
alexGetByte () = Nothing
}
