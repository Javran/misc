module Game.Chess.TH where

import Language.Haskell.TH
import Data.Bits
import qualified Data.Vector as V

{-
  Usage: `bindList l varName <exp> <element type>`

  Expects a list of exactly `l` elements, bind them to variables
  and declare their type signatures.

  Note: it would be nice to have type signatures on those names automatically,
  but we might not have the type info at hand when doing TH.
 -}
bindList :: Int -> (Int -> String) -> Q Exp -> Q Type -> Q [Dec]
bindList l varName rhsQ tyQ = do
  let ns = fmap (mkName . varName) [0 .. l -1]
  bindingDec <-
    valD (listP (varP <$> ns)) (normalB rhsQ) []
  ty <- tyQ
  let tySigs = (\n -> SigD n ty) <$> ns
  pure (bindingDec : tySigs)

rankStrs, fileStrs :: V.Vector Char
rankStrs = V.fromListN 8 ['1' .. '8']
fileStrs = V.fromListN 8 ['a' .. 'h']

linearCoordName :: Int -> String
linearCoordName c = [fileStrs V.! file, rankStrs V.! rank]
  where
    rank = shiftR c 3
    file = c .&. 7
