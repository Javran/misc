module Game.Chess.TH where

import Control.Monad
import Language.Haskell.TH

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
  vs <- mapM (varP . mkName . varName) [0 .. l -1]
  rhs <- rhsQ
  ty <- tyQ
  pure (ValD (ListP vs) (NormalB rhs) [] : fmap (\n -> SigD n ty) ns)
