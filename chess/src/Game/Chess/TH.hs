module Game.Chess.TH where

import Control.Monad
import Language.Haskell.TH

{-
  Note: it would be nice to have type signatures on those names automatically,
  but we don't have that type info for arbitrary expressions.
 -}

destructList :: Int -> (Int -> String) -> Q Exp -> Q Type -> Q [Dec]
destructList l varName rhsQ tyQ = do
  let ns = fmap (mkName . varName) [0 .. l -1]
  vs <- mapM (varP . mkName . varName) [0 .. l -1]
  rhs <- rhsQ
  ty <- tyQ
  pure (ValD (ListP vs) (NormalB rhs) [] : fmap (\n -> SigD n ty) ns)
