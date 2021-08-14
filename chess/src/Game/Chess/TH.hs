module Game.Chess.TH where

import Language.Haskell.TH

{-
  Note: it would be nice to have type signatures on those names automatically,
  but we don't have that type info for arbitrary expressions.
 -}

destructList :: Int -> (Int -> String) -> Q Exp -> Q [Dec]
destructList l varName rhsE = do
  vs <- mapM (varP . mkName . varName) [0 .. l -1]
  rhs <- rhsE
  pure [ValD (ListP vs) (NormalB rhs) []]
