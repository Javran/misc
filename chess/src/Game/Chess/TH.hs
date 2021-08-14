module Game.Chess.TH where

import Language.Haskell.TH

destructList :: Int -> (Int -> String) -> Q Exp -> Q [Dec]
destructList l varName rhsQ = do
  vs <- mapM (varP . mkName . varName) [0 .. l -1]
  rhs <- rhsQ
  pure [ValD (ListP vs) (NormalB rhs) []]
