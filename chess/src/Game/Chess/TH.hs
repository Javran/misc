module Game.Chess.TH where

import Language.Haskell.TH

destructList :: Int -> (Int -> String) -> Q Exp -> Q [Dec]
destructList l varName rhsE = do
  vs <- mapM (varP . mkName . varName) [0 .. l -1]
  rhsN <- newName "rhs"
  rhs <- rhsE
  pure
    [ ValD
        (ListP vs)
        (NormalB (VarE rhsN))
        [ValD (VarP rhsN) (NormalB rhs) []]
    ]
