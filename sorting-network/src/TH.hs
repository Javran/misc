{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad.State
import qualified Data.IntMap.Strict as IM
import Language.Haskell.TH

mkSorter :: (Int -> [(Int, Int)]) -> Int -> ExpQ
mkSorter mkPairs n = do
  let pairs = mkPairs n
  -- cmp :: a -> a -> Ordering
  cmp <- newName "cmp"

  swapper <- newName "sw"
  swapperVal <- [|\u v f -> if $(varE cmp) u v == GT then f v u else f u v|]

  ns <- replicateM n $ newName "v"
  let s0 = IM.fromList $ zip [0 ..] ns
      -- let sw = ... in <???>
      step0 :: Exp -> Exp
      step0 bd = LetE [ValD (VarP swapper) (NormalB swapperVal) []] bd
  (mkBody :: Exp -> Q Exp, s) <-
    runStateT
      ( foldM
          ( \(mk :: Exp -> Q Exp) (i, j) -> do
              iOld <- gets (IM.! i)
              jOld <- gets (IM.! j)
              iNew <- lift $ newName "v"
              jNew <- lift $ newName "v"

              modify (IM.insert i iNew . IM.insert j jNew)
              pure \(hole :: Exp) ->
                mk =<< [|$(varE swapper) $(varE iOld) $(varE jOld) (\ $(varP iNew) $(varP jNew) -> $(pure hole))|]
          )
          (pure . step0)
          pairs
      )
      s0
  r <- mkBody $ ListE $ VarE . snd <$> IM.toAscList s
  pure $ LamE [VarP cmp, ListP $ fmap VarP ns] r
