{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad.State
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Language.Haskell.TH

gMkSorter :: (Int -> [(Int, Int)]) -> Int -> ([Pat] -> Pat) -> ([Exp] -> Exp) -> Q Exp
gMkSorter mkPairs n mkP mkE = do
  let pairs = mkPairs n
  -- cmp :: a -> a -> Ordering
  cmp <- newName "cmp"

  swapper <- newName "sw"
  swapperVal <- [|\u v f -> if $(varE cmp) u v == GT then f v u else f u v|]

  ns0 <- replicateM n $ newName "v"
  let -- let sw = ... in <???>
      step0 :: Exp -> Exp
      step0 bd = LetE [ValD (VarP swapper) (NormalB swapperVal) []] bd
  (mkBody :: Exp -> Q Exp, ns :: [Name]) <- do
    nv <- liftIO $ V.unsafeThaw (V.fromList ns0)
    e <-
      foldM
        ( \(mk :: Exp -> Q Exp) (i, j) -> do
            iOld <- liftIO $ VM.unsafeRead nv i
            jOld <- liftIO $ VM.unsafeRead nv j
            iNew <- newName "v"
            jNew <- newName "v"
            liftIO do
              VM.unsafeWrite nv i iNew
              VM.unsafeWrite nv j jNew
            pure \(hole :: Exp) ->
              mk
                =<< [|
                  $(varE swapper)
                    $(varE iOld)
                    $(varE jOld)
                    (\ $(varP iNew) $(varP jNew) -> $(pure hole))
                  |]
        )
        (pure . step0)
        pairs
    nvFin <- liftIO $ V.unsafeFreeze nv
    pure (e, V.toList nvFin)

  r <- mkBody $ mkE $ VarE <$> ns
  pure $ LamE [VarP cmp, mkP $ fmap VarP ns0] r

mkSorterList, mkSorterTup :: (Int -> [(Int, Int)]) -> Int -> ExpQ
mkSorterList mkPairs n = gMkSorter mkPairs n ListP ListE
mkSorterTup mkPairs n = gMkSorter mkPairs n TupP (TupE . fmap Just)
