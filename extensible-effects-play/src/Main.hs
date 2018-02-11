{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, RankNTypes #-}
module Main where

import Control.Monad
import Control.Monad.State

import Control.Eff
import qualified Control.Eff.State.Strict as Eff
import qualified Control.Eff.Lift as Eff

type SimpleBase = StateT [Int] IO

push :: Int -> SimpleBase ()
push v = do
    liftIO $ putStrLn $ "Pushing: " ++ show v
    modify (v:)

pop :: SimpleBase Int
pop = do
    t <- gets head
    modify tail
    liftIO $ putStrLn $ "Popping: " ++ show t
    return t

bump1 :: StateT Int SimpleBase ()
bump1 = modify succ

pushBump1 :: Int -> StateT Int SimpleBase ()
pushBump1 v = lift (push v) >> bump1

bump2 :: (Member (Eff.State Int) r) => Eff r ()
bump2 = Eff.modify (succ :: Int -> Int)

pushBump2 :: forall r .
             ( Member (Eff.State Int) r
             , SetMember Eff.Lift (Eff.Lift SimpleBase) r) =>
             Int -> Eff r ()
pushBump2 v = Eff.lift (push v) >> bump2

-- TODO: as transformer
-- TODO: as base
-- TODO: customized m

type EffBase r a = (Member (Eff.State Int) r, SetMember Eff.Lift (Eff.Lift IO) r) => Eff r a

pop' :: forall r. EffBase r Int
pop' = do
    (x:xs) <- Eff.get :: EffBase r [Int]
    Eff.modify tail
    return x

main :: IO ()
main = void $ runStateT (mapM_ push [1..10] >> replicateM 3 pop) []
