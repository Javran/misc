{-# LANGUAGE TemplateHaskell #-}

module Lib (
  main,
) where

import Common
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import TH
import Test.QuickCheck

mySorts :: Ord a => V.Vector ([a] -> [a])
mySorts =
  V.fromList
    [ $(mkSorterList gen 3) compare
    , $(mkSorterList gen 4) compare
    , $(mkSorterList gen 5) compare
    , $(mkSorterList gen 6) compare
    , $(mkSorterList gen 7) compare
    , $(mkSorterList gen 8) compare
    ]

main :: IO ()
main = do
  print $ $(mkSorterTup gen 4) compare (2, 4, 1, 3 :: Int)
  quickCheck $ withMaxSuccess 10000 do
    l <- chooseInt (3, 8)
    let mySort' = mySorts V.! (l - 3)
    -- 0-1 principle
    xs <- replicateM l (chooseEnum (False, True))
    let ys = mySort' xs
    pure $ label ("l=" <> show l) $ and $ zipWith (<=) ys (tail ys)

mySort :: Ord a => [a] -> [a]
mySort xs = runST do
  v <- V.unsafeThaw (V.fromList xs)
  let n = VM.length v
  forM_ (gen n) \(i, j) -> do
    vI <- VM.unsafeRead v i
    vJ <- VM.unsafeRead v j
    when (vI > vJ) do
      VM.swap v i j

  mapM (VM.unsafeRead v) [0 .. n - 1]
