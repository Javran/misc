{-# LANGUAGE TemplateHaskell #-}

module Lib (
  main,
) where

import Common
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import TH (mkSorter)
import Test.QuickCheck

mySorts :: Ord a => V.Vector ([a] -> [a])
mySorts =
  V.fromList
    [ $(mkSorter gen 3) compare
    , $(mkSorter gen 4) compare
    , $(mkSorter gen 5) compare
    , $(mkSorter gen 6) compare
    , $(mkSorter gen 7) compare
    , $(mkSorter gen 8) compare
    ]

main :: IO ()
main =
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

{-
main = print $ three (3, 2, 1)

three :: forall v. Ord v => (v, v, v) -> (v, v, v)
three (a, b, c) =
  sw a b \a b -> sw b c \b c -> sw a b \a b -> (a,b,c)
  where
    sw :: forall r. v -> v -> (v -> v -> r) -> r
    sw u v f = if u <= v then f u v else f v u
 -}

-- four :: Ord a => [a] -> [a]
-- four = $(mkSorter 4 gen) compare
