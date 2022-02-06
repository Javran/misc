{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Study1
  ( main
  )
where

import Data.Fix (Fix (..))
import Data.Functor.Foldable
import Data.Monoid

{-
  Some basic examples.
 -}

{-
  `Base` is our bridge between recursive datatype and its
  non-recursive counterpart - there is an isomorphism between them.
 -}

listToListF :: [a] -> Fix (ListF a)
listToListF = ana \case
  [] -> Nil
  x : xs ->
    -- note the lack of recursion here - we do it one step at a time.
    Cons x xs

listFToList :: Fix (ListF a) -> [a]
listFToList = cata \case
  Nil -> []
  Cons x r -> x : r

{-
  Note that we are either "tearing one layer down" or
  "building one layer up" - those are exact `project` and `embed`
 -}
listToListF1 :: [a] -> Fix (ListF a)
listToListF1 = ana project

listFToList1 :: Fix (ListF a) -> [a]
listFToList1 = cata embed

{-
  `project` tears it down, `embed` builds it up - together
  we have an identity function on lists.
 -}
listId :: forall a. [a] -> [a]
listId = hylo (embed :: ListF a [a] -> [a]) project

{-

  A function of the form `f a -> a` is called an "f-algebra".

  The idea is that we write function on a non-recursive datatype,
  pass it to a combinator that does the rest.

  No recursive calls are made, instead, we are exposed to the intermediate
  results and just express how to construct another intermediate result
  given current context (which is the non-recursive datatype).

 -}

{-
  foldMap but expressed in terms of f-algebra
 -}
monoidAlg :: Monoid m => (a -> m) -> ListF a m -> m
monoidAlg toM = \case
  Nil -> mempty
  Cons a r -> toM a <> r

{-
  a working example for unfolding - https://en.wikipedia.org/wiki/Collatz_conjecture,

  this is a bit ugly in that we are manually maintaining a flag
  to tell us whether we should halt the sequence -
  1 => 1 is a fixpoint and we don't want the sequence to be infinite
  (although a case can be made that we could - in which case consumer
  is responsible for cutting this infinite list down to something finite).
 -}
collatzConjecture :: Int -> [Int]
collatzConjecture = ana step . (,True)
  where
    {-
      A function of the form `a -> f a` is then called a "f-coalgebra"
     -}
    step :: (Int, Bool) -> ListF Int (Int, Bool)
    step (x, cont)
      | not cont = Nil
      | x == 1 = Cons x (x, False)
      | odd x = Cons x (3 * x + 1, cont)
      | otherwise = Cons x (x `quot` 2, cont)

studyWith :: [Int] -> IO ()
studyWith xs = do
  let p desc s = putStrLn $ desc <> ": " <> show s
  p "Original list" xs
  p "ListF equivalent" do
    listToListF xs
  p "then convert back" do
    listFToList $ listToListF xs

  p "ListF equivalent" do
    listToListF1 xs
  p "then convert back" do
    listFToList1 $ listToListF1 xs

  p "listId" do
    listId xs
  p "monoidAlg with Sum" do
    cata (monoidAlg Sum) xs
  p "monoidAlg with Product" do
    cata (monoidAlg Product) xs
  p "Collatz conjecture" do
    collatzConjecture 39

main :: IO ()
main = studyWith [1, 2, 4, 24, 10, 8]
