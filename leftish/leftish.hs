module Leftish
    ( BTree
    , merge
    , empty
    , singleton
    , insert
    , deleteMin
    , toList
    , toAscList
    , fromList
    )
where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), ViewL(..), viewl)
import Data.Function

-- import Control.Monad
-- import Control.Monad.Random hiding (fromList)

data BTree a
    = Nil
    | Node !Int !a !(BTree a) !(BTree a)
      deriving (Show)

dist :: BTree a -> Int
dist Nil = -1
dist (Node d _ _ _) = d

value :: BTree a -> a
value Nil = error "value on Nil"
value (Node _ v _ _) = v

empty :: BTree a
empty = Nil

singleton :: a -> BTree a
singleton x = Node 0 x Nil Nil

merge :: Ord a => BTree a -> BTree a -> BTree a
merge Nil r = r
merge l Nil = l
merge ta tb = Node newDist v1 newL newR
    where
        (treeL,treeR) = if value ta <= value tb then (ta,tb) else (tb,ta)
        (Node _ v1 l1 r1) = treeL
        r1' = merge r1 treeR
        (newL,newR) = if dist r1' > dist l1 then (r1',l1) else (l1,r1')
        newDist = 1 + dist newR

insert :: Ord a => a -> BTree a -> BTree a
insert v = merge (singleton v)

deleteMin :: Ord a => BTree a -> (a, BTree a)
deleteMin Nil = error "empty tree"
deleteMin (Node _ v l r) = (v, merge l r)

toAscList :: Ord a => BTree a -> [a]
toAscList Nil = []
toAscList t =
    let (v,t') = deleteMin t
    in v:toAscList t'

toList :: BTree a -> [a]
toList Nil = []
toList (Node _ v l r) = v : ((++) `on` toList) l r

fromList :: Ord a => [a] -> BTree a
fromList = fromSeq . fmap singleton . Seq.fromList

fromSeq :: Ord a => Seq (BTree a) -> BTree a
fromSeq s = case viewl s of
    EmptyL -> Nil
    v1 :< seq1 -> case viewl seq1 of
        EmptyL -> v1
        v2 :< seq2 -> fromSeq (seq2 |> merge v1 v2)

-- main :: IO ()
-- main = do
--      xs <- liftM (take 100000) $ getRandomRs (0 :: Int,1000000)
--      let lt = fromList xs
--          result = toAscList lt
--      print $ and $ zipWith (<=) result (tail result)
