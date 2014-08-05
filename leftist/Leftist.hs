module Leftist
    ( LTree
    , merge
    , empty
    , null
    , singleton
    , insert
    , deleteMin
    , toList
    , toAscList
    , fromList
    , sort
    , propertyHolds
    )
where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), ViewL(..), viewl)
import Data.Function
import Prelude hiding (null)

data LTree a
    = Nil
    | Node !Int !a !(LTree a) !(LTree a)
      deriving (Show)

dist :: LTree a -> Int
dist Nil = -1
dist (Node d _ _ _) = d

value :: LTree a -> a
value Nil = error "value on Nil"
value (Node _ v _ _) = v

empty :: LTree a
empty = Nil

null :: LTree a -> Bool
null Nil = True
null _ = False

singleton :: a -> LTree a
singleton x = Node 0 x Nil Nil

merge :: Ord a => LTree a -> LTree a -> LTree a
merge Nil r = r
merge l Nil = l
merge ta tb = Node newDist v1 newL newR
    where
        (treeL,treeR) = if value ta <= value tb then (ta,tb) else (tb,ta)
        (Node _ v1 l1 r1) = treeL
        r1' = merge r1 treeR
        (newL,newR) = if dist r1' > dist l1 then (r1',l1) else (l1,r1')
        newDist = 1 + dist newR

insert :: Ord a => a -> LTree a -> LTree a
insert v = merge (singleton v)

deleteMin :: Ord a => LTree a -> (a, LTree a)
deleteMin Nil = error "empty tree"
deleteMin (Node _ v l r) = (v, merge l r)

toAscList :: Ord a => LTree a -> [a]
toAscList Nil = []
toAscList t =
    let (v,t') = deleteMin t
    in v:toAscList t'

toList :: LTree a -> [a]
toList Nil = []
toList (Node _ v l r) = v : ((++) `on` toList) l r

fromList :: Ord a => [a] -> LTree a
fromList = fromSeq . fmap singleton . Seq.fromList

fromSeq :: Ord a => Seq (LTree a) -> LTree a
fromSeq s = case viewl s of
    EmptyL -> Nil
    v1 :< seq1 -> case viewl seq1 of
        EmptyL -> v1
        v2 :< seq2 -> fromSeq (seq2 |> merge v1 v2)

sort :: Ord a => [a] -> [a]
sort = toAscList . fromList

propertyHolds :: Ord a => LTree a -> Bool
propertyHolds Nil = True
propertyHolds (Node d v l r) =
       ((&&) `on` propertyHolds) l r
    && ((>=) `on` dist) l r
    && (d == 1 + dist r)
    && (null l || v <= value l)
    && (null r || v <= value r)
