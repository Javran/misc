import Control.Monad
import Control.Monad.Random

data BTree a
    = Nil
    | Node Int !a (BTree a) (BTree a)
      deriving (Show)

dist :: BTree a -> Int
dist Nil = -1
dist (Node d _ _ _) = d

empty :: BTree a
empty = Nil

singleton :: a -> BTree a
singleton x = Node 0 x Nil Nil

merge :: Ord a => BTree a -> BTree a -> BTree a
merge Nil r = r
merge l Nil = l
merge ta@(Node _ a _ _) tb@(Node _ b _ _) =
    let (treeL,treeR) = if a <= b then (ta,tb) else (tb,ta)
        (Node _ v1 l1 r1) = treeL
        r1' = merge r1 treeR
        (newL,newR) = if dist r1' > dist l1 then (r1',l1) else (l1,r1')
        newDist = 1 + dist newR
    in Node newDist v1 newL newR

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

main :: IO ()
main = do
    xs <- liftM (take 500000) $ getRandomRs (0 :: Int,1000000)
    let lt = foldr insert empty xs
        result = toAscList lt
    print $ and $ zipWith (<=) result (tail result)
