module Main where

import Control.Applicative
import Data.Tree
import Data.Monoid
import Data.Generics

-- | a tree containing integers
exTree1 :: Tree Int
exTree1 = Node (1 :: Int) [Node 2 [], Node 3 [], Node 4 [Node 5 [], Node 6 [Node 7 []]]]

-- | exTree1 converted to a tree of strings
exTree2 :: Tree String
exTree2 = show <$> exTree1

printTree :: Show a => Tree a -> IO ()
printTree t = putStrLn (drawTree (fmap show t))

foldWithDirectChildren :: (a -> a -> a) -> a -> Tree a -> Tree a
foldWithDirectChildren f z (Node a s) = Node (a `f` foldl f z (map rootLabel s)) s

main :: IO ()
main = do
    printTree exTree1

    -- drawback: cannot deduce a type
    printTree (everywhere  (mkT (foldWithDirectChildren (+) 0 :: Tree Int -> Tree Int)) exTree1)
    printTree (everywhere' (mkT (foldWithDirectChildren (+) 0 :: Tree Int -> Tree Int)) exTree1)
    -- if type mismatches, nothing will happen
    -- (this might potentially be bad)

    printTree (everywhere' (mkT (foldWithDirectChildren (+) 0 :: Tree Int -> Tree Int)) exTree2)

    let plusStr a b
          | null a = b
          | null b = a
          | otherwise = a ++ "|" ++ b

    -- the different strategies
    printTree (everywhere  (mkT (foldWithDirectChildren plusStr "")) exTree2)
    printTree (everywhere' (mkT (foldWithDirectChildren plusStr "")) exTree2)
