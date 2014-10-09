module Main where

import Data.Tree
import Data.Generics

exTree1 :: Tree Int
exTree1 = Node 1 [Node 2 [], Node 3 [], Node 4 [Node 5 [], Node 6 [Node 7 []]]]

printTree :: Show a => Tree a -> IO ()
printTree t = putStrLn (drawTree (fmap show t))

accumDirectChildren :: Tree Int -> Tree Int
accumDirectChildren (Node a s) = Node (a + sum (map rootLabel s)) s

main :: IO ()
main = printTree exTree1
    >> printTree (everywhere (mkT accumDirectChildren) exTree1)
    >> printTree (everywhere' (mkT accumDirectChildren) exTree1)
