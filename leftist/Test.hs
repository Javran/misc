{-# LANGUAGE TemplateHaskell #-}
module Main
where

import qualified Leftist as L

import Test.QuickCheck

isAscending :: Ord a => [a] -> Bool
-- zipWith _ [] (tail []) is safe
isAscending xs = and (zipWith (<=) xs (tail xs))

prop_Sort :: [Int] -> Bool
prop_Sort = isAscending . L.sort

prop_LeftishProp :: [Int] -> Bool
prop_LeftishProp = L.propertyHolds . L.fromList

return []

main :: IO Bool
main = $quickCheckAll
