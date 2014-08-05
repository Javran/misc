{-# LANGUAGE TemplateHaskell #-}
module Main
where

import qualified Leftist as L

import Test.QuickCheck

-- | a list is ascending iff. xs[i] <= xs[i+1] holds for all valid i
isAscending :: Ord a => [a] -> Bool
-- zipWith _ [] (tail []) is safe
isAscending xs = and (zipWith (<=) xs (tail xs))

-- | a sorted list is an ascending list
prop_Sort :: [Int] -> Bool
prop_Sort = isAscending . L.sort

-- | test if the leftist tree property holds
prop_LeftishProp :: [Int] -> Bool
prop_LeftishProp = L.propertyHolds . L.fromList

return []

main :: IO Bool
main = $quickCheckAll
