{-# LANGUAGE
    OverloadedLabels
  , DataKinds
  , FunctionalDependencies
  , FlexibleInstances
  , FlexibleContexts
  , ScopedTypeVariables
  , TypeFamilies
  #-}
module Main
  ( main
  ) where

import GHC.OverloadedLabels
import GHC.TypeLits


{-
  A exploration of OverloadedLabels.

  - https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/overloaded-labels
  - The following example is modified from:
    https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-OverloadedLabels

 -}

data Label (l :: Symbol) = Get

data Point = Point Int Int deriving Show
data Point2 = Point2 Int Int deriving Show

class Has a l b | a l -> b where
  from :: a -> Label l -> b

{- Type "Point" has a field "x" (a symbol), which is of type Int -}
instance Has Point "x" Int where
  from (Point x _) _ = x

{- Same, but defines "y" -}
instance Has Point "y" Int where
  from (Point _ y) _ = y

{-
  I won't say this is a good example, as fields are supposed to do little to none
  computation, but this example demostrates the flexibility.
 -}
instance Has Point "distance" Double where
  from (Point x y) _ = sqrt (fromIntegral $ x*x + y*y)

{-
  All of those above has nothing to do with Labels until the following one
  connects the dots:

  This is a bit annoying here: we have to replace a with Point because
  instance searching does not take into account constraints,
  so we end up finding this instance even if we are searching for something for Point2.
 -}
instance Has Point l b => IsLabel l (Point -> b) where
  {-
    here we need to define "fromLabel @l", the type is implicit, and the explicit form should be:

    fromLabel @l = \x -> from x (Get :: Label l)

   -}
  fromLabel x = from x (Get :: Label l)

{-
  Another example to show that it doesn't have to be a function.
 -}
instance IsLabel "stuff" String where
  fromLabel = "fff"

instance IsLabel "x" (Point2 -> Int) where
  fromLabel (Point2 x _) = x

instance IsLabel "y" (Point2 -> Int) where
  fromLabel (Point2 _ y) = y

instance IsLabel "distance" (Point2 -> Double) where
  fromLabel (Point2 x y) = sqrt (fromIntegral $ x*x + y*y)

example :: Int
example = #x (Point 1 2)

{-
  #distance (Point 3 4)
  ==> (fromLabel @"distance" :: (Point -> Double)) (Point 3 4)
      with constraint: IsLabel "distance" (Point -> Double)
  ==> from (Point 3 4) (Get :: Label "distance")
      with constraint: Has Point "distance" Double
  ==> sqrt (fromIntegral $ x*x + y*y) (x = 3, y = 4)
  ==> sqrt (fromIntegral 25)
  ==> 5.0
 -}
main :: IO ()
main = do
  print (#distance (Point 3 4))
  print (#distance (Point2 6 8) :: Double)
  print (#stuff :: String)
