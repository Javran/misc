module Mtl (
  main,
) where

import Control.Monad.Trans.Cont
import Debug.Trace

t1 :: Cont w Integer
t1 =
  (-)
    <$> reset
      ( fmap
          (3 +)
          (shift (\_ -> pure ((*) 5 2)))
      )
    <*> pure 1

t1Extra :: Cont w Integer
t1Extra =
  (-)
    <$> reset
      ( fmap
          (3 +)
          ( shift
              ( \k -> do
                  {-
                    here k is `3 + {}`.
                    in addition to computing 5*2 inside,
                    we "insert another layer" between
                    reseting point and final result of this,
                    or `8 + {}`
                    resulting in
                    `8 + (3 + {5*2})` being final result for
                    this part of the computation.
                   -}
                  pure $ 8 + k (5 * 2)
              )
          )
      )
    <*> pure 1

t13 :: Cont w String
t13 =
  fst
    <$> reset do
      x <-
        shift
          ( \(_ :: String -> (String, String)) ->
              {-
                as if the whole `reset {}` is simply replaced
                by the expresion below:
               -}
              pure ("hi", "bye")
          )
      {-
        this part is unreachable as shift shortcuts this by
        "dropping the continuation"
       -}
      pure (x, x)

t3 :: forall w. w ~ Int => Cont Int Int
t3 =
  (+)
    <$> reset do
      (2 *) <$> shift \k ->
        {- k = 2 * {} - remember this is scoped up to reset -}
        pure (k (k 10))
    <*> pure 1

type DL a = [a] -> [a]

-- a stupid version of list reversal, just for demostration.
myRev :: [a] -> Cont (DL a) (DL a)
myRev xs = reset
  case xs of
    [] -> pure id
    y : ys -> do
      -- do it recursively
      ys' <- myRev ys
      shift \k -> do
        -- for whatever result we have, append [y] to it.
        pure $ k ys' . (y :)

timesC :: [Int] -> Cont Int Int
timesC =
  \case
    [] -> pure 1
    x : xs ->
      if x == 0
        then shift $ \_k ->
          -- one of them is 0, overall result is 0, no need to go further
          pure 0
        else do
          r <- timesC xs
          {-
            We should never be able to reach here if we found a zero,
            as the rest of the continuation is discarded.
           -}
          traceShow ("here" :: String, r) pure (x * r)

{-
  As an example of discarding continuation up to closest `reset` -
  not even any multiplications are done whenever 0 is found.
  This function could work with infinite lists if somewhere there is a 0.

  The caveat being we will have to do multiplication associating to right,
  so potentially we could build up a thunk and only reduce that down
  only after last element is reached.
 -}
times :: [Int] -> Int
times xs =
  -- notice where `reset` is placed.
  evalCont (reset $ do timesC xs)

{-
  TODO: still not sure if this can be done in Haskell?
  probably facing the same problem as in https://stackoverflow.com/q/72833519/315302
examplex = reset do
  a <- pure 3
  b <- shift (\k -> _)
  let tmp = a + b
  c <- pure 1
  pure $ tmp - c

 -}

main :: IO ()
main = do
  print $ evalCont t1
  print $ evalCont t1Extra

  print $ evalCont t13
  print $ evalCont t3

  print $ evalCont (myRev [1 :: Int, 2, 3]) []

  print $ times [1 .. 5]
  print $ times [1, 2, 0, 2, undefined]
  print $ times ([1, 2, 3] <> (0 : repeat 10))
