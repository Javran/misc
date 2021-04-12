{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Gaussian where

import Control.Monad.Random
import Data.List
import Data.List.HT
import qualified Data.List.NonEmpty as NE
import Data.Ratio
import qualified Puzzle as Pz

-- it is expected that each equation is of the same length.
upperTriangularStep :: [[Rational]] -> Maybe ([Rational], [[Rational]])
upperTriangularStep eqns = do
  let alts = do
        (e, es) <- removeEach eqns
        case NE.nonEmpty e of
          Nothing -> []
          Just ne@(hd NE.:| _) -> do
            guard $ hd /= 0
            pure (ne, es)
  case alts of
    [] ->
      if all null eqns
        then Nothing
        else upperTriangularStep (fmap (drop 1) eqns)
    (e@(hd NE.:| _), es) : _ -> do
      let e' = (/ hd) <$> NE.toList e
          elim :: [Rational] -> [Rational]
          elim [] = []
          elim (c : cs) =
            if c == 0
              then cs
              else zipWith (-) cs (tail $ fmap (* c) e')
      pure (e', fmap elim es)

upperTriangular = unfoldr upperTriangularStep

solveStep :: ([Rational], [[Rational]]) -> Maybe (Rational, ([Rational], [[Rational]]))
solveStep (_, []) = Nothing
solveStep (xs, hd : tl) = do
  let x = rhs - sum (zipWith (*) lhs xs)
      1 : ys = hd
      rhs = last ys
      lhs = init ys
  pure (x, (x : xs, tl))

mainE :: IO ()
mainE = do
  let sz = 4
      rndInt = getRandomR @IO @Int (-10, 10)
  matLhs <- replicateM sz (replicateM sz rndInt)
  assns <- replicateM sz rndInt
  let mat =
        fmap
          (\lhs ->
             let rhs = sum (zipWith (*) lhs assns)
              in lhs <> [rhs])
          matLhs
      ut = upperTriangular $ (fmap . fmap) fromIntegral mat
      r = reverse $ unfoldr solveStep ([], reverse ut)

  pure ()

-- INVARIANT: varCount >= length (head tri) (when tri not empty)

{-
  make matrix determined by assigning unknown vars to 0.
 -}
fillTriangular varCount tri
  | varCount <= 0 = tri
  | otherwise = case tri of
    [] -> (1 : replicate varCount 0) : fillTriangular (varCount -1) []
    hd : tl ->
      if length hd == varCount + 1
        then hd : fillTriangular (varCount -1) tl
        else (1 : replicate varCount 0) : fillTriangular (varCount -1) tri

main :: IO ()
main = do
  let tr orig i = init orig <> [if i == j then 6 else 0 | j <- [0 .. l -1]] <> [last orig]
      l = length Pz.hexExample
      mat = zipWith tr Pz.hexExample [0 ..]
      varCount = length (head mat) - 1
      filled = fillTriangular varCount (upperTriangular ((fmap . fmap) fromIntegral mat))
  print varCount
  mapM_ print $ Pz.hexSplit . take l . fmap ((`mod` 6) . round @_ @Int . fromRational @Double) $ reverse $ unfoldr solveStep ([], reverse filled)

