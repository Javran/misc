module PuzzleSpec where

import Parser
import Puzzle
import Test.Hspec

{-
  make sure this works for:

  - square of size 3, mod 4
  - square of size 4, mod 4

  - hexagon of side length 4, mod 2
  - hexagon of side length 6, mod 6

 -}

spec :: Spec
spec = do
  describe "hexSplit" $ do
    specify "size = 4" $
      hexSplit 4 (['A' .. 'Z'] <> ['a' .. 'k'])
        `shouldBe` [ "ABCD"
                   , "EFGHI"
                   , "JKLMNO"
                   , "PQRSTUV"
                   , "WXYZab"
                   , "cdefg"
                   , "hijk"
                   ]
    specify "size = 3" $
      hexSplit 3 ['A' .. 'S']
        `shouldBe` [ "ABC"
                   , "DEFG"
                   , "HIJKL"
                   , "MNOP"
                   , "QRS"
                   ]
  describe "solvePuzzle" $ do
    specify "example" $ do
      let xs =
            [ [0, 1, 3, 4]
            , [4, 4, 2, 1, 3]
            , [0, 0, 2, 2, 1, 3]
            , [4, 5, 1, 3, 1, 1, 5]
            , [4, 0, 2, 4, 1, 5]
            , [0, 3, 1, 0, 2]
            , [3, 4, 5, 3]
            ]
      {-
        TODO: simulate and verify if the result is correct rather
        having to write out the solution
        (this adds a bit of flexibility also as
        this would allow alternative solutions as long as puzzle is solved.
       -}
      solvePuzzle (Puzzle 6 (PHexagon 4) xs)
        `shouldBe` Right
          [ [4, 1, 1, 3]
          , [4, 3, 2, 2, 2]
          , [3, 0, 0, 1, 0, 2]
          , [5, 0, 2, 2, 3, 1, 4]
          , [0, 3, 4, 3, 1, 0]
          , [0, 3, 5, 2, 1]
          , [0, 0, 0, 0]
          ]
