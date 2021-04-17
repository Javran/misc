module PuzzleSpec where

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
      hexSplit 4 (['A'..'Z'] <> ['a'..'k'])
        `shouldBe` [ "ABCD"
                   , "EFGHI"
                   , "JKLMNO"
                   , "PQRSTUV"
                   , "WXYZab"
                   , "cdefg"
                   , "hijk"
                   ]
    specify "size = 3" $
      hexSplit 3 ['A'..'S']
        `shouldBe` [ "ABC"
                   , "DEFG"
                   , "HIJKL"
                   , "MNOP"
                   , "QRS"
                   ]
