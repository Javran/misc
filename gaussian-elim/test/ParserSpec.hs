module ParserSpec where

import Parser
import Test.Hspec

spec :: Spec
spec =
  describe "fromRawString" $ do
    specify "example (square 4)" $
      fromRawString
        (unlines
           [ "4"
           , "square 4"
           , "3 1 3 2"
           , "0 0 3 3"
           , "3 2 3 0"
           , "3 0 2 0"
           ])
        `shouldBe` Just
          Puzzle
            { opMod = 4
            , pzType = PSquare 4
            , grid =
                [ [3, 1, 3, 2]
                , [0, 0, 3, 3]
                , [3, 2, 3, 0]
                , [3, 0, 2, 0]
                ]
            }
    specify "example (hexagon 4)" $
      fromRawString
        (unlines
           [ "6"
           , "hexagon 4"
           , "1 3 5 1"
           , "4 5 5 4 4"
           , "0 3 2 1 2 0"
           , "5 2 1 4 2 3 5"
           , "0 1 2 1 4 5"
           , "3 4 2 2 2"
           , "3 4 0 3"
           ])
        `shouldBe` Just
          Puzzle
            { opMod = 6
            , pzType = PHexagon 4
            , grid =
                [ [1, 3, 5, 1]
                , [4, 5, 5, 4, 4]
                , [0, 3, 2, 1, 2, 0]
                , [5, 2, 1, 4, 2, 3, 5]
                , [0, 1, 2, 1, 4, 5]
                , [3, 4, 2, 2, 2]
                , [3, 4, 0, 3]
                ]
            }
