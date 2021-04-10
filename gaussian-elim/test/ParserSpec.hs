module ParserSpec where

import Parser
import Test.Hspec

spec :: Spec
spec =
  describe "fromRawString" $
    specify "examples" $
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
