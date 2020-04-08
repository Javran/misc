{-
  Solver for https://en.wikipedia.org/wiki/Takuzu

  Credit to game "0h h1" by Q42 for introducting me to this game.
  If you see a lot of mention of color red and blue instead
  of 0 and 1, that's why.
  When packing to binary bits, red is 0 and blue is 1.
 -}
module Main
  ( main
  ) where

exampleRaw :: [] ([] Char)
exampleRaw =
  [ "    rr  br  "
  , "      r  b b"
  , "  br    r  b"
  , " r r        "
  , "b     r b b "
  , "  b b     b "
  , " r  br  r   "
  , "r    r      "
  , "r   r   bb  "
  , "  r     b   "
  , "      r   rb"
  , "  r  r      "
  ]

{-
  Total number of valid cell placements in a single line
  can be calculated following https://oeis.org/A177790
 -}
gen 0 0 xs = [xs]
gen rCount bCount xs = case xs of
    x0:x1:_
      | x0 == x1 ->
        if x0 == 'r'
          then newB
          else newR
    _ -> newB <> newR
  where
    newB = do
      True <- [bCount > 0]
      gen rCount (bCount-1) ('b' : xs)
    newR = do
      True <- [rCount > 0]
      gen (rCount-1) bCount ('r' : xs)

main :: IO ()
main = do
  mapM_ (\c -> print . length $ gen c c []) [1,2,3,4,5,6]
