module Main
  ( main
  ) where

{-
  Automatic solver for Android game "tents & trees"
 -}

{-
  Design: for now I think the most difficult part will be,
  given an image, to recognize board position and
  extract board configuration from it.
  good old matchTemplate should work, but here I want to see
  if there are better ways that is not sensitive to scaling
  so that a method works for 10x10 board will also work for 20x20 board.

  Questions:

  - how to extract and recognize trees and empty spaces?

    For simplicity, let's only work on a clean board (that is, no empty or tent marked).

    This is currently done based on the fact that all empty cells use the exact same color,
    so the region can be extracted with OpenCV's inRange function, after which we can floodFill
    to find boundaries of empty cells can use that to derive row and col coordinates of the board.

    Board's position (or we should say each cell's position) is straightforward to figure out
    after we derive bounding low/high for each cell's row and col.

    a reasonable (but not safe) assumption that we use here is that:

    + there will always be some empty spaces on board for each row and col.
    + every boundary will have at least one empty cell.

  - how to extract and recognize digits on board sides?

    This is now done by guessing: given first two cells of a row or column,
    we extend cell boundary in the other direction - as it turns out that
    all digits happen to be placed in that region.

    (TODO) Quick notes:

    - there are two different colors for digits:

      + one color is consistently used for unsatisfied digits
      + another color is consistently used for satisfied digits,
        and satistifed digits are trickier to recognoze as there's an extra check mark character
        that we want to exclude.

   - for unsatisfied digits, the plan is simple: run through inRange by filtering on just that unsatisfied color,
     the we can find boundary of it and extract those as samples

   - for satisfied digits, well since that row / col is already satisfied, we can derive what that number is from
     the board itself. The board parsing function might need some modification, but I believe we can entirely
     ignore the problem of recognizing digits from picture.

 -}

main :: IO ()
main = pure ()
