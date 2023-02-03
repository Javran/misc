module Types where

import Control.Monad.Reader
import qualified Data.Text as T
import Data.Word
import System.Random.MWC

data Move = A | B

other :: Move -> Move
other = \case
  A -> B
  B -> A

{-
  note: pickMove always assume itself to be the "horizontal" one.
 -}
data Strategy = Strategy
  { sName :: T.Text
  , sPickMove :: Payoff -> (Move, Move) -> ReaderT GenIO IO Move
  }

{-
  Should be sufficient for us since max payoff for a tourney is 1600.
 -}
type Score = Word64

type Payoff = (Score, Score, Score, Score) -- aa, ab, ba, bb
