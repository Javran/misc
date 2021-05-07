module Unit_5
  ( main
  )
where

import Control.Monad
import qualified Data.Set as S
import System.Console.Terminfo
import Text.Printf

type SkipThenKeep = (Int, Int)

collectPoints :: SkipThenKeep -> [Double] -> S.Set Double
collectPoints (s, k) =
  S.fromList
    . take k
    . drop s

getSequence :: Double -> Double -> [Double]
getSequence r seed = iterate f seed
  where
    f x = r * x * (1 - x)

renderLine :: Int -> (Double, Double) -> S.Set Double -> String
renderLine cols (xLo, xHi) xs = fmap tr [0 .. cols -1]
  where
    xDist = xHi - xLo
    cols' = fromIntegral cols
    markedCols :: S.Set Int
    markedCols =
      S.fromList
        . fmap (\x -> round (cols' * (x - xLo) / xDist))
        . filter (\x -> x >= xLo && x <= xHi)
        . S.toList
        $ xs
    tr c = if S.member c markedCols then '*' else ' '

main :: IO ()
main = do
  term <- setupTermFromEnv
  let Just (rows, cols) = getCapability term ((,) <$> termLines <*> termColumns)
  printf "rows: %d, cols: %d\n" rows cols
  let (rLo, rHi) = (3.536511882631319, 3.593323318496883)
      xRange = (0.7765042979942693, 0.9140401146131805)
      wantRows = rows - 3
      step = (rHi - rLo) / fromIntegral (wantRows -1)
  forM_ (take wantRows [rLo, rLo + step ..]) $ \r -> do
    let t = collectPoints (100, 200) $ getSequence r 0.5
    putStrLn $ renderLine cols xRange t
  pure ()
