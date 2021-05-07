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

renderLine :: Int -> (Double, Double) -> S.Set Double -> [Bool]
renderLine cols (xLo, xHi) xs = fmap tr [0 .. cols -1]
  where
    xDist = xHi - xLo
    cols' = fromIntegral cols
    markedCols :: S.Set Int
    markedCols =
      S.fromList
        . concatMap
          (\x ->
             [ round (cols' * (x - xLo) / xDist)
             | x >= xLo && x <= xHi
             ])
        . S.toList
        $ xs
    tr c = S.member c markedCols

renderIteratedFunction
  :: (Double -> Double -> Double)
  -> (Int, Int)
  -> ((Double, Double), (Double, Double))
  -> [[Bool]]
renderIteratedFunction mkFunc (rows, cols) ranges =
  fmap
    (\r ->
       let t = collectPoints (100, 200) $ getSequence r 0.5
        in renderLine cols xRange t)
    (take rows [rLo, rLo + step ..])
  where
    getSequence :: Double -> Double -> [Double]
    getSequence r seed = iterate f seed
      where
        f = mkFunc r
    (xRange, (rLo, rHi)) = ranges
    step = (rHi - rLo) / fromIntegral (rows -1)

main :: IO ()
main = do
  term <- setupTermFromEnv
  let Just (rows, cols) = getCapability term ((,) <$> termLines <*> termColumns)
  printf "rows: %d, cols: %d\n" rows cols
  let wantRows = rows - 3
      rendered =
        renderIteratedFunction
          (\r x -> r * x * (1 - x))
          (wantRows, cols)
          ( -- xRange
            (0.7765042979942693, 0.9140401146131805)
          , -- rRange
            (3.536511882631319, 3.593323318496883)
          )

  forM_ rendered $ \rs -> do
    putStrLn $ (\b -> if b then '*' else ' ') <$> rs
  pure ()
