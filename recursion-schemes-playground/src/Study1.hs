module Study1
  ( main
  )
where

{-
  Some basic examples.
 -}

studyWith :: [Int] -> IO ()
studyWith xs = do
  putStrLn $ "Original list: " <> show xs
  pure ()

main :: IO ()
main = studyWith [1, 2, 4, 24, 10, 8]
