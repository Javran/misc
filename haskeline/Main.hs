import System.Console.Haskeline

main :: IO ()
main = runInputT mySettings loop
    where
      mySettings = Settings (\(a,b) -> return ("ba",[Completion "WOWO" "wow" False])) Nothing False
      loop :: InputT IO ()
      loop = do
          minput <- getInputLine "% "
          case minput of
              Nothing -> return ()
              Just "quit" -> return ()
              Just input -> do outputStrLn $ "Input was: " ++ input
                               loop
