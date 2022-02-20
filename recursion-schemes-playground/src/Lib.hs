module Lib
  ( main
  )
where

import qualified Study1
import qualified Catalan

{-
  TODO:

  - find example for zygo?
  - references:

    + https://github.com/willtim/recursion-schemes
    + https://stackoverflow.com/q/36851766/315302
    + https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

 -}
main = Catalan.main

main2 :: IO ()
main2 = do
  putStrLn "# Study1"
  Study1.main
  putStrLn ""
  putStrLn "# Study2"
  Catalan.main
  putStrLn ""
