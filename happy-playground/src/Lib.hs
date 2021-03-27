module Lib
  ( main
  )
where

import Control.Monad.Except
import Parser
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  let parser :: ReadP (Either String Exp)
      parser = runExceptT (lift skipSpaces *> calc)
      input = "   let x = -10 in (let z = 7 in z * z) + x * -1  "
  case readP_to_S parser input of
    [(v, "")] -> print v
    xs -> print xs
