{-# LANGUAGE
    OverloadedStrings
  , LambdaCase
  #-}
module Main
  ( main
  ) where

import qualified Data.Map.Strict as M
import Data.List
import System.Process
import System.Exit
import System.Environment
import Turtle

-- success as long as the given key matches exactly one result (by prefix)
uniqueLookup :: Eq ke => [ke] -> M.Map [ke] v -> Maybe v
uniqueLookup k m = case filter ((k `isPrefixOf`) . fst) $ M.toList m of
  [(_,v)] -> Just v
  _ -> Nothing

mainSwitch :: IO ()
mainSwitch = pure ()

mainInstall :: IO ()
mainInstall = pure ()

mainClean :: IO ()
mainClean = pure ()

subCmds :: M.Map String (IO ())
subCmds = M.fromList
  [ ("switch", mainSwitch)
  , ("install", mainInstall)
  , ("clean", mainClean)
  ]

{-
  Example of how to work together with current shell:

  https://github.com/Gabriel439/Haskell-Turtle-Library/issues/33

  Probably not exactly what we need (as it spawns a subshell)
  but is good enough for now.
 -}
mainCdTest :: IO ()
mainCdTest = do
    cd "/"
    shBin <- getEnv "SHELL"
    setEnv "KERNEL_TOOL" "Test"
    let p = (System.Process.proc shBin [])
            { std_in  = Inherit
            , std_out = Inherit
            , std_err = Inherit
            , delegate_ctlc = True
            }
    (_, _, _, ph) <- createProcess p
    _ <- waitForProcess ph
    pure ()

main :: IO ()
main = getArgs >>= \case
  [cmd] | Just action <- uniqueLookup cmd subCmds -> action
  _ -> do
    putStrLn $ "kernel-tool <" <> intercalate "|" (M.keys subCmds) <> ">"
    exitFailure
