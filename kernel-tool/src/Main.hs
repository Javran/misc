{-# LANGUAGE
    OverloadedStrings
  , LambdaCase
  #-}
module Main
  ( main
  ) where

import Data.List
import System.Environment
import System.Exit

import qualified Data.Map.Strict as M

import CommandClean
import CommandInstall
import CommandSwitch

-- succeed as long as the given key matches exactly one result (by prefix)
uniqueLookup :: Eq ke => [ke] -> M.Map [ke] v -> Maybe v
uniqueLookup k m = case filter ((k `isPrefixOf`) . fst) $ M.toList m of
  [(_,v)] -> Just v
  _ -> Nothing

subCmds :: M.Map String (IO ())
subCmds = M.fromList
  [ ("switch", cmdSwitch)
  , ("install", cmdInstall)
  , ("clean", cmdClean)
  ]

main :: IO ()
main = getArgs >>= \case
  [cmd] | Just action <- uniqueLookup cmd subCmds -> action
  _ -> do
    putStrLn $ "kernel-tool <" <> intercalate "|" (M.keys subCmds) <> ">"
    exitFailure
