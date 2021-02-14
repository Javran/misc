{-# LANGUAGE OverloadedStrings #-}

module ExercismWizard.CommandParse
  ( getArgs
  , Command (..)
  , RawExercise (..)
  )
where

import Data.List.Split (splitOn)
import qualified Data.Text as T
import Options.Applicative
import Options.Applicative.Types
import qualified System.Environment as Env

data Command
  = CmdProxy [T.Text]
  | CmdTest RawExercise
  deriving (Show)

newtype RawExercise
  = RawExercise
      ( Maybe T.Text -- language name
      , Maybe T.Text -- exercise name
      )
  deriving (Show)

{-
  Parses a raw description of an exercise, allowed formats:

  - "<language>:<exercise>"
  - ":<exercise>"
  - "<exercise>"
  - ""

  Missing parts will be implied from current working directory,
  which is not handled by command line parser.

 -}
rawExercise :: ReadM RawExercise
rawExercise = do
  xs <- readerAsk
  RawExercise <$> case fmap T.pack $ splitOn ":" xs of
    ["", ""] -> pure (Nothing, Nothing)
    [e] -> pure (Nothing, Just e)
    ["", e] -> pure (Nothing, Just e)
    [l, e] -> pure (Just l, Just e)
    _ -> readerError "Invalid RawExercise format."

opts :: ParserInfo Command
opts =
  info
    (subparser
       (proxyCommand
          <> testCommand)
       <**> helper)
    (fullDesc
       <> header "Exercism Wizard - exercism workflow automation")
  where
    proxyCommand =
      command
        "proxy"
        (info
           (error "CmdProxy should not be reachable from within optparse-applicative framework.")
           (progDesc "Proxy all following arguments to exercism cli."))
    testCommand =
      command
        "test"
        (info
           (CmdTest
              <$> argument
                rawExercise
                (help
                   "Specifies language track and exercise name. \
                   \Format: `[lang:]exercise-name`, left any part blank to guess from current directory."
                   <> value (RawExercise (Nothing, Nothing))
                   <> metavar "EXERCISE")
              <**> helper)
           (fullDesc <> progDesc "Run test suite."))

parseArgs :: [String] -> ParserResult Command
parseArgs xs = case xs of
  "proxy" : args ->
    {-
      proxy subcommand is a special mode that passes everything after it as-is
     -}
    Success $ CmdProxy $ fmap T.pack args
  _ -> execParserPure defaultPrefs opts xs

getArgs :: IO Command
getArgs = parseArgs <$> Env.getArgs >>= handleParseResult
