module ExercismWizard.CommandParse
  ( getArgs
  , Command(..)
  )
where

import qualified Data.Text as T
import Options.Applicative
import qualified System.Environment as Env

data Command
  = CmdProxy [T.Text]
  deriving (Show)

opts :: ParserInfo Command
opts =
  info
    (subparser proxyCommand
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
