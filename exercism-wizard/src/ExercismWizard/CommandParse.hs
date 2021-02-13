module ExercismWizard.CommandParse
  ( getArgs
  )
where

import qualified Data.Text as T
import Options.Applicative
import Options.Applicative.Types
import qualified System.Environment as Env

data Command
  = CmdProxy [T.Text]
  deriving (Show)

opts :: ParserInfo Command
opts = undefined

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
