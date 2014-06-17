{-# LANGUAGE TupleSections #-}

import Data.List
import Data.List.Utils
import Data.Maybe
import Control.Monad
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO

import qualified System.IO.Strict as SIO
import qualified Data.Map.Strict as M

data CmdOption = CmdOption
    { shortArg  :: Char    -- ^ short option character
    , longArg   :: String  -- ^ long option string
    , key       :: String  -- ^ will eventually generate a Map, here is the key
    , opt       :: String  -- ^ option argument
    , argDesc   :: String  -- ^ argument description
    , defVal    :: Maybe String -- ^ a default value
    } deriving (Show)

infoStrLn :: String -> IO ()
infoStrLn = hPutStrLn stderr

optCmds :: [CmdOption]
optCmds =
    [ CmdOption 'd' "directory" "dir"
                "<dir>" "boot directory" (Just "/boot")
    , CmdOption 'p' "version-str" "ver" "<version>"
                 "template kernel version" (Just "VER")
    , CmdOption 't' "template" "tmpl"
                "<template>" "template file" Nothing
    , CmdOption 'b' "begin-str" "begin"
                "<str>" "where template begins" (Just "BEGIN")
    , CmdOption 'e' "end-str" "end"
                "<str>" "where template ends" (Just "END")
    ]

defaultOpts :: M.Map String String
defaultOpts = M.fromList $ map makePair $ filter (isJust . defVal) optCmds
    where
        makePair co = (key co, fromJust (defVal co))

optDescrs :: [OptDescr (String, String)]
optDescrs = map makeOptDescr optCmds
    where
        makeOptDescr :: CmdOption -> OptDescr (String, String)
        makeOptDescr co = Option
                              [shortArg co]
                              [longArg co]
                              ( ReqArg (key co,) (opt co) )
                              makeDescription
            where
                makeDescription =
                      maybe (argDesc co)
                            ((argDesc co ++ ", default: ") ++)
                            (defVal co)

grubUpdater :: M.Map String String -> IO ()
grubUpdater conf = do
    -- the only required argument is the path to the template
    when (isNothing (M.lookup "tmpl" conf))
         (infoStrLn "template is missing" >> exitFailure)
    -- otherwise the arguments look good
    let getConf = (M.!) conf
        tmplPath = getConf "tmpl"
        beginStr = getConf "begin"
        endStr   = getConf "end"
        verStr   = getConf "ver"
        bootDir  = getConf "dir"

    -- prepare template
    contents <- liftM lines $ SIO.readFile tmplPath
    let (beforeTemplate,_:rest1) = span (/= beginStr) contents
        (templatePart, _:afterTemplate) = span (/= endStr) rest1
    -- detect vmlinuz files
    files <- getDirectoryContents bootDir
    let availableVers = sortBy (flip compare)
                      . map (drop (length "vmlinuz-"))
                      . filter ("vmlinuz" `isPrefixOf`)
                      $ files
        makeListItem version = map (replace verStr version) templatePart
    -- print to stdout
    mapM_ putStrLn beforeTemplate
    mapM_ (mapM_ putStrLn . makeListItem) availableVers
    mapM_ putStrLn afterTemplate

main :: IO ()
main = do
    result <- liftM (getOpt Permute optDescrs) getArgs
    resultDispatch result
    where
        printHelp = infoStrLn (usageInfo "The grub.conf updater, arguments:" optDescrs)
        resultDispatch (argPairs, nonOpt, errs)
            | not (null errs) = infoStrLn (head errs) >> printHelp >> exitFailure
            | not (null nonOpt) = printHelp >> exitFailure
            | otherwise = grubUpdater (M.union (M.fromList argPairs) defaultOpts)
