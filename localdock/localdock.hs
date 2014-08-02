-- | see: http://stackoverflow.com/questions/25091562/how-can-i-organize-local-haskell-package-documents

import System.Directory
import System.IO
import System.Environment
import System.Exit
import System.Path
import System.FilePath.Posix

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import Text.Printf

-- | make markdown table row
makeTableRow :: String -> FilePath -> String
makeTableRow dirName htmlPath = intercalate "|" [ dirName
                                                , link "frames"
                                                , link "index"
                                                , link "doc-index"]
    where
        link s = printf "[%s](%s)" s $ htmlPath </> s ++ ".html"

scanAndMakeTable :: String -> IO [String]
scanAndMakeTable relDocPath = do
    (Just docPath) <- absNormPath' <$> getCurrentDirectory <*> pure relDocPath
    dirs <- getDirectoryContents docPath
    items <- liftM catMaybes
           . mapM (asHaskellPackage docPath)
           . sort $ dirs
    return $ headers1:headers2:map (uncurry makeTableRow) items
    where
        headers1 = "| " ++  intercalate " | " (words "Package Frames Contents Index") ++ " |"
        headers2 = intercalate " --- " $ replicate 5 "|"
        absNormPath' a p = addMissingRoot  <$> absNormPath a p
        -- sometimes the leading '/' is missing in absNormPath results
        addMissingRoot s@('/':_) = s
        addMissingRoot s = '/' : s
        asHaskellPackage :: String -> String -> IO (Maybe (String,FilePath))
        asHaskellPackage docPath dirName = do
            -- a valid haskell package has a "haddock dir"
            -- in which we can at least find a file with ".haddock" as extension name
            b1 <- doesDirectoryExist haddockFileDir
            if b1
               then do
                   b2 <- any ((== ".haddock") . takeExtension)
                             <$> getDirectoryContents haddockFileDir
                   return $ if b2 then Just (dirName,haddockFileDir) else Nothing
               else return Nothing
            where
                -- guess haddock dir
                haddockFileDir = docPath </> dirName </> "html"

main :: IO ()
main = do
    args <- getArgs
    case args of
      [docPath'] -> scanAndMakeTable docPath' >>= putStrLn . unlines
      _ -> help
    where
        help = hPutStrLn stderr "Usage: <program> <path-to-packages>"
            >> exitFailure
