import qualified System.IO.Strict as SIO
import System.Environment
import Control.Applicative
import Data.Char
import Text.Printf

wordsFromDict :: FilePath -> IO [String]
wordsFromDict p = map (filter (not . isSpace)) . lines <$> SIO.readFile p

pruneDict :: Int -> [String] -> [String]
pruneDict l = (map . map) toLower . filter allLetter . filter ((== l) . length)
    where
        allLetter = all (\x -> x `elem` ['a'..'z']
                            || x `elem` ['A'..'Z'])

guessWords :: [String] -> [String] -> [String]
guessWords dict wheels = filter isWord $ sequence wheels'
    where
        wheels' = (map . map) toLower wheels
        dict' = pruneDict (length wheels) dict
        isWord = (`elem` dict')

main :: IO ()
main = do
    (dpath:_) <- getArgs
    d <- wordsFromDict dpath
    let guesses = guessWords d wheels
    _ <- printf "%d words loaded.\n" (length $ pruneDict 5 d)
    mapM_ putStrLn guesses
    printf "%d valid words found.\n" (length guesses)
    where
        wheels = words "swbpfmdtal porilcetna erilanutos ldaosknrte lsnthyder"
