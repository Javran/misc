import Control.Monad
import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import System.Environment

import qualified Data.Map.Strict as M
import qualified System.IO.Strict as SIO

parseRaw :: String -> [(Char, Char)]
parseRaw = mapMaybe makePair . lines
    where makePair :: String -> Maybe (Char,Char)
          makePair raw
              | all isSpace raw = Nothing
              | head raw == '#' = Nothing
              | length xs == 2  = let (y1:y2:_) = xs
                                  in Just (head y1, head y2)
              | otherwise       = Nothing
              where xs = words raw

balance :: [(Char,Char)] -> String -> String
balance pairs = uncurry (++) . partition isRParens . go [] . filter isParens
    where
        isLParens = (`elem` M.keys dictLR)
        isRParens = (`elem` M.keys dictRL)
        isParens = (||) <$> isLParens <*> isRParens
        dictLR = M.fromList pairs
        dictRL = M.fromList . map (uncurry (flip (,))) $ pairs
        find' a alist = fromJust . M.lookup a $ alist
        go :: String -> String -> String
        go stack xs
            | null xs = map (\x -> fromJust . M.lookup x $ dictLR) stack
            | null stack = let (y:ys) = xs
                           in if isLParens y
                                then go (y:stack) ys
                                else find' y dictRL : go stack ys
            | otherwise =
                let (y:ys) = xs
                    (s:ss) = stack
                in if isLParens y
                     then go (y:stack) ys
                     else
                       if y == find' s dictLR
                         then go ss ys
                         else find' s dictLR : go ss xs

main :: IO ()
main = liftM listToMaybe getArgs
   >>= SIO.readFile . fromMaybe "Balance.txt"
   >>= (forever . doInteract) . parseRaw
    where doInteract dict = getLine >>= putStrLn . balance dict
