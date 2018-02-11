import Data.Csv
import Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8 as BSC
import Data.Vector as Vec

-- let's just assume all the csv files we read has a header
main :: IO ()
main = print $ (decode NoHeader (BSC.pack "a,10,2,b") :: Either String (Vec.Vector Text))
