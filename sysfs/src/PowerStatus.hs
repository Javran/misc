module PowerStatus
  ( Status(..)
  , statusP
  )
where

import Text.ParserCombinators.ReadP

data Status
  = Unknown
  | Charging
  | Discharging
  | NotCharging
  | Full
  deriving (Show)

statusP :: ReadP Status
statusP =
  Unknown <~ "Unknown"
    <++ Charging <~ "Charging"
    <++ Discharging <~ "Discharging"
    <++ NotCharging <~ "Not charging"
    <++ Full <~ "Full"
  where
    c <~ r = c <$ string r
