{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Population
  ( populations
  )
where

import Control.Applicative
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.RawString.QQ

popLine :: P.Parser (T.Text, Int)
popLine = do
  _ <- P.decimal @Int
  _ <- ": "
  stateName <- T.pack <$> many (P.satisfy (/= ','))
  _ <- ", Population "
  pop <- P.decimal
  pure (stateName, pop)

{-
  source: https://www.thegreenpapers.com/Census10/ApportionMath.phtml
 -}

populations :: M.Map T.Text Int
populations = M.fromList $ parsePop <$> T.lines rawPop
  where
    parsePop :: T.Text -> (T.Text, Int)
    parsePop raw = v
      where
        Right v = P.parseOnly popLine raw

rawPop :: T.Text
rawPop =
  T.stripStart
    [r|
1: Alabama, Population 4802982
2: Alaska, Population 721523
3: Arizona, Population 6412700
4: Arkansas, Population 2926229
5: California, Population 37341989
6: Colorado, Population 5044930
7: Connecticut, Population 3581628
8: Delaware, Population 900877
9: Florida, Population 18900773
10: Georgia, Population 9727566
11: Hawaii, Population 1366862
12: Idaho, Population 1573499
13: Illinois, Population 12864380
14: Indiana, Population 6501582
15: Iowa, Population 3053787
16: Kansas, Population 2863813
17: Kentucky, Population 4350606
18: Louisiana, Population 4553962
19: Maine, Population 1333074
20: Maryland, Population 5789929
21: Massachusetts, Population 6559644
22: Michigan, Population 9911626
23: Minnesota, Population 5314879
24: Mississippi, Population 2978240
25: Missouri, Population 6011478
26: Montana, Population 994416
27: Nebraska, Population 1831825
28: Nevada, Population 2709432
29: New Hampshire, Population 1321445
30: New Jersey, Population 8807501
31: New Mexico, Population 2067273
32: New York, Population 19421055
33: North Carolina, Population 9565781
34: North Dakota, Population 675905
35: Ohio, Population 11568495
36: Oklahoma, Population 3764882
37: Oregon, Population 3848606
38: Pennsylvania, Population 12734905
39: Rhode Island, Population 1055247
40: South Carolina, Population 4645975
41: South Dakota, Population 819761
42: Tennessee, Population 6375431
43: Texas, Population 25268418
44: Utah, Population 2770765
45: Vermont, Population 630337
46: Virginia, Population 8037736
47: Washington, Population 6753369
48: West Virginia, Population 1859815
49: Wisconsin, Population 5698230
50: Wyoming, Population 568300
|]
