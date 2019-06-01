{-# LANGUAGE
    OverloadedStrings
  #-}
module Main
  ( main
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T

gadgetUrl = "http://203.104.209.7/gadget_html5/js/kcs_const.js"

{-
  This project is an experiment to see,
  when given a large list of resource paths,
  can we spawn multiple threads and fetch them
  from different servers simultaneously.

  Some stuff will be from WhaleChan as the initial starting point.
 -}

-- known server names
serverNamesTable :: IM.IntMap T.Text
serverNamesTable = IM.fromList
  [ (1 , "横須賀鎮守府")
  , (2 , "呉鎮守府")
  , (3 , "佐世保鎮守府")
  , (4 , "舞鶴鎮守府")
  , (5 , "大湊警備府")
  , (6 , "トラック泊地")
  , (7 , "リンガ泊地")
  , (8 , "ラバウル基地")
  , (9 , "ショートランド泊地")
  , (10 , "ブイン基地")
  , (11 , "タウイタウイ泊地")
  , (12 , "パラオ泊地")
  , (13 , "ブルネイ泊地")
  , (14 , "単冠湾泊地")
  , (15 , "幌筵泊地")
  , (16 , "宿毛湾泊地")
  , (17 , "鹿屋基地")
  , (18 , "岩川基地")
  , (19 , "佐伯湾泊地")
  , (20 , "柱島泊地")
  ]


main :: IO ()
main = pure ()
