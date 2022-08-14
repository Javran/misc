{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , TypeFamilies
  , ViewPatterns
  #-}

module Foundation where

import Yesod.Core

data App = App

mkYesodData "App" $(parseRoutesFile "routes.yesodroutes")

instance Yesod App
