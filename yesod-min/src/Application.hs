{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , ViewPatterns
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application where

import Add
import Foundation
import Home
import Yesod.Core

mkYesodDispatch "App" resourcesApp
