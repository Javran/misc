{-# LANGUAGE
    OverloadedStrings
  , OverloadedLabels
  , GeneralizedNewtypeDeriving
  , TypeApplications
  #-}
module Main
  ( main
  ) where

import Proto.Person as P
import Data.ProtoLens (defMessage, showMessage)
import qualified Proto.Coffee.Order as P
import Data.ProtoLens.Labels ()

import Lens.Micro
import Lens.Micro.Extras (view)

-- | Smart constructor for making an 'Americano'
--   with a price of €2.70
americano :: P.Coffee
americano =
  defMessage
      & #cost      .~ 2.70
      & #americano .~ defMessage

-- | Smart constructor for making an 'Latte'
--   with a price of €3.20
latte :: P.Coffee
latte =
  defMessage
      & #cost  .~ 3.20
      & #latte .~ defMessage

-- | Smart constructor for making an 'FlatWhite'
--   with a price of €3.30
flatWhite :: P.Coffee
flatWhite =
  defMessage
      & #cost      .~ 3.30
      & #flatWhite .~ defMessage

-- | Smart constructor for making an 'Americano'
--   with a price of €3.00
cappuccino :: P.Coffee
cappuccino =
  defMessage
      & #cost       .~ 3.00
      & #cappuccino .~ defMessage

-- | Smart constructor for making an 'Americano'
--   with a price of €3.50
mocha :: P.Coffee
mocha =
  defMessage
      & #cost  .~ 3.50
      & #mocha .~ defMessage

person :: P.Person
person =
    defMessage
    & #name .~ "Fintan"
    & #age .~ 24
    & #addresses .~ [address]
  where
    address :: P.Address
    address =
        defMessage
        & #street .~ "Yolo street"
        & #zipCode .~ "D8"

main :: IO ()
main = pure ()
