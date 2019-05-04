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
import Data.Monoid

import Lens.Micro
import Lens.Micro.Extras (view)

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

personExample :: IO ()
personExample = putStrLn . showMessage $ person

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

data TransactionError
  = NotEnoughMoney
  | InvalidPin
  | NotPreparedForThisPayment
  deriving (Eq, Show)

processCashPayment :: Float
                   -> P.CashPayment
                   -> Either TransactionError ()
processCashPayment amount payment
  | amount <= pay = pure ()
  | otherwise     = Left NotEnoughMoney
  where
    pay = payment ^. #amount

processCardPayment :: Float
                   -> P.CardPayment
                   -> Either TransactionError ()
processCardPayment amount payment =
    pinCheck >> balanceCheck
  where
    account = payment ^. #account
    pinCheck
      | account ^. #pinValidation == payment ^. #pin = pure ()
      | otherwise = Left InvalidPin

    balanceCheck
      | account ^. #currentBalance >= amount = pure ()
      | otherwise = Left NotEnoughMoney

totalCost :: Foldable f => f P.Coffee -> Float
totalCost = getSum . foldMap (Sum . view #cost)

addMilkToAmericano :: P.Coffee -> P.Coffee
addMilkToAmericano coffee =
    -- I guess the "id" is supposed to somehow add milk to Americano,
    -- otherwise we are just traversing Maybe making no changes at all.
    coffee & (#maybe'coffeeType . _Just . P._Coffee'Americano %~ id)

coffeeOrderExample :: IO ()
coffeeOrderExample = do
  putStrLn $ showMessage americano
  putStrLn $ showMessage (addMilkToAmericano americano)
  putStrLn $ showMessage (addMilkToAmericano latte)

main :: IO ()
main = personExample >> coffeeOrderExample
