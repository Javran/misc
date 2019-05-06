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

import Control.Lens

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
                   -> Either TransactionError P.CashPayment
processCashPayment amount payment
  | amount <= pay = pure $ payment & #amount %~ subtract amount
  | otherwise     = Left NotEnoughMoney
  where
    pay = payment ^. #amount

processCardPayment :: Float
                   -> P.CardPayment
                   -> Either TransactionError P.CardPayment
processCardPayment amount payment = do
      pinCheck
      balanceCheck
      pure $ payment & #account . #currentBalance %~ subtract amount
  where
    account = payment ^. #account
    pinCheck
      | account ^. #pinValidation == payment ^. #pin = pure ()
      | otherwise = Left InvalidPin

    balanceCheck
      | account ^. #currentBalance >= amount = pure ()
      | otherwise = Left NotEnoughMoney

totalCost :: Foldable f => f P.Coffee -> Float
totalCost = getSum . foldMap (Sum . (^. #cost))

addMilkToAmericano :: P.Coffee -> P.Coffee
addMilkToAmericano coffee =
    -- I guess the "id" is supposed to somehow add milk to Americano,
    -- otherwise we are just traversing Maybe making no changes at all.
    coffee & (#maybe'coffeeType . _Just . P._Coffee'Americano %~ id)

takeOrder :: Float
          -> P.Order
          -> Either TransactionError (Either P.CashPayment P.CardPayment)
takeOrder amount order =
    maybe (Left NotPreparedForThisPayment) processPayment $
        order ^. #maybe'paymentMethod
  where
    processPayment (P.Order'Card card) =
      Right <$> processCardPayment amount card
    processPayment (P.Order'Cash cash) =
      Left <$> processCashPayment amount cash

coffeeOrderExample :: IO ()
coffeeOrderExample = do
  -- just to show that "addMilkToAmericano" doesn't have effect
  -- on the message because the modification is effectively doing nothing
  putStrLn $ showMessage americano
  putStrLn $ showMessage (addMilkToAmericano americano)
  putStrLn $ showMessage (addMilkToAmericano latte)

  let order1Coffees = [americano, americano, flatWhite]
      totalCost1 = totalCost order1Coffees
  putStrLn $ "Two americans + a flat white will cost: " <> show totalCost1

  let order1 :: P.Order
      order1 = defMessage
               & #coffees .~ [americano, americano, flatWhite]
               & #cash . #amount .~ totalCost1
      account :: P.Account
      account = defMessage
                & #currentBalance .~ 0
                & #pinValidation .~ "123456"
      order2 = defMessage
               & #coffees .~ [americano, americano, flatWhite]
               & #card .~ (defMessage
                            & #pin .~ "123456"
                            & #account .~ ( account
                                            & #currentBalance %~ (+ 20)
                                          )
                          )

  case takeOrder totalCost1 order1 of
    Left err -> print err
    Right r  -> do
      putStr "Success: "
      putStrLn $ either showMessage showMessage r

  case takeOrder totalCost1 order2 of
    Left err -> print err
    Right r  -> do
      putStr "Success: "
      putStrLn $ either showMessage showMessage r
  putStrLn . showMessage $ order2

main :: IO ()
main = personExample >> coffeeOrderExample
