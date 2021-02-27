{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module GenCase
  ( genCase
  )
where

import Control.Applicative
import Data.Aeson
import Data.Char
import qualified Data.Text as T
import Deriving.Aeson
import Network.HTTP.Client
import Network.HTTP.Client.TLS

data ToLower

instance StringModifier ToLower where
  getStringModifier "" = ""
  getStringModifier (c : xs) = toLower c : xs

data CanonicalData = CanonicalData
  { cdExercise :: T.Text
  , cdComments :: [T.Text]
  , cdCases :: [CaseSet]
  }
  deriving (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier (StripPrefix "cd", ToLower)] CanonicalData

data CaseSet = CaseSet
  { csDescription :: T.Text
  , csCases :: [TestCase]
  }
  deriving (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier (StripPrefix "cs", ToLower)] CaseSet

data TestCase = TestCase
  { tcUuid :: T.Text
  , tcDescription :: T.Text
  , tcProperty :: T.Text
  , tcInput :: VlqSeq 'In Int
  , tcExpected :: CaseExpect Int
  }
  deriving (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier (StripPrefix "tc", ToLower)] TestCase

data Dir = In | Out

newtype VlqSeq (dir :: Dir) int = VlqSeq [int] deriving Show

instance FromJSON i => FromJSON (VlqSeq 'In i) where
  parseJSON = withObject "VlqInput" $ \v -> do
    xs <- v .: "integers"
    pure $ VlqSeq xs

deriving via [i] instance FromJSON i => FromJSON (VlqSeq 'Out i)

data CaseExpect i
  = CaseSuccess (VlqSeq 'Out i)
  | CaseFail T.Text
  deriving (Show)

instance FromJSON i => FromJSON (CaseExpect i) where
  parseJSON x =
    withObject
      "CaseExpect"
      (\v -> do
         msg <- v .: "error"
         pure $ CaseFail msg)
      x
      <|> (CaseSuccess <$> parseJSON x)

testRawUrl :: String
testRawUrl =
  "https://raw.githubusercontent.com/exercism/problem-specifications/\
  \main/exercises/variable-length-quantity/canonical-data.json"

genCase :: IO ()
genCase = do
  mgr <- newManager tlsManagerSettings
  req <- parseRequest testRawUrl
  resp <- httpLbs req mgr
  let raw = responseBody resp
      decoded = eitherDecode' @CanonicalData raw
  print decoded
