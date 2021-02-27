{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GenCase
  ( genCase
  , Case(..)
  , EncDec(..)
  , PprHex(..)
  )
where

import Control.Applicative
import Data.Aeson
import Data.Either
import Data.List
import Data.Proxy
import qualified Data.Text as T
import Data.Word
import Deriving.Aeson
import GHC.TypeLits
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Text.Printf
import Vlq
import GHC.Exts (IsList)

data CanonicalData = CanonicalData
  { exercise :: T.Text
  , comments :: [T.Text]
  , cases :: [CaseSet]
  }
  deriving (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON '[] CanonicalData

data CaseSet = CaseSet
  { description :: T.Text
  , cases :: [TestCase]
  }
  deriving (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON '[] CaseSet

data TestCase = TestCase
  { uuid :: T.Text
  , description :: T.Text
  , property :: T.Text
  , input :: VlqSeq 'In Int
  , expected :: CaseExpect Int
  }
  deriving (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON '[] TestCase

data Dir = In | Out

newtype VlqSeq (dir :: Dir) int = VlqSeq [int] deriving (Show, Functor)

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

newtype PprHex a = PprHex [a] deriving (IsList, Eq, Foldable)

type family DisplayWidth i where
  DisplayWidth Word32 = 8
  DisplayWidth Word8 = 2

instance (PrintfArg i, Integral i, KnownNat w, w ~ DisplayWidth i) => Show (PprHex i) where
  show (PprHex xs) = "[" <> intercalate "," (fmap ppr xs) <> "]"
    where
      ppr = printf "0x%0*X" (fromInteger @i (natVal (Proxy :: Proxy w)))

data EncDec = Enc | Dec

type family CaseInputExpect (ed :: EncDec) where
  CaseInputExpect 'Enc = (PprHex Word32, PprHex Word8)
  CaseInputExpect 'Dec = (PprHex Word8, Either DecodeError (PprHex Word32))

data Case ty = Case
  { uuid :: T.Text
  , description :: T.Text
  , inputAndExpected :: CaseInputExpect ty
  }

deriving instance Show (CaseInputExpect ty) => Show (Case ty)

toCase :: TestCase -> Either (Case 'Enc) (Case 'Dec)
toCase TestCase {property, uuid, description, input = VlqSeq xs, expected} = case property of
  "encode" ->
    let CaseSuccess (VlqSeq ys) = expected
     in Left Case {uuid, description, inputAndExpected = (PprHex (fromIntegral <$> xs), PprHex (fromIntegral <$> ys))}
  "decode" ->
    Right
      Case
        { uuid
        , description
        , inputAndExpected =
            ( PprHex (fromIntegral <$> xs)
            , case expected of
                CaseSuccess (VlqSeq ys) -> Right $ PprHex (fromIntegral <$> ys)
                CaseFail msg ->
                  if msg == "incomplete sequence"
                    then Left IncompleteSequence
                    else Left $ error $ "unknown error: " <> T.unpack msg
            )
        }
  _ -> error "unknown property"

genCase :: IO ()
genCase = do
  mgr <- newManager tlsManagerSettings
  req <- parseRequest testRawUrl
  resp <- httpLbs req mgr
  let raw = responseBody resp
      Right CanonicalData {cases = caseSets} = eitherDecode' @CanonicalData raw
      (encCases, decCases) = partitionEithers $ toCase <$> concatMap (\CaseSet {cases} -> cases) caseSets
  print encCases
  print decCases
