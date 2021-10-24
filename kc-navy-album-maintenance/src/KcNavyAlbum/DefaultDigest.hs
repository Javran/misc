{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module KcNavyAlbum.DefaultDigest where

import Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Deriving.Aeson
import Network.HTTP.Client
import System.Exit

data KcApiField

instance StringModifier KcApiField where
  getStringModifier = ("api_" <>)

type KcConvention = [CamelToSnake, KcApiField]

data KcMasterData = KcMasterData
  { mstSlotitem :: [KcSlotitem]
  , mstShipgraph :: [KcShipgraph]
  , mstShip :: [KcShip]
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier KcConvention]
          KcMasterData

data KcSlotitem = KcSlotitem
  { slotId :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "slotId" "id" : KcConvention)]
          KcSlotitem

data KcShipgraph = KcShipgraph
  { version :: NE.NonEmpty T.Text
  , filename :: T.Text
  , shipId :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "shipId" "id" : KcConvention)]
          KcShipgraph

data KcShip = KcShip
  { shipId :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "shipId" "id" : KcConvention)]
          KcShip

subCmdMain :: Manager -> String -> IO ()
subCmdMain mgr _cmdHelpPrefix = do
  req <- parseRequest "https://raw.githubusercontent.com/kcwiki/kancolle-data/master/api/api_start2.json"
  resp <- httpLbs req mgr
  parsed <- case Aeson.eitherDecode @KcMasterData (responseBody resp) of
    Left msg -> die ("parse error: " <> msg)
    Right r -> pure r
  mapM_ print (mstShip parsed)
  pure ()
