{-# LANGUAGE DeriveGeneric #-}
module Config where

import Dhall

data PsqlConfig
  = PsqlConfig
  { host :: Text
  , port :: Natural
  , user :: Text
  , password :: Text
  , database :: Text
  } deriving (Generic)

instance FromDhall PsqlConfig

data ProgConfig
  = ProgConfig
  { pcSqlConfig :: PsqlConfig
  , pcBattleDataPath :: FilePath
  , pcTableName :: Text
  } deriving (Generic)

instance FromDhall ProgConfig
