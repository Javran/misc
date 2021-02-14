{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module ExercismWizard.Language
  ( LangTrack (..)
  , Language (..)
  , go
  , kotlin
  , rust
  , haskell
  , languages
  , parseLangTrack
  , langName
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Dhall

data LangTrack
  = Haskell
  | Kotlin
  | Rust
  | Go
  deriving (FromDhall, Generic, Show)

data Language = Language
  { track :: LangTrack
  , altNames :: [Text]
  , formatCommand :: Maybe Text
  , testCommand :: Maybe Text
  , lintCommand :: Maybe Text
  }
  deriving (FromDhall, Generic)

langName :: LangTrack -> Text
langName = T.toLower . T.pack . show

-- TODO: peekRepo seems common: "https://github.com/exercism/<lang>/tree/main/exercises/practice"

languages :: M.Map Text LangTrack
languages = M.fromListWith err $ do
  Language {track, altNames} <- [haskell, kotlin, rust, go]
  (,track) <$> langName track : altNames
  where
    err = error "Conflicting keys"

parseLangTrack :: Text -> Maybe LangTrack
parseLangTrack = (languages M.!?)

go :: Language
go =
  Language
    { track = Go
    , altNames = []
    , formatCommand = Just "go fmt"
    , testCommand = Just "go test -v --bench . --benchmem"
    , lintCommand = Just "golint"
    }

kotlin :: Language
kotlin =
  Language
    { track = Kotlin
    , altNames = ["kt"]
    , formatCommand = Nothing
    , testCommand = Nothing
    , lintCommand = Nothing
    }

rust :: Language
rust =
  Language
    { track = Rust
    , altNames = ["rs"]
    , formatCommand = Just "cargo fmt"
    , testCommand = Just "cargo test"
    , lintCommand = Just "cargo clippy --all-targets"
    }

haskell :: Language
haskell =
  Language
    { track = Haskell
    , altNames = ["hs"]
    , formatCommand = Nothing
    , testCommand = Just "stack test"
    , lintCommand = Nothing
    }
