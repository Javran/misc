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
  )
where

import qualified Data.Map.Strict as M
import Dhall

data LangTrack
  = Haskell
  | Kotlin
  | Rust
  | Go
  deriving (FromDhall, Generic, Show)

data Language = Language
  { name :: Text
  , track :: LangTrack
  , altNames :: [Text]
  , formatCommand :: Maybe Text
  , testCommand :: Maybe Text
  , lintCommand :: Maybe Text
  }
  deriving (FromDhall, Generic)

-- TODO: peekRepo seems common: "https://github.com/exercism/<lang>/tree/main/exercises/practice"

languages :: M.Map Text LangTrack
languages = M.fromListWith err $ do
  Language {track, name, altNames} <- [haskell, kotlin, rust, go]
  (,track) <$> name : altNames
  where
    err = error "Conflicting keys"

parseLangTrack :: Text -> Maybe LangTrack
parseLangTrack = (languages M.!?)

go :: Language
go =
  Language
    { name = "go"
    , track = Go
    , altNames = []
    , formatCommand = Just "go fmt"
    , testCommand = Just "go test -v --bench . --benchmem"
    , lintCommand = Just "golint"
    }

kotlin :: Language
kotlin =
  Language
    { name = "kotlin"
    , track = Kotlin
    , altNames = ["kt"]
    , formatCommand = Nothing
    , testCommand = Nothing
    , lintCommand = Nothing
    }

rust :: Language
rust =
  Language
    { name = "rust"
    , track = Rust
    , altNames = ["rs"]
    , formatCommand = Just "cargo fmt"
    , testCommand = Just "cargo test"
    , lintCommand = Just "cargo clippy --all-targets"
    }

haskell :: Language
haskell =
  Language
    { name = "haskell"
    , track = Haskell
    , altNames = ["hs"]
    , formatCommand = Nothing
    , testCommand = Just "stack test"
    , lintCommand = Nothing
    }
