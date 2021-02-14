{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExercismWizard.Language
  ( LangTrack (..)
  , go
  , kotlin
  , rust
  )
where

import Dhall

data LangTrack
  = Haskell
  | Kotlin
  | Rust
  | Go

data Language = Language
  { name :: Text
  , peekRepo :: Maybe Text
  , formatCommand :: Maybe Text
  , testCommand :: Maybe Text
  , lintCommand :: Maybe Text
  }
  deriving (FromDhall, Generic)

go :: Language
go =
  Language
    { name = "go"
    , peekRepo = Just "https://github.com/exercism/go/tree/main/exercises/practice"
    , formatCommand = Just "go fmt"
    , testCommand = Just "go test -v --bench . --benchmem"
    , lintCommand = Just "golint"
    }

kotlin :: Language
kotlin =
  Language
    { name = "kotlin"
    , peekRepo = Just "https://github.com/exercism/kotlin/tree/main/exercises/practice"
    , formatCommand = Nothing
    , testCommand = Nothing
    , lintCommand = Nothing
    }

rust :: Language
rust =
  Language
    { name = "rust"
    , peekRepo = Just "https://github.com/exercism/rust/tree/main/exercises/practice"
    , formatCommand = Just "cargo fmt"
    , testCommand = Just "cargo test"
    , lintCommand = Just "cargo clippy --all-targets"
    }
