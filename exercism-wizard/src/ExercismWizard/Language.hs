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

data Action
  = Format
  | Test
  | Lint
  deriving (FromDhall, Generic, Eq, Ord)

data Language = Language
  { track :: LangTrack
  , altNames :: [Text]
  , actions :: M.Map Action Text -- TODO: Text can be further refined to proc / shell and whether fork() is necessary.
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
    , actions =
        M.fromList $
          [ (Format, "go fmt")
          , (Test, "go test -v --bench . --benchmem")
          , (Lint, "golint")
          ]
    }

kotlin :: Language
kotlin =
  Language
    { track = Kotlin
    , altNames = ["kt"]
    , actions = M.empty
    }

rust :: Language
rust =
  Language
    { track = Rust
    , altNames = ["rs"]
    , actions =
        M.fromList
          [ (Format, "cargo fmt")
          , (Test, "cargo test")
          , (Lint, "cargo clippy --all-targets")
          ]
    }

haskell :: Language
haskell =
  Language
    { track = Haskell
    , altNames = ["hs"]
    , actions =
        M.fromList
          [ (Test, "stack test")
          ]
    }
