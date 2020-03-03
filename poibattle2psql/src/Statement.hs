{-# LANGUAGE OverloadedStrings #-}
module Statement where

import Data.Functor.Contravariant
import Data.Int
import Data.Text.Encoding (encodeUtf8)
import Hasql.Statement

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

import RecordScanner

{-
  TODO: Hasql.TH.
 -}

{-
  id: int8, same as time
  version: text
  type: text
  map: array of int2
  desciption: nullable text -- note: desc is a keyword.
  time: timestamptz
  fleet: jsonb
  packet: array of jsonb
 -}

createTable :: Statement () ()
createTable =
    Statement sql Encoders.noParams Decoders.noResult False
  where
    sql =
      "CREATE TABLE IF NOT EXISTS poi_battle_records (\
      \  id int8 PRIMARY KEY NOT NULL,\
      \  version text NOT NULL,\
      \  type text NOT NULL,\
      \  map int2 ARRAY NOT NULL,\
      \  description text,\
      \  time timestamptz NOT NULL,\
      \  fleet jsonb NOT NULL,\
      \  packet jsonb ARRAY NOT NULL\
      \)"

queryMissingRecords :: Statement [Int64] [Int64]
queryMissingRecords =
    Statement sql encoder decoder False
  where
    encoder = Encoders.param . Encoders.nonNullable . Encoders.foldableArray $ Encoders.nonNullable Encoders.int8
    decoder = Decoders.rowList . Decoders.column . Decoders.nonNullable $ Decoders.int8
    sql =
      "SELECT tmp.id FROM poi_battle_records AS rs\
      \  RIGHT JOIN (\
      \    SELECT * FROM UNNEST($1) AS id\
      \  ) AS tmp\
      \  ON rs.id = tmp.id \
      \  WHERE rs.id IS NULL"

-- TODO: test.
insertBattleRecord :: Statement BattleRecord ()
insertBattleRecord =
    Statement sql encoder decoder False
  where
    nNulParam = Encoders.param . Encoders.nonNullable
    encoder =
      (brId >$< nNulParam Encoders.int8)
      <> (brVersion >$< nNulParam Encoders.text)
      <> (brType >$< nNulParam Encoders.text)
      <> (brMap >$< nNulParam (Encoders.foldableArray (Encoders.nonNullable Encoders.int2)))
      <> (brDesc >$< Encoders.param (Encoders.nullable Encoders.text))
      <> (brTime >$< nNulParam Encoders.timestamptz)
      <> (brFleet >$< nNulParam Encoders.jsonb)
      <> (brPacket >$< nNulParam (Encoders.foldableArray (Encoders.nonNullable Encoders.jsonb)))
    decoder = Decoders.noResult
    sql =
      "INSERT INTO poi_battle_records\
      \  (id, version, type, map, description, time, fleet, packet)\
      \  VALUES ($1, $2, $3, $4, $5, $6, $7, $8)\
      \  ON CONFLICT DO NOTHING"
