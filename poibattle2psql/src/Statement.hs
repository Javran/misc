{-# LANGUAGE OverloadedStrings #-}
module Statement where

import Data.Text.Encoding (encodeUtf8)
import Hasql.Statement
import Data.Int

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

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

createTable :: T.Text -> Statement () ()
createTable tblName =
    Statement sql Encoders.noParams Decoders.noResult False
  where
    sql =
      "CREATE TABLE IF NOT EXISTS " <> encodeUtf8 tblName <> "(\
      \  id int8 PRIMARY KEY NOT NULL,\
      \  version text NOT NULL,\
      \  type text NOT NULL,\
      \  map int2 ARRAY NOT NULL,\
      \  desciption text,\
      \  time timestamptz NOT NULL,\
      \  fleet jsonb NOT NULL,\
      \  packet jsonb ARRAY NOT NULL\
      \)"

queryMissingRecords :: T.Text ->  Statement [Int64] [Int64]
queryMissingRecords tblName =
    Statement sql encoder decoder False
  where
    encoder = Encoders.param . Encoders.nonNullable . Encoders.foldableArray $ Encoders.nonNullable Encoders.int8
    decoder = Decoders.rowList . Decoders.column . Decoders.nonNullable $ Decoders.int8
    sql =
      "SELECT tmp.id FROM " <> encodeUtf8 tblName <> " AS rs\
      \  RIGHT JOIN (\
      \    SELECT * FROM UNNEST($1) AS id\
      \  ) AS tmp\
      \  ON rs.id = tmp.id \
      \  WHERE rs.id IS NULL"
