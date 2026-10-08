{-# LANGUAGE OverloadedStrings #-}

-- | Schema 10 -> 11: identities for conditional image imports and builds.
module Corvus.Database.Migrations.V011 (migration) where

import Corvus.Database.Migration
import Database.Persist.Sql (SqlPersistT, rawExecute)

migration :: Migration
migration = Migration 11 "Record image import and build identities" upgrade

upgrade :: DatabaseEngine -> SqlPersistT IO ()
upgrade engine = do
  let pk = case engine of
        DatabasePostgresql -> "BIGSERIAL PRIMARY KEY"
        DatabaseSqlite -> "INTEGER PRIMARY KEY AUTOINCREMENT"
      fk = case engine of
        DatabasePostgresql -> "BIGINT"
        DatabaseSqlite -> "INTEGER"
  rawExecute ("CREATE TABLE disk_image_import_identity (id " <> pk <> ", disk_image_id " <> fk <> " NOT NULL REFERENCES disk_image(id) ON DELETE RESTRICT ON UPDATE RESTRICT, algorithm VARCHAR NOT NULL, digest VARCHAR NOT NULL, target VARCHAR NOT NULL, import_url VARCHAR NULL, CONSTRAINT unique_disk_image_import_identity UNIQUE (disk_image_id))") []
  rawExecute ("CREATE TABLE disk_image_build_identity (id " <> pk <> ", disk_image_id " <> fk <> " NOT NULL REFERENCES disk_image(id) ON DELETE RESTRICT ON UPDATE RESTRICT, fingerprint VARCHAR NOT NULL, inputs VARCHAR NOT NULL, CONSTRAINT unique_disk_image_build_identity UNIQUE (disk_image_id))") []
