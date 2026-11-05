{-# LANGUAGE OverloadedStrings #-}

module Corvus.Database.Migrations.V014 (migration) where

import Corvus.Database.Migration
import Database.Persist.Sql (rawExecute)

migration :: Migration
migration = Migration 14 "Record apply disk identities" $ \engine -> do
  let pk = case engine of
        DatabasePostgresql -> "BIGSERIAL PRIMARY KEY"
        DatabaseSqlite -> "INTEGER PRIMARY KEY AUTOINCREMENT"
      fk = case engine of
        DatabasePostgresql -> "BIGINT"
        DatabaseSqlite -> "INTEGER"
  rawExecute ("CREATE TABLE disk_image_apply_identity (id " <> pk <> ", disk_image_id " <> fk <> " NOT NULL REFERENCES disk_image(id) ON DELETE RESTRICT ON UPDATE RESTRICT, fingerprint VARCHAR NOT NULL, inputs VARCHAR NOT NULL, CONSTRAINT unique_disk_image_apply_identity UNIQUE (disk_image_id))") []
