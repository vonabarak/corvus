{-# LANGUAGE OverloadedStrings #-}

module Corvus.Database.Migrations.V015 (migration) where

import Corvus.Database.Migration
import Database.Persist.Sql (rawExecute)

migration :: Migration
migration = Migration 15 "Add template visibility" $ \engine ->
  rawExecute
    ( case engine of
        DatabasePostgresql -> "ALTER TABLE template_vm ADD COLUMN hidden BOOLEAN NOT NULL DEFAULT FALSE"
        DatabaseSqlite -> "ALTER TABLE template_vm ADD COLUMN hidden BOOLEAN NOT NULL DEFAULT 0"
    )
    []
