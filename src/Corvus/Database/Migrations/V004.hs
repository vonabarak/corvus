-- | Schema 3 -> 4: daemon-owned VM lifecycle fence tokens.
module Corvus.Database.Migrations.V004 (migration) where

import Corvus.Database.Migration
import Database.Persist.Sql (SqlPersistT, rawExecute)

migration :: Migration
migration = Migration 4 "Add VM lifecycle fence tokens" upgrade

upgrade :: DatabaseEngine -> SqlPersistT IO ()
upgrade DatabasePostgresql = do
  rawExecute "ALTER TABLE vm ADD COLUMN lifecycle_revision BIGINT NOT NULL DEFAULT 0" []
  rawExecute "ALTER TABLE vm ADD COLUMN runtime_generation BIGINT NULL" []
upgrade DatabaseSqlite = do
  rawExecute "ALTER TABLE vm ADD COLUMN lifecycle_revision INTEGER NOT NULL DEFAULT 0" []
  rawExecute "ALTER TABLE vm ADD COLUMN runtime_generation INTEGER NULL" []
