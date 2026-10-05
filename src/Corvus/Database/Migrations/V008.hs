module Corvus.Database.Migrations.V008 (migration) where

import Corvus.Database.Migration
import Database.Persist.Sql (SqlPersistT, rawExecute)

migration :: Migration
migration = Migration 8 "Add optional VirtIO device settings" upgrade

upgrade :: DatabaseEngine -> SqlPersistT IO ()
upgrade _ = mapM_ (`rawExecute` []) statements
  where
    statements =
      [ "ALTER TABLE vm ADD COLUMN vsock BOOLEAN NOT NULL DEFAULT TRUE"
      , "ALTER TABLE vm ADD COLUMN balloon BOOLEAN NOT NULL DEFAULT TRUE"
      , "ALTER TABLE vm ADD COLUMN rng BOOLEAN NOT NULL DEFAULT TRUE"
      , "ALTER TABLE template_vm ADD COLUMN vsock BOOLEAN NOT NULL DEFAULT TRUE"
      , "ALTER TABLE template_vm ADD COLUMN balloon BOOLEAN NOT NULL DEFAULT TRUE"
      , "ALTER TABLE template_vm ADD COLUMN rng BOOLEAN NOT NULL DEFAULT TRUE"
      ]
