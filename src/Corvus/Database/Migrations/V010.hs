{-# LANGUAGE OverloadedStrings #-}

-- | Schema 9 -> 10: multiple versions per name and mutable unique tags.
module Corvus.Database.Migrations.V010 (migration) where

import Control.Monad (forM_)
import Corvus.Database.Migration
import Data.Text (Text)
import Database.Persist (PersistValue (..))
import Database.Persist.Sql (Single (..), SqlPersistT, rawExecute, rawSql)

migration :: Migration
migration = Migration 10 "Introduce image tags and floating template selectors" upgrade

upgrade :: DatabaseEngine -> SqlPersistT IO ()
upgrade engine = do
  rawExecute "ALTER TABLE template_drive ADD COLUMN disk_tag VARCHAR" []
  rawExecute "UPDATE template_drive SET disk_name = COALESCE(disk_name, (SELECT name FROM disk_image WHERE id = disk_image_id)), disk_tag = 'latest', disk_image_id = NULL WHERE clone_strategy <> 'create' AND disk_image_id IS NOT NULL" []
  case engine of
    DatabasePostgresql -> rawExecute "ALTER TABLE disk_image DROP CONSTRAINT unique_disk_image_name" []
    DatabaseSqlite -> rebuildImages
  case engine of
    DatabasePostgresql -> rawExecute "ALTER TABLE disk_image_node ADD CONSTRAINT unique_disk_image_path_on_node UNIQUE (node_id, file_path)" []
    DatabaseSqlite -> rawExecute "CREATE UNIQUE INDEX unique_disk_image_path_on_node ON disk_image_node (node_id, file_path)" []
  let pk = case engine of
        DatabasePostgresql -> "BIGSERIAL PRIMARY KEY"
        DatabaseSqlite -> "INTEGER PRIMARY KEY AUTOINCREMENT"
  rawExecute ("CREATE TABLE disk_image_tag (id " <> pk <> ", name VARCHAR NOT NULL, tag VARCHAR NOT NULL, disk_image_id BIGINT NOT NULL REFERENCES disk_image(id) ON DELETE RESTRICT ON UPDATE RESTRICT, CONSTRAINT unique_disk_image_tag UNIQUE (name, tag))") []
  rawExecute "INSERT INTO disk_image_tag (name, tag, disk_image_id) SELECT name, 'latest', id FROM disk_image" []

-- The runner already owns a transaction with foreign keys enabled. Preserve
-- dependent rows while replacing the image table, rather than toggling PRAGMA.
rebuildImages :: SqlPersistT IO ()
rebuildImages = do
  indexes <- rawSql "SELECT sql FROM sqlite_master WHERE type = 'index' AND tbl_name = 'disk_image' AND sql IS NOT NULL" [] :: SqlPersistT IO [Single Text]
  sequences <- rawSql "SELECT seq FROM sqlite_sequence WHERE name = 'disk_image'" []
  forM_ ["build_cache_entry", "snapshot", "drive", "disk_image_node", "disk_image"] $ \table ->
    rawExecute ("CREATE TEMP TABLE image_upgrade_" <> table <> " AS SELECT * FROM " <> table) []
  forM_ ["build_cache_entry", "snapshot", "drive", "disk_image_node"] $ \table -> rawExecute ("DELETE FROM " <> table) []
  rawExecute "UPDATE disk_image SET backing_image_id = NULL" []
  rawExecute "DROP TABLE disk_image" []
  rawExecute "CREATE TABLE disk_image (id INTEGER PRIMARY KEY AUTOINCREMENT, name VARCHAR NOT NULL, format VARCHAR NOT NULL, size INTEGER, created_at TIMESTAMP NOT NULL, backing_image_id INTEGER REFERENCES disk_image(id) ON DELETE RESTRICT ON UPDATE RESTRICT, ephemeral BOOLEAN NOT NULL DEFAULT false)" []
  rawExecute "INSERT INTO disk_image SELECT id, name, format, size, created_at, NULL, ephemeral FROM image_upgrade_disk_image" []
  rawExecute "UPDATE disk_image SET backing_image_id = (SELECT backing_image_id FROM image_upgrade_disk_image old WHERE old.id = disk_image.id)" []
  forM_ ["disk_image_node", "drive", "snapshot", "build_cache_entry"] $ \table -> rawExecute ("INSERT INTO " <> table <> " SELECT * FROM image_upgrade_" <> table) []
  forM_ indexes $ \(Single sql) -> rawExecute sql []
  forM_ sequences $ \(Single sequenceValue) -> do
    rawExecute "INSERT INTO sqlite_sequence (name, seq) SELECT 'disk_image', ? WHERE NOT EXISTS (SELECT 1 FROM sqlite_sequence WHERE name = 'disk_image')" [PersistInt64 sequenceValue]
    rawExecute "UPDATE sqlite_sequence SET seq = MAX(seq, ?) WHERE name = 'disk_image'" [PersistInt64 sequenceValue]
  forM_ ["build_cache_entry", "snapshot", "drive", "disk_image_node", "disk_image"] $ \table -> rawExecute ("DROP TABLE image_upgrade_" <> table) []
