-- | Schema 2 -> 3: an ejected CD-ROM drive has no disk image.
module Corvus.Database.Migrations.V003 (migration) where

import Corvus.Database.Migration
import Data.Text (Text)
import Database.Persist (PersistValue (..))
import Database.Persist.Sql (Single (..), SqlPersistT, rawExecute, rawSql)

migration :: Migration
migration = Migration 3 "Allow empty CD-ROM drives" upgrade

upgrade :: DatabaseEngine -> SqlPersistT IO ()
upgrade DatabasePostgresql =
  rawExecute "ALTER TABLE drive ALTER COLUMN disk_image_id DROP NOT NULL" []
upgrade DatabaseSqlite = do
  -- SQLite cannot alter nullability in place. Preserve user-created indexes
  -- and the sequence high-water mark, including IDs of previously deleted rows.
  indexes <- rawSql "SELECT sql FROM sqlite_master WHERE type = 'index' AND tbl_name = 'drive' AND sql IS NOT NULL" []
  sequences <- rawSql "SELECT seq FROM sqlite_sequence WHERE name = 'drive'" []
  rawExecute
    ( "CREATE TABLE drive_new ("
        <> "id INTEGER PRIMARY KEY AUTOINCREMENT, "
        <> "vm_id INTEGER NOT NULL REFERENCES vm(id) ON DELETE RESTRICT ON UPDATE RESTRICT, "
        <> "disk_image_id INTEGER REFERENCES disk_image(id) ON DELETE RESTRICT ON UPDATE RESTRICT, "
        <> "interface VARCHAR NOT NULL, media VARCHAR, "
        <> "read_only BOOLEAN NOT NULL DEFAULT false, "
        <> "cache_type VARCHAR NOT NULL, discard BOOLEAN NOT NULL DEFAULT false, "
        <> "CONSTRAINT unique_drive UNIQUE (vm_id, disk_image_id))"
    )
    []
  rawExecute
    ( "INSERT INTO drive_new (id, vm_id, disk_image_id, interface, media, read_only, cache_type, discard) "
        <> "SELECT id, vm_id, disk_image_id, interface, media, read_only, cache_type, discard FROM drive"
    )
    []
  rawExecute "DROP TABLE drive" []
  rawExecute "ALTER TABLE drive_new RENAME TO drive" []
  mapM_ (\(Single sql) -> rawExecute (sql :: Text) []) indexes
  mapM_
    ( \(Single seqValue) -> do
        rawExecute "UPDATE sqlite_sequence SET seq = MAX(seq, ?) WHERE name = 'drive'" [PersistInt64 seqValue]
        rawExecute
          "INSERT INTO sqlite_sequence (name, seq) SELECT 'drive', ? WHERE NOT EXISTS (SELECT 1 FROM sqlite_sequence WHERE name = 'drive')"
          [PersistInt64 seqValue]
    )
    sequences
