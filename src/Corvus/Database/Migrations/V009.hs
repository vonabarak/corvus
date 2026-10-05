{-# LANGUAGE OverloadedStrings #-}

-- | Schema 8 -> 9: capacities are signed 64-bit byte counts.
module Corvus.Database.Migrations.V009 (migration) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Corvus.Database.Migration
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist.Sql (Single (..), SqlPersistT, rawExecute, rawSql)

migration :: Migration
migration = Migration 9 "Store all capacities in bytes" upgrade

upgrade :: DatabaseEngine -> SqlPersistT IO ()
upgrade engine = forM_ columns $ \(table, old, new) -> do
  invalid <- rawSql ("SELECT COUNT(*) FROM " <> table <> " WHERE " <> old <> " > 8796093022207 OR " <> old <> " < -8796093022208") [] :: SqlPersistT IO [Single Int64]
  when (invalid /= [Single 0]) $ liftIO $ ioError $ userError "Size conversion would overflow signed 64-bit bytes"
  rawExecute ("ALTER TABLE " <> table <> " RENAME COLUMN " <> old <> " TO " <> new) []
  case engine of
    DatabasePostgresql -> rawExecute ("ALTER TABLE " <> table <> " ALTER COLUMN " <> new <> " TYPE BIGINT") []
    DatabaseSqlite -> pure ()
  rawExecute ("UPDATE " <> table <> " SET " <> new <> " = " <> new <> " * 1048576") []

columns :: [(Text, Text, Text)]
columns =
  [ ("node", "ram_mb_total", "ram_total")
  , ("node", "ram_mb_free", "ram_free")
  , ("vm", "ram_mb", "ram")
  , ("disk_image", "size_mb", "size")
  , ("snapshot", "size_mb", "size")
  , ("template_vm", "ram_mb", "ram")
  , ("template_drive", "size_mb", "size")
  ]
