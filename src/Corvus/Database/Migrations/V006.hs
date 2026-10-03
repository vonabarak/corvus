-- | Schema 5 -> 6: persist the graphics adapter on VMs and templates.
module Corvus.Database.Migrations.V006 (migration) where

import Corvus.Database.Migration
import Database.Persist.Sql (SqlPersistT, rawExecute)

migration :: Migration
migration = Migration 6 "Add graphics adapter to VMs and templates" upgrade

upgrade :: DatabaseEngine -> SqlPersistT IO ()
upgrade _ = do
  rawExecute "ALTER TABLE vm ADD COLUMN graphics_adapter VARCHAR NOT NULL DEFAULT 'virtio-vga'" []
  rawExecute "ALTER TABLE template_vm ADD COLUMN graphics_adapter VARCHAR NOT NULL DEFAULT 'virtio-vga'" []
