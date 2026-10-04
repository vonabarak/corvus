module Corvus.Database.Migrations.V007 (migration) where

import Corvus.Database.Migration
import Database.Persist.Sql (SqlPersistT, rawExecute)

migration :: Migration
migration = Migration 7 "Add audio and network device models" upgrade

upgrade :: DatabaseEngine -> SqlPersistT IO ()
upgrade _ = do
  rawExecute "ALTER TABLE audio_device ADD COLUMN model VARCHAR NOT NULL DEFAULT 'virtio-sound'" []
  rawExecute "ALTER TABLE template_audio_device ADD COLUMN model VARCHAR NOT NULL DEFAULT 'virtio-sound'" []
  rawExecute "ALTER TABLE network_interface ADD COLUMN model VARCHAR NOT NULL DEFAULT 'virtio-net-pci'" []
  rawExecute "ALTER TABLE template_network_interface ADD COLUMN model VARCHAR NOT NULL DEFAULT 'virtio-net-pci'" []
