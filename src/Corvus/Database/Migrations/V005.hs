-- | Schema 4 -> 5: multiple audio devices per VM and template.
module Corvus.Database.Migrations.V005 (migration) where

import Corvus.Database.Migration
import Database.Persist.Sql (SqlPersistT, rawExecute)

migration :: Migration
migration = Migration 5 "Add VM and template audio devices" upgrade

upgrade :: DatabaseEngine -> SqlPersistT IO ()
upgrade DatabasePostgresql = do
  rawExecute "CREATE TABLE audio_device (id BIGSERIAL PRIMARY KEY, vm_id BIGINT NOT NULL REFERENCES vm(id), backend VARCHAR NOT NULL, options VARCHAR NOT NULL DEFAULT '')" []
  rawExecute "CREATE INDEX audio_device_vm_id_idx ON audio_device(vm_id)" []
  rawExecute "CREATE TABLE template_audio_device (id BIGSERIAL PRIMARY KEY, template_id BIGINT NOT NULL REFERENCES template_vm(id), backend VARCHAR NOT NULL, options VARCHAR NOT NULL DEFAULT '')" []
  rawExecute "CREATE INDEX template_audio_device_template_id_idx ON template_audio_device(template_id)" []
upgrade DatabaseSqlite = do
  rawExecute "CREATE TABLE audio_device (id INTEGER PRIMARY KEY AUTOINCREMENT, vm_id INTEGER NOT NULL REFERENCES vm(id), backend VARCHAR NOT NULL, options VARCHAR NOT NULL DEFAULT '')" []
  rawExecute "CREATE INDEX audio_device_vm_id_idx ON audio_device(vm_id)" []
  rawExecute "CREATE TABLE template_audio_device (id INTEGER PRIMARY KEY AUTOINCREMENT, template_id INTEGER NOT NULL REFERENCES template_vm(id), backend VARCHAR NOT NULL, options VARCHAR NOT NULL DEFAULT '')" []
  rawExecute "CREATE INDEX template_audio_device_template_id_idx ON template_audio_device(template_id)" []
