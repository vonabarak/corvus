{-# LANGUAGE OverloadedStrings #-}

-- | Schema 12 -> 13: persist VM and template startup readiness budgets.
module Corvus.Database.Migrations.V013 (migration) where

import Corvus.Database.Migration
import Database.Persist.Sql (rawExecute)

migration :: Migration
migration = Migration 13 "Add VM startup timeouts" $ \_ -> do
  rawExecute "ALTER TABLE vm ADD COLUMN startup_timeout_sec BIGINT NOT NULL DEFAULT 120" []
  rawExecute "ALTER TABLE template_vm ADD COLUMN startup_timeout_sec BIGINT NOT NULL DEFAULT 120" []
