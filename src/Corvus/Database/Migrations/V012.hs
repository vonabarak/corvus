{-# LANGUAGE OverloadedStrings #-}

-- | Schema 11 -> 12: remove intermediate build cache metadata.
module Corvus.Database.Migrations.V012 (migration) where

import Corvus.Database.Migration
import Database.Persist.Sql (rawExecute)

migration :: Migration
migration = Migration 12 "Remove intermediate build cache metadata" $ \_ ->
  rawExecute "DROP TABLE build_cache_entry" []
