{-# LANGUAGE OverloadedStrings #-}

module Corvus.Database.Migrations.V016 (migration) where

import Corvus.Database.Migration
import Database.Persist.Sql (rawExecute)

migration :: Migration
migration = Migration 16 "Replace draining node state with maintenance" $ \_ ->
  rawExecute "UPDATE node SET admin_state = 'maintenance' WHERE admin_state = 'draining'" []
