-- | Shared result types for QMP commands.
module Corvus.Node.Qmp.Types
  ( QmpResult (..)
  , QmpMigrationStatus (..)
  )
where

import Data.Text (Text)

-- | Result of a QMP command.
data QmpResult
  = QmpSuccess
  | QmpError !Text
  | QmpConnectionFailed !Text
  deriving (Eq, Show)

-- | Status returned by QMP @query-migrate@.
--
-- The full QEMU vocabulary is collapsed into the states the save/load
-- coordinator needs. Terminal failures retain the QMP response for callers to
-- surface.
data QmpMigrationStatus
  = MigInactive
  | MigActive
  | MigCompleted
  | MigFailed !Text
  deriving (Eq, Show)
