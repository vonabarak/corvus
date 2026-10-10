-- | Shared result types for QMP commands.
module Corvus.Node.Qmp.Types
  ( QmpResult (..)
  , QmpMigrationStatus (..)
  , QmpBalloonFailure (..)
  )
where

import Data.Text (Text)

-- | Result of a QMP command.
data QmpResult
  = QmpSuccess
  | QmpError !Text
  | QmpConnectionFailed !Text
  deriving stock (Eq, Show)

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
  deriving stock (Eq, Show)

-- | Balloon failures distinguish driver readiness from QMP communication errors.
data QmpBalloonFailure = QmpBalloonDriverNotReady | QmpBalloonError !Text
  deriving stock (Eq, Show)
