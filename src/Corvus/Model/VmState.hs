{-# LANGUAGE OverloadedStrings #-}

-- | VM lifecycle state machine.
--
-- Defines the set of client-triggerable actions ('VmAction') and the pure
-- rule that maps @(currentStatus, action)@ to either a new 'VmStatus' or
-- a human-readable rejection message.
--
-- Kept separate from "Corvus.Handlers.Vm" so the state machine is
-- directly unit-testable — no handler context, no database, no server
-- state.
module Corvus.Model.VmState
  ( VmAction (..)
  , validateTransition
  )
where

import Corvus.Model (VmStatus (..))
import Data.Text (Text)

-- | VM action triggered by client
data VmAction = ActionStart | ActionStop | ActionPause | ActionReset
  deriving (Eq, Show)

-- | Check if a state transition is valid for client commands.
-- Returns @Right newStatus@ if valid, @Left errorMessage@ if invalid.
--
-- Transition rules:
--
--   * Reset: always allowed, sets to 'VmStopped'
--   * From 'VmStopped': can Start (→ 'VmStarting' if GA enabled, → 'VmRunning' otherwise)
--   * From 'VmStarting': can Stop (→ 'VmStopping') or Reset (→ 'VmStopped')
--   * From 'VmRunning': can Stop (→ 'VmStopping') or Pause (→ 'VmPaused')
--   * From 'VmStopping': only Reset is allowed
--   * From 'VmPaused': can Start (resume → 'VmRunning')
--   * From 'VmError': only Reset is allowed
--
-- Note: 'validateTransition' returns 'VmRunning' for Start from 'VmStopped'.
-- The caller (@handleVmStart@) overrides to 'VmStarting' when guest agent
-- is enabled.
validateTransition :: VmStatus -> VmAction -> Either Text VmStatus
validateTransition currentStatus action = case (currentStatus, action) of
  -- Reset is always allowed, sets to Stopped
  (_, ActionReset) -> Right VmStopped
  -- From Stopped: can only Start
  (VmStopped, ActionStart) -> Right VmRunning
  (VmStopped, ActionStop) -> Left "VM is already stopped"
  (VmStopped, ActionPause) -> Left "Cannot pause a stopped VM"
  -- From Starting: can Stop or Reset (handled above)
  (VmStarting, ActionStart) -> Left "VM is already starting"
  (VmStarting, ActionStop) -> Right VmStopping
  (VmStarting, ActionPause) -> Left "Cannot pause a VM that is still starting"
  -- From Running: can Stop or Pause
  (VmRunning, ActionStart) -> Left "VM is already running"
  (VmRunning, ActionStop) -> Right VmStopping
  (VmRunning, ActionPause) -> Right VmPaused
  -- From Stopping: only Reset (handled above)
  (VmStopping, ActionStart) -> Left "Cannot start VM while it is stopping"
  (VmStopping, ActionStop) -> Left "VM is already stopping"
  (VmStopping, ActionPause) -> Left "Cannot pause VM while it is stopping"
  -- From Paused: can only Start (resume)
  (VmPaused, ActionStart) -> Right VmRunning
  (VmPaused, ActionStop) -> Left "Cannot stop a paused VM, reset instead"
  (VmPaused, ActionPause) -> Left "VM is already paused"
  -- From Error: only Reset is allowed (handled above)
  (VmError, ActionStart) -> Left "Cannot start VM in error state, reset first"
  (VmError, ActionStop) -> Left "Cannot stop VM in error state, reset first"
  (VmError, ActionPause) -> Left "Cannot pause VM in error state, reset first"
