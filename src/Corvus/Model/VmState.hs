{-# LANGUAGE OverloadedStrings #-}

-- | VM lifecycle state machine.
--
-- Defines the set of actions that can be applied to a VM
-- (both operator-triggered and daemon-internal) and the pure
-- rule that maps @(currentStatus, action)@ to either a new
-- 'VmStatus' or a human-readable rejection message.
--
-- Kept separate from "Corvus.Handlers.Vm" so the state machine is
-- directly unit-testable — no handler context, no database, no
-- server state.
module Corvus.Model.VmState
  ( VmAction (..)
  , validateTransition
  )
where

import Corvus.Model (VmStatus (..))
import Data.Text (Text)

-- | Actions on a VM. Split into two families:
--
--   * Operator-triggered actions are issued by @crv vm …@ commands
--     and arrive at the daemon over the Cap'n Proto RPC surface.
--     'ActionStart' is split into four variants because the
--     resulting intermediate status depends on what the daemon
--     knows about the VM (saved state? guest agent? paused?). The
--     caller in @handleVmStartValidate@ picks the right variant so
--     the validator is the sole source of truth for the
--     intermediate state.
--
--   * System-completion actions are issued by the daemon's own
--     background workers when an async operation (save / load /
--     migrate) finishes. Plumbing them through 'validateTransition'
--     keeps every status write in the codebase gated by the FSM —
--     no caller can rename a transition out from under the unit
--     tests.
data VmAction
  = -- Operator-triggered
    ActionStartCold
  | ActionStartColdWithGA
  | ActionStartResumeSaved
  | ActionStartResumePaused
  | ActionStop
  | ActionPause
  | ActionReset
  | ActionSave
  | -- Save worker completion
    ActionSaveDone
  | ActionSaveFail
  | -- Load worker completion
    ActionLoadDone
  | ActionLoadFail
  | -- Migration orchestrator completion
    ActionMigrateDone
  | ActionMigrateFail
  deriving (Eq, Show, Bounded, Enum)

-- | Check if a state transition is valid.
-- Returns @Right newStatus@ if valid, @Left errorMessage@ if invalid.
--
-- Transition rules (operator actions only):
--
--   * Reset: always allowed, sets to 'VmStopped'. The reset
--     handler dispatches the appropriate cancel side-effect based
--     on the from-state (kill QEMU, delete saved-state file,
--     QMP @migrate_cancel@, abort migration orchestrator).
--   * From 'VmStopped': 'ActionStartCold' → 'VmRunning';
--     'ActionStartColdWithGA' → 'VmStarting'.
--   * From 'VmStarting': 'ActionStop' → 'VmStopping'.
--   * From 'VmRunning': 'ActionStop' → 'VmStopping',
--     'ActionPause' → 'VmPaused', 'ActionSave' → 'VmSaving'.
--   * From 'VmStopping': only 'ActionReset'.
--   * From 'VmPaused': 'ActionStartResumePaused' → 'VmRunning'
--     (QMP @cont@ on the still-live QEMU), 'ActionSave' →
--     'VmSaving'.
--   * From 'VmSaved': 'ActionStartResumeSaved' → 'VmLoading'
--     (the agent will spawn QEMU with @-incoming@), 'ActionReset'
--     drops the state file and goes to 'VmStopped'.
--   * From 'VmError': only 'ActionReset'.
--   * From 'VmSaving' / 'VmLoading' / 'VmMigrating': only
--     'ActionReset' is allowed for operators (cancels the in-flight
--     op). All other operator actions return 'Left' with a message
--     naming the in-flight operation.
--
-- System-completion actions (only valid from the matching
-- transient state):
--
--   * 'VmSaving' → 'ActionSaveDone' → 'VmSaved',
--     'ActionSaveFail' → 'VmError'.
--   * 'VmLoading' → 'ActionLoadDone' → 'VmRunning',
--     'ActionLoadFail' → 'VmError'.
--   * 'VmMigrating' → 'ActionMigrateDone' → 'VmSaved',
--     'ActionMigrateFail' → 'VmSaved' (rollback; source-side saved
--     state is intact). Catastrophic migration failure is a
--     handler concern — the FSM only models "ended saved".
validateTransition :: VmStatus -> VmAction -> Either Text VmStatus
validateTransition currentStatus action = case (currentStatus, action) of
  -- Reset is always allowed; sets to Stopped. The reset handler
  -- dispatches cancel side-effects per from-state.
  (_, ActionReset) -> Right VmStopped
  --
  -- From Stopped
  --
  (VmStopped, ActionStartCold) -> Right VmRunning
  (VmStopped, ActionStartColdWithGA) -> Right VmStarting
  (VmStopped, ActionStartResumeSaved) ->
    Left "Cannot resume from saved: VM is stopped (no state file)"
  (VmStopped, ActionStartResumePaused) ->
    Left "Cannot resume from paused: VM is stopped"
  (VmStopped, ActionStop) -> Left "VM is already stopped"
  (VmStopped, ActionPause) -> Left "Cannot pause a stopped VM"
  (VmStopped, ActionSave) -> Left "Cannot save a stopped VM"
  --
  -- From Starting
  --
  (VmStarting, ActionStartCold) -> Left "VM is already starting"
  (VmStarting, ActionStartColdWithGA) -> Left "VM is already starting"
  (VmStarting, ActionStartResumeSaved) -> Left "VM is already starting"
  (VmStarting, ActionStartResumePaused) -> Left "VM is already starting"
  (VmStarting, ActionStop) -> Right VmStopping
  (VmStarting, ActionPause) -> Left "Cannot pause a VM that is still starting"
  (VmStarting, ActionSave) -> Left "Cannot save a VM that is still starting"
  --
  -- From Running
  --
  (VmRunning, ActionStartCold) -> Left "VM is already running"
  (VmRunning, ActionStartColdWithGA) -> Left "VM is already running"
  (VmRunning, ActionStartResumeSaved) -> Left "VM is already running"
  (VmRunning, ActionStartResumePaused) -> Left "VM is already running"
  (VmRunning, ActionStop) -> Right VmStopping
  (VmRunning, ActionPause) -> Right VmPaused
  (VmRunning, ActionSave) -> Right VmSaving
  --
  -- From Stopping
  --
  (VmStopping, ActionStartCold) -> Left "Cannot start VM while it is stopping"
  (VmStopping, ActionStartColdWithGA) -> Left "Cannot start VM while it is stopping"
  (VmStopping, ActionStartResumeSaved) -> Left "Cannot start VM while it is stopping"
  (VmStopping, ActionStartResumePaused) -> Left "Cannot start VM while it is stopping"
  (VmStopping, ActionStop) -> Left "VM is already stopping"
  (VmStopping, ActionPause) -> Left "Cannot pause VM while it is stopping"
  (VmStopping, ActionSave) -> Left "Cannot save VM while it is stopping"
  --
  -- From Paused
  --
  (VmPaused, ActionStartCold) -> Left "VM is paused; use resume"
  (VmPaused, ActionStartColdWithGA) -> Left "VM is paused; use resume"
  (VmPaused, ActionStartResumeSaved) -> Left "VM is paused, not saved"
  (VmPaused, ActionStartResumePaused) -> Right VmRunning
  (VmPaused, ActionStop) -> Left "Cannot stop a paused VM, reset instead"
  (VmPaused, ActionPause) -> Left "VM is already paused"
  (VmPaused, ActionSave) -> Right VmSaving
  --
  -- From Saved
  --
  (VmSaved, ActionStartCold) -> Left "VM has saved state; resume via start"
  (VmSaved, ActionStartColdWithGA) -> Left "VM has saved state; resume via start"
  (VmSaved, ActionStartResumeSaved) -> Right VmLoading
  (VmSaved, ActionStartResumePaused) -> Left "VM is saved, not paused"
  (VmSaved, ActionStop) ->
    Left "VM has saved state; use 'vm start' to resume or 'vm reset' to discard it"
  (VmSaved, ActionPause) -> Left "Cannot pause a saved VM; start it first"
  (VmSaved, ActionSave) -> Left "VM is already saved"
  --
  -- From Error
  --
  (VmError, ActionStartCold) -> Left "Cannot start VM in error state, reset first"
  (VmError, ActionStartColdWithGA) -> Left "Cannot start VM in error state, reset first"
  (VmError, ActionStartResumeSaved) -> Left "Cannot start VM in error state, reset first"
  (VmError, ActionStartResumePaused) -> Left "Cannot start VM in error state, reset first"
  (VmError, ActionStop) -> Left "Cannot stop VM in error state, reset first"
  (VmError, ActionPause) -> Left "Cannot pause VM in error state, reset first"
  (VmError, ActionSave) -> Left "Cannot save VM in error state, reset first"
  --
  -- From Saving (operator actions; only Reset is allowed, handled above)
  --
  (VmSaving, ActionStartCold) -> Left "VM is being saved; wait for completion or reset"
  (VmSaving, ActionStartColdWithGA) -> Left "VM is being saved; wait for completion or reset"
  (VmSaving, ActionStartResumeSaved) -> Left "VM is being saved; wait for completion or reset"
  (VmSaving, ActionStartResumePaused) -> Left "VM is being saved; wait for completion or reset"
  (VmSaving, ActionStop) -> Left "VM is being saved; wait or reset"
  (VmSaving, ActionPause) -> Left "VM is being saved; wait or reset"
  (VmSaving, ActionSave) -> Left "VM is already being saved"
  --
  -- From Loading
  --
  (VmLoading, ActionStartCold) -> Left "VM is being loaded; wait or reset"
  (VmLoading, ActionStartColdWithGA) -> Left "VM is being loaded; wait or reset"
  (VmLoading, ActionStartResumeSaved) -> Left "VM is being loaded; wait or reset"
  (VmLoading, ActionStartResumePaused) -> Left "VM is being loaded; wait or reset"
  (VmLoading, ActionStop) -> Left "VM is being loaded; wait or reset"
  (VmLoading, ActionPause) -> Left "VM is being loaded; wait or reset"
  (VmLoading, ActionSave) -> Left "VM is being loaded; wait or reset"
  --
  -- From Migrating
  --
  (VmMigrating, ActionStartCold) -> Left "VM is being migrated; wait or reset"
  (VmMigrating, ActionStartColdWithGA) -> Left "VM is being migrated; wait or reset"
  (VmMigrating, ActionStartResumeSaved) -> Left "VM is being migrated; wait or reset"
  (VmMigrating, ActionStartResumePaused) -> Left "VM is being migrated; wait or reset"
  (VmMigrating, ActionStop) -> Left "VM is being migrated; wait or reset"
  (VmMigrating, ActionPause) -> Left "VM is being migrated; wait or reset"
  (VmMigrating, ActionSave) -> Left "VM is being migrated; wait or reset"
  --
  -- System-completion actions (only valid from the matching transient state)
  --
  (VmSaving, ActionSaveDone) -> Right VmSaved
  (VmSaving, ActionSaveFail) -> Right VmError
  (_, ActionSaveDone) -> Left "ActionSaveDone is only valid from VmSaving"
  (_, ActionSaveFail) -> Left "ActionSaveFail is only valid from VmSaving"
  (VmLoading, ActionLoadDone) -> Right VmRunning
  (VmLoading, ActionLoadFail) -> Right VmError
  (_, ActionLoadDone) -> Left "ActionLoadDone is only valid from VmLoading"
  (_, ActionLoadFail) -> Left "ActionLoadFail is only valid from VmLoading"
  (VmMigrating, ActionMigrateDone) -> Right VmSaved
  (VmMigrating, ActionMigrateFail) -> Right VmSaved
  (_, ActionMigrateDone) -> Left "ActionMigrateDone is only valid from VmMigrating"
  (_, ActionMigrateFail) -> Left "ActionMigrateFail is only valid from VmMigrating"
