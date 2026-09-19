-- | Shared database helpers for the VM subsystem.
--
-- Kept separate from 'Corvus.Handlers.Vm' so that the per-concern
-- submodules (Console, CloudInit, Monitor) can all import these
-- helpers without creating cycles through the umbrella module.
module Corvus.Handlers.Vm.Db
  ( -- * Queries
    getVmWithStatus
  , getVmStatusOnly
  , hasNetdMediatedNetIf

    -- * Status updates
  , setVmStatus
  , setVmStarted
  , setVmStopped
  , setVmError
  , claimVmStart
  , setVmStartedIfCurrent
  , setVmErrorIfCurrent
  , setVmSpicePortIfCurrent
  , setVmVsockCidIfCurrent
  , claimVmReset
  , completeVmReset
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Corvus.Model (Vm (vmLifecycleRevision, vmRuntimeGeneration, vmStatus), VmId, VmStatus (..))
import qualified Corvus.Model as M
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist (entityVal, get, selectList, update, updateWhere, (=.), (==.))
import Database.Persist.Sql (SqlPersistT, toSqlKey, updateWhereCount)

-- | Get VM with its current status
getVmWithStatus :: Int64 -> SqlPersistT IO (Maybe (Vm, VmStatus))
getVmWithStatus vmId = do
  let key = toSqlKey vmId :: VmId
  mVm <- get key
  pure $ case mVm of
    Nothing -> Nothing
    Just vm -> Just (vm, vmStatus vm)

-- | Just the current status, or 'Nothing' if the row is gone.
-- Used by 'attachVmMonitor' to skip reconciliation when a
-- competing handler ('handleVmReset') has already committed a
-- terminal state.
getVmStatusOnly :: Int64 -> SqlPersistT IO (Maybe VmStatus)
getVmStatusOnly vmId = fmap vmStatus <$> get (toSqlKey vmId :: VmId)

-- | Set VM status (used during start: VmStarting or VmRunning).
-- Clears any prior error reason so a recovered VM doesn't keep
-- showing a stale "Last error" in @crv vm show@.
-- The @_pid@ parameter is kept for caller-side symmetry but is no
-- longer persisted — the agent owns every PID. Drop the parameter
-- on the next breaking change.
setVmStarted :: Int64 -> VmStatus -> Int -> SqlPersistT IO ()
setVmStarted vmId status _pid = do
  let key = toSqlKey vmId :: VmId
  update
    key
    [ M.VmStatus =. status
    , M.VmErrorMessage =. Nothing
    , M.VmLastErrorAt =. Nothing
    ]

-- | Set VM status to stopped and clear healthcheck, SPICE port,
-- prior error reason, and guest network data.
setVmStopped :: Int64 -> SqlPersistT IO ()
setVmStopped vmId = do
  let key = toSqlKey vmId :: VmId
  update
    key
    [ M.VmStatus =. VmStopped
    , M.VmHealthcheck =. Nothing
    , M.VmSpicePort =. Nothing
    , M.VmErrorMessage =. Nothing
    , M.VmLastErrorAt =. Nothing
    ]
  updateWhere
    [M.NetworkInterfaceVmId ==. key]
    [M.NetworkInterfaceGuestIpAddresses =. Nothing]

-- | Set VM status to error, record the reason + timestamp on the
-- VM row, and clear runtime state (healthcheck, SPICE port, guest
-- IPs). The @reason@ surfaces verbatim in @crv vm show@ so the
-- operator sees the actual cause (e.g. "QEMU exited with code 137
-- before first guest-agent ping") instead of having to chase task
-- history.
setVmError :: Int64 -> Text -> SqlPersistT IO ()
setVmError vmId reason = do
  now <- liftIO getCurrentTime
  let key = toSqlKey vmId :: VmId
  update
    key
    [ M.VmStatus =. VmError
    , M.VmHealthcheck =. Nothing
    , M.VmSpicePort =. Nothing
    , M.VmErrorMessage =. Just reason
    , M.VmLastErrorAt =. Just now
    ]
  updateWhere
    [M.NetworkInterfaceVmId ==. key]
    [M.NetworkInterfaceGuestIpAddresses =. Nothing]

-- | Set VM status (without changing PID). Clears any prior error
-- reason when transitioning out of 'VmError'; leaves it alone for
-- 'VmError' itself so explicit error setters keep the message
-- they wrote (see 'setVmError').
setVmStatus :: Int64 -> VmStatus -> SqlPersistT IO ()
setVmStatus vmId status = do
  let key = toSqlKey vmId :: VmId
  if status == VmError
    then update key [M.VmStatus =. status]
    else
      update
        key
        [ M.VmStatus =. status
        , M.VmErrorMessage =. Nothing
        , M.VmLastErrorAt =. Nothing
        ]

-- | Admit reset by advancing the daemon-owned lifecycle revision and leaving
-- the runtime generation in place until the agent confirms termination.
-- The conditional revision predicate prevents a stale reset completion from
-- overwriting a later lifecycle operation.
claimVmReset :: Int64 -> SqlPersistT IO (Maybe (Int64, Maybe Int64))
claimVmReset vmId = do
  let key = toSqlKey vmId :: VmId
  mVm <- get key
  case mVm of
    Nothing -> pure Nothing
    Just vm -> do
      let oldRevision = vmLifecycleRevision vm
          revision = oldRevision + 1
      updateWhere
        [ M.VmId ==. key
        , M.VmLifecycleRevision ==. oldRevision
        ]
        [ M.VmLifecycleRevision =. revision
        , M.VmStatus =. VmStopping
        ]
      mClaimed <- get key
      pure $ do
        claimed <- mClaimed
        if vmLifecycleRevision claimed == revision && vmStatus claimed == VmStopping
          then Just (revision, vmRuntimeGeneration vm)
          else Nothing

-- | Claim a fresh runtime from an expected inactive state before any slow
-- start work begins. Both tokens are
-- daemon-owned: a new lifecycle revision makes an older reset or start stale,
-- and its paired runtime generation identifies this QEMU incarnation.
--
-- This is deliberately a conditional update rather than a read followed by an
-- unconditional status write. A reset which wins the race leaves no row that
-- this start is allowed to update.
claimVmStart :: Int64 -> VmStatus -> VmStatus -> SqlPersistT IO (Maybe Vm)
claimVmStart vmId expectedStatus startStatus = do
  let key = toSqlKey vmId :: VmId
  mVm <- get key
  case mVm of
    Just vm | vmStatus vm == expectedStatus -> do
      let revision = vmLifecycleRevision vm + 1
      updateWhere
        [ M.VmId ==. key
        , M.VmStatus ==. expectedStatus
        , M.VmLifecycleRevision ==. vmLifecycleRevision vm
        ]
        [ M.VmStatus =. startStatus
        , M.VmLifecycleRevision =. revision
        , M.VmRuntimeGeneration =. Just revision
        , M.VmErrorMessage =. Nothing
        , M.VmLastErrorAt =. Nothing
        ]
      mClaimed <- get key
      pure $ do
        claimed <- mClaimed
        if vmStatus claimed == startStatus
          && vmLifecycleRevision claimed == revision
          && vmRuntimeGeneration claimed == Just revision
          then Just claimed
          else Nothing
    _ -> pure Nothing

-- | Complete a fresh start only if its daemon-owned runtime fence still owns
-- the VM row.  A reset advances the revision before stopping QEMU, so a late
-- start response must never be allowed to revive that reset.
setVmStartedIfCurrent :: Int64 -> Int64 -> Int64 -> VmStatus -> SqlPersistT IO Bool
setVmStartedIfCurrent vmId revision generation status = do
  let key = toSqlKey vmId :: VmId
  count <-
    updateWhereCount
      [ M.VmId ==. key
      , M.VmLifecycleRevision ==. revision
      , M.VmRuntimeGeneration ==. Just generation
      ]
      [ M.VmStatus =. status
      , M.VmErrorMessage =. Nothing
      , M.VmLastErrorAt =. Nothing
      ]
  pure (count == 1)

-- | Record a fresh-start failure only for its owning runtime.  Guest runtime
-- data is cleared only after the guarded VM update succeeded.
setVmErrorIfCurrent :: Int64 -> Int64 -> Int64 -> Text -> SqlPersistT IO Bool
setVmErrorIfCurrent vmId revision generation reason = do
  now <- liftIO getCurrentTime
  let key = toSqlKey vmId :: VmId
  count <-
    updateWhereCount
      [ M.VmId ==. key
      , M.VmLifecycleRevision ==. revision
      , M.VmRuntimeGeneration ==. Just generation
      ]
      [ M.VmStatus =. VmError
      , M.VmHealthcheck =. Nothing
      , M.VmSpicePort =. Nothing
      , M.VmErrorMessage =. Just reason
      , M.VmLastErrorAt =. Just now
      ]
  when (count == 1) $
    updateWhere
      [M.NetworkInterfaceVmId ==. key]
      [M.NetworkInterfaceGuestIpAddresses =. Nothing]
  pure (count == 1)

setVmSpicePortIfCurrent :: Int64 -> Int64 -> Int64 -> Int -> SqlPersistT IO Bool
setVmSpicePortIfCurrent vmId revision generation port = do
  let key = toSqlKey vmId :: VmId
  count <-
    updateWhereCount
      [ M.VmId ==. key
      , M.VmLifecycleRevision ==. revision
      , M.VmRuntimeGeneration ==. Just generation
      ]
      [M.VmSpicePort =. Just port]
  pure (count == 1)

setVmVsockCidIfCurrent :: Int64 -> Int64 -> Int64 -> Int -> SqlPersistT IO Bool
setVmVsockCidIfCurrent vmId revision generation cid = do
  let key = toSqlKey vmId :: VmId
  count <-
    updateWhereCount
      [ M.VmId ==. key
      , M.VmLifecycleRevision ==. revision
      , M.VmRuntimeGeneration ==. Just generation
      ]
      [M.VmVsockCid =. Just cid]
  pure (count == 1)

-- | Commit the terminal reset state only for its owning fence.
completeVmReset :: Int64 -> Int64 -> SqlPersistT IO Bool
completeVmReset vmId revision = do
  let key = toSqlKey vmId :: VmId
  count <-
    updateWhereCount
      [ M.VmId ==. key
      , M.VmLifecycleRevision ==. revision
      , M.VmStatus ==. VmStopping
      ]
      [ M.VmStatus =. VmStopped
      , M.VmRuntimeGeneration =. Nothing
      , M.VmHealthcheck =. Nothing
      , M.VmSpicePort =. Nothing
      , M.VmErrorMessage =. Nothing
      , M.VmLastErrorAt =. Nothing
      ]
  when (count == 1) $
    updateWhere
      [M.NetworkInterfaceVmId ==. key]
      [M.NetworkInterfaceGuestIpAddresses =. Nothing]
  pure (count == 1)

-- | Check if a VM has any netd-mediated network interface: a
-- managed NIC (attached to a Corvus virtual network) or a bridge
-- NIC (attached to a user-managed host bridge). Both go through
-- the same netd applyTap path during 'assembleVmSpec'.
hasNetdMediatedNetIf :: Int64 -> SqlPersistT IO Bool
hasNetdMediatedNetIf vmId = do
  let vmKey = toSqlKey vmId :: VmId
  nics <- selectList [M.NetworkInterfaceVmId ==. vmKey] []
  pure $ any (isNetdMediated . entityVal) nics
  where
    isNetdMediated ni =
      M.networkInterfaceInterfaceType ni == M.NetBridge
        || isJust (M.networkInterfaceNetworkId ni)
