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
  )
where

import Control.Monad.IO.Class (liftIO)
import Corvus.Model (Vm (vmStatus), VmId, VmStatus (..))
import qualified Corvus.Model as M
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist (entityVal, get, selectList, update, updateWhere, (=.), (==.))
import Database.Persist.Sql (SqlPersistT, toSqlKey)

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
