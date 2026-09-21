{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | VM management handlers.
-- This module contains handlers for VM lifecycle operations:
-- list, show, start, stop, pause, reset.
module Corvus.Handlers.Vm
  ( -- * Action types
    VmCreate (..)
  , VmDelete (..)
  , VmStart (..)
  , VmStop (..)
  , VmEdit (..)
  , VmPause (..)
  , VmReset (..)
  , VmSave (..)

    -- * Handlers
  , handleVmList
  , handleVmShow
  , handleVmCreate
  , handleVmDelete
  , attachVmMonitor
  , reattachVmMonitors
  , autostartVmsOnNode
  , handleVmPause
  , handleVmSaveExecute
  , handleVmSaveValidate
  , handleVmEdit
  , handleVmCloudInit
  , handleSerialConsole
  , handleSerialConsoleFlush
  , handleHmpMonitor
  , handleHmpMonitorFlush
  , handleVmSendCtrlAltDel
  , handleVmViewGrant

    -- * Helpers (exposed for tests)
  , generateSpicePassword

    -- * In-daemon helpers used by other handlers
  , getVmDetails
  , getVmWithStatus
  , getVmStatusOnly
  , setVmStatus
  , setVmError
  , setVmStarted
  , setVmStopped
  , claimVmReset
  , claimVmStart
  , completeVmReset
  , setVmErrorIfCurrent
  , setVmSpicePortIfCurrent
  , setVmStartedIfCurrent
  , setVmVsockCidIfCurrent
  , hasNetdMediatedNetIf
  )
where

import Corvus.Action

import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.Handlers.Disk (DiskDelete (..))
import Corvus.Handlers.Disk.Db (diskImageNodeFilePathFor)
import Corvus.Handlers.Resolve (ResolveError (..), resolveErrorMessage, resolveNode, validateName)
import Corvus.Handlers.Scheduler (pickNodeForVm)
import Corvus.Handlers.Vm.CloudInit (ensureCloudInitIso, handleVmCloudInit)
import Corvus.Handlers.Vm.Console
  ( generateSpicePassword
  , handleHmpMonitor
  , handleHmpMonitorFlush
  , handleSerialConsole
  , handleSerialConsoleFlush
  , handleVmSendCtrlAltDel
  , handleVmViewGrant
  )
import Corvus.Handlers.Vm.Db
  ( claimVmReset
  , claimVmStart
  , completeVmReset
  , getVmStatusOnly
  , getVmWithStatus
  , hasNetdMediatedNetIf
  , setVmError
  , setVmErrorIfCurrent
  , setVmSpicePortIfCurrent
  , setVmStarted
  , setVmStartedIfCurrent
  , setVmStatus
  , setVmStopped
  , setVmVsockCidIfCurrent
  )
import Corvus.Handlers.Vm.Monitor
  ( attachVmMonitor
  , reattachVmMonitors
  , releaseManagedTaps
  )
import Corvus.Model (DriveFormat (..), VmStatus (..))
import Corvus.Model hiding (DriveFormat, VmStatus)
import qualified Corvus.Model as M
import Corvus.Model.VmState (VmAction (..), validateTransition)
import Corvus.Node.SpicePort (withAllocatedSpicePort)
import Corvus.Node.VsockCid (withAllocatedVsockCid)
import qualified Corvus.NodeAgentClient as NOA
import qualified Corvus.NodeAgentClient.Spec as NSpec
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol
import Corvus.Qemu (QemuConfig, getGuestAgentSocket, getMonitorSocket, getSerialSocket)
import Corvus.Types
import Data.Int (Int64)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Word (Word32)
import Database.Persist
import Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlPool)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- VM Handlers
--------------------------------------------------------------------------------

-- | Handle VM list command
handleVmList :: ServerState -> IO Response
handleVmList state = do
  vms <- runSqlPool listVms (ssDbPool state)
  pure $ RespVmList vms

-- | Handle VM show command
handleVmShow :: ServerState -> Int64 -> IO Response
handleVmShow state vmId = do
  result <- runSqlPool (getVmDetails (ssQemuConfig state) vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just details -> pure $ RespVmDetails details

-- | Handle VM create command. An empty 'nodeRefText' means
-- "no explicit placement" — defer to 'pickNodeForVm'.
handleVmCreate
  :: ServerState
  -> Text
  -- ^ name
  -> Text
  -- ^ node ref (name or id); empty = defer to scheduler
  -> Int
  -> Int
  -> Maybe Text
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -- ^ rebootQuirk
  -> Text
  -- ^ cpuModel (empty == "host")
  -> IO Response
handleVmCreate state name nodeRefText cpuCount ramMb description headless guestAgent tpm cloudInit autostart rebootQuirk cpuModel0 =
  case validateName "VM" name of
    Left err -> pure $ RespError err
    Right () -> do
      let pool = ssDbPool state
          -- Empty wire string is the operator's "use the default"
          -- signal — fall back to "host" so existing callers and
          -- older wire clients keep working unchanged.
          cpuModel = if T.null cpuModel0 then "host" else cpuModel0
          placeOn nodeKey = do
            -- Try to allocate a CID via the target node's agent.
            -- A 'Left' here typically means the agent's host has no
            -- vhost-vsock support (or the agent is unreachable);
            -- fall back to creating the VM with vsockCid = Nothing
            -- — QEMU will start without a vhost-vsock-pci device
            -- and operators just lose the @ssh user\@vsock/CID@
            -- shortcut for that VM.
            eVmId <- do
              r <-
                withAllocatedVsockCid state nodeKey $ \cid ->
                  runSqlPool
                    (createVm name nodeKey cpuCount ramMb description headless guestAgent tpm cloudInit autostart rebootQuirk cpuModel (Just cid))
                    pool
              case r of
                Right vmId -> pure (Right vmId)
                Left _ -> do
                  vmId <-
                    runSqlPool
                      (createVm name nodeKey cpuCount ramMb description headless guestAgent tpm cloudInit autostart rebootQuirk cpuModel Nothing)
                      pool
                  pure (Right vmId)
            case eVmId of
              Left err -> pure $ RespError err
              Right vmId -> do
                -- Bump the scheduler's in-memory reservation so the
                -- next 'pickNodeForVm' call (within the same daemon,
                -- before the agent's next stats push) doesn't
                -- double-spend this VM's RAM share. The reservation
                -- clears when fresh 'NodeStats' arrive (Phase 5).
                reserveRam state nodeKey ramMb
                pure $ RespVmCreated vmId
      -- Empty text == operator did not pass @--node@; capnp's
      -- unset-EntityRef default ('byId 0') also lands here.
      -- Either way, defer to the scheduler.
      if T.null nodeRefText || nodeRefText == "0"
        then do
          eNid <- pickNodeForVm state ramMb
          case eNid of
            Left err -> pure $ RespError err
            Right nodeKey -> placeOn nodeKey
        else do
          r <- resolveNode (Ref nodeRefText) pool
          case r of
            Left (RefNotFound _ _) -> pure RespNodeNotFound
            Left re -> pure $ RespAmbiguousRef (resolveErrorMessage re)
            Right nidRaw -> placeOn (M.toSqlKey nidRaw)

-- | Handle VM delete command. Reaps ephemeral disks attached to the
-- VM (cloud-init ISOs, template-instantiated disks) unless 'keepDisks'
-- is set. Non-ephemeral disks are never auto-deleted — the operator
-- removes those manually with @crv disk delete@.
handleVmDelete :: ActionContext -> Int64 -> Bool -> IO Response
handleVmDelete ctx vmId keepDisks = do
  let state = acState ctx
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (vm, status) ->
      if status
        `elem` [ VmRunning
               , VmStarting
               , VmStopping
               , VmPaused
               , VmSaving
               , VmLoading
               , VmMigrating
               ]
        then pure RespVmRunning
        else do
          -- TPM state belongs to an enabled TPM VM. Its deletion is
          -- strict: if the nodeagent cannot remove it, leave the VM
          -- and all of its associations intact so the operator can
          -- retry without orphaning persistent security state.
          tpmDeleteResult <-
            if vmTpm vm
              then deleteTpmStateForVm state vmId (vmName vm)
              else pure (Right ())
          case tpmDeleteResult of
            Left err -> pure (RespError err)
            Right () -> do
              -- For a saved VM, ask the agent to drop the per-VM
              -- state file before we tear the row down. Best-effort
              -- (the call is idempotent on the agent) so an
              -- unreachable agent doesn't block the delete.
              when (status == VmSaved) $
                runServerLogging state $ do
                  outerDel <-
                    liftIO $
                      withVmNodeAgent state vmId $ \nac ->
                        NOA.deleteSavedState nac (vmName vm)
                  case outerDel of
                    Left err ->
                      logWarnN $
                        "nodeagent unavailable; saved-state file may persist for deleted VM "
                          <> T.pack (show vmId)
                          <> ": "
                          <> err
                    Right (Left e) ->
                      logWarnN $
                        "deleteSavedState during delete failed for VM "
                          <> T.pack (show vmId)
                          <> ": "
                          <> T.pack (show e)
                    Right (Right ()) -> pure ()
              disksToDelete <-
                if keepDisks
                  then pure []
                  else runSqlPool (getEphemeralAttachedDisks vmId) (ssDbPool state)
              -- Delete VM and its associations (drives, netifs, etc.)
              runSqlPool (deleteVm vmId) (ssDbPool state)
              -- Reap each ephemeral disk as a subtask.
              mapM_ (runActionAsSubtask ctx . DiskDelete) disksToDelete
              pure RespVmDeleted

-- | Strictly remove persistent TPM state through the VM's nodeagent.
-- A missing state directory is success on the agent. Routing or RPC
-- failures are returned to the caller so it can leave the database
-- flag/row unchanged.
deleteTpmStateForVm :: ServerState -> Int64 -> Text -> IO (Either Text ())
deleteTpmStateForVm state vmId name = do
  outer <- withVmNodeAgent state vmId $ \nac -> NOA.deleteTpmState nac name
  pure $ case outer of
    Left err -> Left ("nodeagent unavailable; TPM state was not deleted: " <> err)
    Right (Left err) -> Left ("failed to delete TPM state: " <> T.pack (show err))
    Right (Right ()) -> Right ()

-- | Pick the right 'ActionStart' variant for a VM in its current
-- state. The validator returns the correct intermediate status
-- (VmRunning / VmStarting / VmLoading) per the chosen variant, so
-- this is the only place in the codebase that has to know "what
-- does start mean for this VM right now."
startVariantFor :: Vm -> VmStatus -> VmAction
startVariantFor vm = \case
  VmPaused -> ActionStartResumePaused
  VmSaved -> ActionStartResumeSaved
  _
    | vmGuestAgent vm -> ActionStartColdWithGA
    | otherwise -> ActionStartCold

-- | Validate that a VM can be started. Returns the VM, current
-- status, and the intermediate status the validator picked (one of
-- 'VmStarting', 'VmRunning', 'VmLoading') for the executor to commit.
-- Checks: VM exists, state transition valid, all referenced networks
-- are running.
handleVmStartValidate
  :: ServerState
  -> Int64
  -> IO (Either Response (Vm, VmStatus, VmStatus))
handleVmStartValidate state vmId = do
  mVm <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure $ Left RespVmNotFound
    Just (vm, currentStatus) ->
      case validateTransition currentStatus (startVariantFor vm currentStatus) of
        Left errMsg -> pure $ Left $ RespInvalidTransition currentStatus errMsg
        Right nextStatus -> do
          -- Check that all referenced networks are running. Applies
          -- equally to cold start (VmStopped → fresh QEMU) and
          -- resume-from-saved (VmSaved → fresh QEMU with `-incoming`).
          -- The paused-resume branch (VmPaused) is a QMP @cont@ on
          -- the still-live QEMU and doesn't touch networking at the
          -- agent level — skip the check there.
          if currentStatus `elem` [VmStopped, VmSaved]
            then do
              networkCheck <- runSqlPool (checkNetworksRunning vmId) (ssDbPool state)
              case networkCheck of
                Just networkName ->
                  pure $ Left $ RespInvalidTransition currentStatus $ "Network '" <> networkName <> "' is not running"
                Nothing -> pure $ Right (vm, currentStatus, nextStatus)
            else pure $ Right (vm, currentStatus, nextStatus)

-- | Execute VM start to completion (blocks until VmRunning).
-- Used with --wait flag or in withTaskAsync.
--
-- The agent's @vmStart@ blocks internally for the first QGA
-- ping when @waitForGuestAgentMs > 0@ — by the time the RPC
-- returns successfully, the VM is fully booted and the guest
-- agent is reachable. So there's no longer a separate
-- @waitForFirstPing@ step in the daemon.
handleVmStartExecute :: ActionContext -> Int64 -> IO Response
handleVmStartExecute ctx vmId = do
  let state = acState ctx
  validated <- handleVmStartValidate state vmId
  case validated of
    Left errResp -> pure errResp
    Right (vm, currentStatus, nextStatus) ->
      case currentStatus of
        VmPaused -> runServerLogging state $ resumeFromPaused state vmId vm
        VmStopped -> startFreshRuntime state nextStatus VmStopped
        VmSaved -> startFreshRuntime state nextStatus VmSaved
        _ -> runServerLogging state $ do
          -- Commit the intermediate status NOW, before the slow
          -- pre-launch dance (vsock probe, spec assembly, agent
          -- dial). Without this, a UI client that re-fetches right
          -- after the (async) RPC returns sees the old status
          -- ('VmStopped' / 'VmSaved') for ~100-500ms while
          -- 'launchVmViaAgent' is still preparing. Stop and save
          -- don't have this gap because their handlers flip the
          -- DB before any agent work; mirror that here. The same
          -- @setVmStatus@ inside 'launchVmViaAgent' is now a
          -- no-op when the row is already in @nextStatus@.
          liftIO $ runSqlPool (setVmStatus vmId nextStatus) (ssDbPool state)
          resp <- startQemuAndMonitor ctx vmId vm nextStatus
          case resp of
            RespVmStateChanged s
              | s `elem` [VmStarting, VmLoading] ->
                  -- The agent's vmStart returns after QEMU spawn,
                  -- before the first QGA ping (cold start) or before
                  -- QMP @cont@ on the incoming migration stream (load
                  -- from saved). Block here until the push channel
                  -- finishes the transition — either to 'VmRunning'
                  -- (ping landed / load complete) or 'VmError' (first
                  -- ping never came / QEMU crashed early). Without
                  -- this poll, callers passing @wait=true@ would see
                  -- the RPC return early and immediately fail their
                  -- first @vm.exec@.
                  liftIO $ waitForStartCompletion state vmId
              | otherwise -> pure resp
            _ -> pure resp
  where
    -- A stopped cold boot and a saved-state load each create a new QEMU
    -- incarnation. Claim them before slow work so the nodeagent can reject a
    -- delayed command from either prior incarnation.
    startFreshRuntime state nextStatus expectedStatus = do
      mClaimed <- runSqlPool (claimVmStart vmId expectedStatus nextStatus) (ssDbPool state)
      case mClaimed of
        Nothing -> do
          mCurrent <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
          pure $ case mCurrent of
            Nothing -> RespVmNotFound
            Just (_, status) -> RespInvalidTransition status "VM lifecycle operation was superseded"
        Just claimedVm -> runServerLogging state $ do
          resp <- startQemuAndMonitor ctx vmId claimedVm nextStatus
          case resp of
            RespVmStateChanged VmStarting ->
              liftIO $ waitForStartCompletion state vmId
            RespVmStateChanged VmLoading ->
              liftIO $ waitForStartCompletion state vmId
            _ -> pure resp

-- | Block until the DB row for @vmId@ leaves 'VmStarting'.
-- Returns the final response (always one of 'VmRunning',
-- 'VmError', or 'RespError' if the row vanishes / wait
-- timeout). Used by the @wait=true@ start path to preserve the
-- old "block until fully booted" semantics now that the agent
-- returns from 'vmStart' before QGA is ready.
waitForStartCompletion :: ServerState -> Int64 -> IO Response
waitForStartCompletion state vmId = go (10 * 60 * 10)
  where
    pool = ssDbPool state
    -- 10-minute budget at 100 ms ticks; matches the agent's
    -- worst-case cloud-init bootstrap wait (5 min) with margin.
    go remaining
      | remaining <= 0 =
          pure $
            RespError $
              "VM "
                <> T.pack (show vmId)
                <> " did not finish starting within 10 minutes; "
                <> "the agent's first-QGA-ping watcher may be stuck"
      | otherwise = do
          mStatus <- runSqlPool (getVmStatusOnly vmId) pool
          case mStatus of
            Nothing -> pure RespVmNotFound
            Just s | s `elem` [VmStarting, VmLoading] -> do
              threadDelay 100000
              go (remaining - 1)
            Just VmRunning -> pure $ RespVmStateChanged VmRunning
            Just VmError -> do
              -- Read the persisted error message so the RPC
              -- caller sees what the agent reported.
              mVm <- runSqlPool (get (toSqlKey vmId :: VmId)) pool
              let msg = case mVm of
                    Just v -> fromMaybe "VM start failed" (vmErrorMessage v)
                    Nothing -> "VM start failed"
              pure $ RespError msg
            -- Anything else: the start was overtaken by another
            -- handler (reset / delete). Report whatever the
            -- terminal state is so the caller doesn't loop.
            Just s -> pure $ RespVmStateChanged s

-- | Resume a paused VM via @vmResume@ (agent issues QMP @cont@).
resumeFromPaused :: ServerState -> Int64 -> Vm -> LoggingT IO Response
resumeFromPaused state vmId vm = do
  outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmResume nac vmId
  case outer of
    Left err -> do
      logWarnN $ "nodeagent unavailable; cannot resume VM " <> T.pack (show vmId) <> ": " <> err
      pure $ RespInvalidTransition VmPaused err
    Right r -> case r of
      Left e -> do
        logWarnN $ "vmResume failed for VM " <> T.pack (show vmId) <> ": " <> T.pack (show e)
        pure $ RespInvalidTransition VmPaused ("vmResume: " <> T.pack (show e))
      Right () -> do
        logInfoN $ "VM " <> T.pack (show vmId) <> " resumed"
        updated <- liftIO $ runSqlPool (setVmStartedIfCurrent vmId (vmLifecycleRevision vm) (fromMaybe (vmLifecycleRevision vm) (vmRuntimeGeneration vm)) VmRunning) (ssDbPool state)
        if updated
          then pure $ RespVmStateChanged VmRunning
          else pure $ RespInvalidTransition VmPaused "VM lifecycle operation was superseded"

-- | Start QEMU + virtiofsd via the agent, set status, fork the
-- monitor thread, hook up the chardev ring buffers.
--
-- After the refactor, the agent handles the QGA first-ping wait
-- itself (driven by @VmSpec.waitForGuestAgentMs@), spawns
-- virtiofsd internally if the spec carries shared dirs, and
-- spawns QEMU. So this function shrinks to:
--
--   1. Cloud-init ISO regen (still daemon-side, calls into the
--      agent's @cloudInitGenerateIso@).
--   2. SPICE port allocation (daemon owns @ssSpicePortLock@ and
--      persists 'Vm.spicePort').
--   3. Vsock CID re-validation (daemon owns @ssVsockCidLock@).
--   4. 'Spec.assembleVmSpec' — walk the DB, resolve managed
--      NICs through netd, pack into 'VmSpec'.
--   5. 'NOA.vmStart' — one RPC, the agent does everything else
--      (including blocking for first QGA ping).
--   6. 'attachVmMonitor' to watch for QEMU exit.
--   7. Wire up the chardev ring buffers.
startQemuAndMonitor :: ActionContext -> Int64 -> Vm -> VmStatus -> LoggingT IO Response
startQemuAndMonitor ctx vmId vm nextStatus = do
  let state = acState ctx
      pool = ssDbPool state
  -- Surface the node we're waiting on (vsock probe + agent vmStart
  -- are the long blockers here) for @crv task list/show/wait@.
  liftIO $
    pushTaskProgress
      state
      (fromSqlKey (acTaskId ctx))
      ("waiting for node " <> T.pack (show (fromSqlKey (M.vmNodeId vm))) <> " RPC (start)")
      (0, 0)
  ensureCloudInitIso ctx vmId vm
  spiceResult <- allocateSpicePortIfNeeded state vmId vm
  case spiceResult of
    Left err -> recordPreStartFailure state vmId vm pool "Failed to allocate SPICE port" err
    Right _ -> do
      cidResult <- liftIO $ ensureFreeVsockCid state vmId vm
      case cidResult of
        Left err ->
          recordPreStartFailure state vmId vm pool "Failed to secure a free vsock CID" err
        Right _ -> launchVmViaAgent state vmId vm pool nextStatus

-- | When the VM is not headless, allocate a SPICE port and persist
-- it on the VM row. Headless VMs short-circuit to 'Right Nothing'.
allocateSpicePortIfNeeded
  :: ServerState
  -> Int64
  -> Vm
  -> LoggingT IO (Either Text (Maybe Int))
allocateSpicePortIfNeeded state vmId vm
  | vmHeadless vm = pure (Right Nothing)
  | otherwise = liftIO $ do
      alloc <- withAllocatedSpicePort state $ \port -> do
        updated <- runSqlPool (setVmSpicePortIfCurrent vmId (vmLifecycleRevision vm) (fromMaybe (vmLifecycleRevision vm) (vmRuntimeGeneration vm)) port) (ssDbPool state)
        pure $ if updated then port else -1
      pure $ case alloc of
        Left err -> Left err
        Right (-1) -> Left "VM lifecycle operation was superseded"
        Right port -> Right (Just port)

-- | A pre-launch step failed: log, persist the error on the VM row,
-- and produce the wire response.
recordPreStartFailure
  :: ServerState
  -> Int64
  -> Vm
  -> Pool SqlBackend
  -> Text
  -- ^ message prefix (\"Failed to …\")
  -> Text
  -- ^ underlying error
  -> LoggingT IO Response
recordPreStartFailure _state vmId vm pool prefix err = do
  let msg = prefix <> ": " <> err
  logWarnN msg
  _ <- liftIO $ runSqlPool (setVmErrorIfCurrent vmId (vmLifecycleRevision vm) (fromMaybe (vmLifecycleRevision vm) (vmRuntimeGeneration vm)) msg) pool
  pure $ RespError msg

-- | Assemble 'VmSpec' from DB rows and call 'NOA.vmStart'.
-- The @nextStatus@ is the intermediate status the validator picked
-- (one of 'VmStarting', 'VmRunning', 'VmLoading'); a fresh-runtime
-- claim has already committed it before invoking the agent so 'crv vm show' can
-- distinguish "still spawning" from "fully up" and "loading saved
-- state" from "cold booting".
launchVmViaAgent
  :: ServerState
  -> Int64
  -> Vm
  -> Pool SqlBackend
  -> VmStatus
  -> LoggingT IO Response
launchVmViaAgent state vmId vm pool nextStatus =
  launchVmViaAgentAttempt state vmId vm pool nextStatus 0

-- | A VSOCK probe only observes whether the CID is free at one instant: its
-- kernel claim is released before QEMU can acquire it. The nodeagent
-- serializes that final admission window and returns 'VmStartVsockCidBusy'
-- when another VM won it. Reallocate once under the original lifecycle claim;
-- a second collision is reported as a normal start failure rather than
-- spinning forever.
launchVmViaAgentAttempt
  :: ServerState
  -> Int64
  -> Vm
  -> Pool SqlBackend
  -> VmStatus
  -> Int
  -> LoggingT IO Response
launchVmViaAgentAttempt state vmId vm pool nextStatus attempt = do
  -- Managed and bridge NICs need the netd cap so
  -- 'assembleVmSpec' can pre-allocate persistent TAPs. If the VM
  -- has none, we don't care whether netd is up.
  needsNetd <- liftIO $ runSqlPool (hasNetdMediatedNetIf vmId) pool
  mNetAgent <- liftIO $ lookupNetAgentMaybe state (M.vmNodeId vm)
  when (needsNetd && isNothing mNetAgent) $
    logWarnN $
      "VM "
        <> T.pack (show vmId)
        <> " has a managed or bridge NIC but netd is unavailable"
  let netAgentForSpec = if needsNetd then mNetAgent else Nothing
  -- Wait-for-first-ping budget for the agent's vmStart. Covers
  -- cold boot through QGA's first response.
  --
  --  * 'guestAgent' off → no wait.
  --  * 'guestAgent' on, has previously checked in (steady-state
  --    reboot) → 90 s. Handles Alpine + Debian + UEFI Alpine
  --    boots under nested KVM on a moderately loaded host.
  --  * 'guestAgent' on, never checked in AND 'cloudInit' on →
  --    300 s. First-boot cloud images do 'apt install
  --    qemu-guest-agent' from cloud-init's package list, which
  --    runs apt-get update + install over the network and only
  --    *then* starts the agent. Under parallel integration-test
  --    load this routinely needs > 90 s; the 5-min budget
  --    matches 'reapplyVm' below.
  --  * 'guestAgent' on, never checked in, no cloud-init → 90 s
  --    (image presumably has the agent baked in and started by
  --    systemd at boot).
  let cfg = ssQemuConfig state
      firstBoot = isNothing (vmHealthcheck vm)
      cloudInitBootstrap = firstBoot && vmCloudInit vm
      waitMs
        | not (vmGuestAgent vm) = 0
        | cloudInitBootstrap = 300000
        | otherwise = 90000

  mSpec <- liftIO $ NSpec.assembleVmSpec pool cfg netAgentForSpec vmId (vmLifecycleRevision vm) (fromMaybe (vmLifecycleRevision vm) (vmRuntimeGeneration vm)) waitMs
  case mSpec of
    Left err
      | "disappeared from DB" `T.isInfixOf` err -> do
          logWarnN $ "VM " <> T.pack (show vmId) <> " disappeared from DB during start"
          pure RespVmNotFound
      | otherwise -> do
          logWarnN $ "VM " <> T.pack (show vmId) <> ": assembleVmSpec: " <> err
          _ <- liftIO $ runSqlPool (setVmErrorIfCurrent vmId (vmLifecycleRevision vm) (fromMaybe (vmLifecycleRevision vm) (vmRuntimeGeneration vm)) err) pool
          pure $ RespError err
    Right spec -> do
      outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmStart nac spec
      case outer of
        Left err -> do
          logWarnN $ "nodeagent unavailable; cannot start VM: " <> err
          _ <- liftIO $ runSqlPool (setVmErrorIfCurrent vmId (vmLifecycleRevision vm) (fromMaybe (vmLifecycleRevision vm) (vmRuntimeGeneration vm)) err) pool
          pure $ RespError err
        Right r -> case r of
          Left e -> do
            let msg = "vmStart: " <> T.pack (show e)
            logWarnN $ "vmStart failed for VM " <> T.pack (show vmId) <> ": " <> T.pack (show e)
            _ <- liftIO $ runSqlPool (setVmErrorIfCurrent vmId (vmLifecycleRevision vm) (fromMaybe (vmLifecycleRevision vm) (vmRuntimeGeneration vm)) msg) pool
            pure $ RespError msg
          Right (NOA.VmStartStarted info) -> do
            let _pid = fromIntegral (NOA.vriQemuPid info) :: Int
            -- Post-split semantics: the agent's 'vmStart' returns
            -- once QEMU is spawned, *not* once QGA is reachable.
            -- The first-QGA-ping wait was moved into a forked
            -- watcher inside the agent (see 'doVmStart'). When the
            -- ping lands, the watcher dispatches a single-VM
            -- snapshot via the existing push channel and the
            -- daemon's 'VmStatusSink' promotes our DB row from
            -- 'VmStarting' or 'VmLoading' to 'VmRunning'.
            --
            -- VMs without a guest agent never trigger that push
            -- (the watcher only fires when 'waitForGuestAgentMs >
            -- 0'); promote synchronously here for them. Resume from
            -- saved always uses the push channel — the agent's load
            -- coordinator dispatches a snapshot when QMP 'cont'
            -- succeeds.
            liftIO $ attachVmMonitor state vmId
            case nextStatus of
              VmStarting -> pure $ RespVmStateChanged VmStarting
              VmLoading -> pure $ RespVmStateChanged VmLoading
              _ -> do
                updated <- liftIO $ runSqlPool (setVmStartedIfCurrent vmId (vmLifecycleRevision vm) (fromMaybe (vmLifecycleRevision vm) (vmRuntimeGeneration vm)) VmRunning) (ssDbPool state)
                pure $ if updated then RespVmStateChanged VmRunning else RespInvalidTransition nextStatus "VM lifecycle operation was superseded"
          Right NOA.VmStartVsockCidBusy
            | attempt == 0 -> retryWithFreshVsockCid
            | otherwise ->
                recordPreStartFailure
                  state
                  vmId
                  vm
                  pool
                  "Failed to secure a free vsock CID"
                  "the nodeagent reported a VSOCK CID collision after retrying"
  where
    retryWithFreshVsockCid = do
      mCurrent <- liftIO $ runSqlPool (getVmWithStatus vmId) pool
      case mCurrent of
        Nothing -> pure RespVmNotFound
        Just (current, currentStatus)
          | vmLifecycleRevision current /= vmLifecycleRevision vm
              || vmRuntimeGeneration current /= vmRuntimeGeneration vm ->
              pure $ RespInvalidTransition currentStatus "VM lifecycle operation was superseded"
          | otherwise -> do
              cidResult <- liftIO $ ensureFreeVsockCid state vmId current
              case cidResult of
                Left err ->
                  recordPreStartFailure state vmId current pool "Failed to secure a free vsock CID" err
                Right _ ->
                  launchVmViaAgentAttempt state vmId current pool nextStatus 1

-- | Re-validate the VM's stored vsock CID against the live host
-- kernel before launching QEMU, and reallocate if necessary.
--
-- Two corvus daemons (or a parallel test harness) sharing a host
-- can independently allocate the same CID from their own databases
-- because the host probe at create time isn't atomic with persisting
-- the value. The kernel enforces uniqueness when QEMU opens
-- @/dev/vhost-vsock@; the loser gets EADDRINUSE and the VM lands in
-- 'VmError'. The nodeagent owns the final host-local admission gate; this
-- preflight merely avoids asking QEMU to start with a CID already occupied by
-- a live runtime.
ensureFreeVsockCid :: ServerState -> Int64 -> Vm -> IO (Either Text Int)
ensureFreeVsockCid state vmId vm = do
  let pool = ssDbPool state
      nid = vmNodeId vm
  -- Re-probe via the node's agent. If the agent isn't reachable
  -- the call returns Left, which propagates as "vmStart can't
  -- check vsock"; the caller already routes 'Left' into
  -- 'VmError' with a clear message.
  case vmVsockCid vm of
    Nothing -> reallocate pool nid
    Just cid -> do
      r <- probeViaAgent nid cid
      if r
        then pure (Right cid)
        else reallocate pool nid
  where
    probeViaAgent nid cid = do
      mAgent <- lookupNodeAgent state nid
      case mAgent of
        Left _ -> pure False
        Right nac -> do
          res <- NOA.probeVsockCid nac cid
          pure $ case res of
            Right b -> b
            Left _ -> False
    reallocate pool nid =
      do
        alloc <- withAllocatedVsockCid state nid pure
        case alloc of
          Left err -> pure (Left err)
          Right newCid -> do
            updated <- runSqlPool (setVmVsockCidIfCurrent vmId (vmLifecycleRevision vm) (fromMaybe (vmLifecycleRevision vm) (vmRuntimeGeneration vm)) newCid) pool
            pure $ if updated then Right newCid else Left "VM lifecycle operation was superseded"

-- | Validate that a VM can be stopped.
handleVmStopValidate :: ServerState -> Int64 -> IO (Either Response (Vm, VmStatus))
handleVmStopValidate state vmId = do
  mVm <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure $ Left RespVmNotFound
    Just (vm, currentStatus) ->
      case validateTransition currentStatus ActionStop of
        Left errMsg -> pure $ Left $ RespInvalidTransition currentStatus errMsg
        Right _ -> pure $ Right (vm, currentStatus)

-- | Execute VM stop to completion (blocks until VmStopped).
--
-- After the refactor, the agent's 'vmStopGraceful' blocks until
-- QEMU has actually exited (or the timeout elapses), so the
-- daemon no longer needs to poll the DB. On graceful-timeout we
-- escalate to 'vmStopHard'.
handleVmStopExecute :: ActionContext -> Int64 -> Word32 -> IO Response
handleVmStopExecute ctx vmId timeoutSec = do
  let state = acState ctx
  validated <- handleVmStopValidate state vmId
  case validated of
    Left errResp -> pure errResp
    Right (_vm, currentStatus) -> runServerLogging state $ do
      liftIO $ runSqlPool (update (toSqlKey vmId :: VmId) [M.VmStatus =. VmStopping]) (ssDbPool state)
      let markStopped = do
            liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
            liftIO $ releaseManagedTaps state vmId
            pure $ RespVmStateChanged VmStopped
          -- Hard-kill via SIGTERM→SIGKILL. Reused by both the
          -- graceful-timeout escalation and the @timeoutSec == 0@
          -- short-circuit. 'vmStopHard' is dispatched synchronously
          -- and lock-free on the agent, so it can interrupt a stuck
          -- graceful stop already in flight for the same VM.
          forceStop = do
            outerHard <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmStopHard nac vmId Nothing
            case outerHard of
              Left err ->
                pure $ RespInvalidTransition currentStatus ("vmStopHard: " <> err)
              Right rh -> case rh of
                Right _ -> markStopped
                Left e ->
                  pure $ RespInvalidTransition currentStatus ("vmStopHard: " <> T.pack (show e))
      if timeoutSec == 0
        then do
          logInfoN $
            "VM "
              <> T.pack (show vmId)
              <> " stop requested with timeout=0; hard-killing immediately"
          forceStop
        else do
          -- Surface what the task is blocked on (visible via
          -- @crv task list/show/wait@) before the long graceful RPC.
          liftIO $
            pushTaskProgress
              state
              (fromSqlKey (acTaskId ctx))
              ("graceful-shutdown wait, ETA " <> T.pack (show timeoutSec) <> "s")
              (0, 0)
          -- 'vmStopGraceful' on the agent issues QMP @system_powerdown@
          -- (ACPI) and blocks until QEMU exits. That's the canonical
          -- guest shutdown path; the previous daemon-side QGA
          -- @guest-shutdown@ pre-call was belt-and-suspenders and is
          -- gone now that the agent owns QGA.
          outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmStopGraceful nac vmId timeoutSec
          case outer of
            Left err -> do
              logWarnN $ "nodeagent unavailable; cannot stop VM: " <> err
              liftIO $ runSqlPool (update (toSqlKey vmId :: VmId) [M.VmStatus =. currentStatus]) (ssDbPool state)
              pure $ RespInvalidTransition currentStatus err
            Right r -> case r of
              Left e -> do
                logWarnN $ "vmStopGraceful RPC failed: " <> T.pack (show e)
                pure $ RespInvalidTransition currentStatus ("vmStopGraceful: " <> T.pack (show e))
              Right res -> case NOA.vsrKind res of
                NOA.VmStopStopped -> markStopped
                NOA.VmStopAlreadyStopped -> markStopped
                NOA.VmStopTimeout -> do
                  logWarnN $
                    "VM "
                      <> T.pack (show vmId)
                      <> " did not exit within graceful window; force-stopping"
                  forceStop
                NOA.VmStopFailed ->
                  pure $ RespError ("vmStopGraceful failed: " <> NOA.vsrMessage res)

-- | Poll until VM status is VmStopped or VmError, or timeout.
waitForVmStopped :: ServerState -> Int64 -> Int -> IO ()
waitForVmStopped state vmId = go
  where
    go 0 = pure () -- timeout, return anyway
    go n = do
      mVm <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
      case mVm of
        Just (_, VmStopped) -> pure ()
        Just (_, VmError) -> pure ()
        _ -> do
          threadDelay 1000000
          go (n - 1)

-- | Handle VM pause command
-- Send QMP stop command
handleVmPause :: ServerState -> Int64 -> IO Response
handleVmPause state vmId = runServerLogging state $ do
  mVm <- liftIO $ runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just (_, currentStatus) ->
      case validateTransition currentStatus ActionPause of
        Left errMsg -> pure $ RespInvalidTransition currentStatus errMsg
        Right _ -> do
          logDebugN $ "Sending pause command to VM " <> T.pack (show vmId)
          outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmPause nac vmId
          case outer of
            Left err -> do
              logWarnN $ "nodeagent unavailable; cannot pause VM " <> T.pack (show vmId) <> ": " <> err
              pure $ RespInvalidTransition currentStatus err
            Right r -> case r of
              Left e -> do
                logWarnN $ "vmPause failed for VM " <> T.pack (show vmId) <> ": " <> T.pack (show e)
                pure $ RespInvalidTransition currentStatus ("vmPause: " <> T.pack (show e))
              Right () -> do
                logInfoN $ "VM " <> T.pack (show vmId) <> " paused"
                liftIO $ runSqlPool (setVmStatus vmId VmPaused) (ssDbPool state)
                pure $ RespVmStateChanged VmPaused

-- | Validate that a VM can be saved. Returns the current status, or
-- an error response. The status is forwarded to 'handleVmSaveExecute'
-- so the executor can commit the validator-picked 'VmSaving'
-- transition without re-reading the row.
handleVmSaveValidate :: ServerState -> Int64 -> IO (Either Response VmStatus)
handleVmSaveValidate state vmId = do
  mVm <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure $ Left RespVmNotFound
    Just (_, currentStatus) ->
      case validateTransition currentStatus ActionSave of
        Left errMsg -> pure $ Left $ RespInvalidTransition currentStatus errMsg
        Right _ -> pure $ Right currentStatus

-- | Execute the save flow.
--
-- Flips the row to 'VmSaving' (clearing VSOCK CID + SPICE port —
-- both are freed when QEMU exits, and the next @vm start@ will
-- allocate fresh ones), then asks the agent to issue QMP @migrate@
-- to the state file, wait for the write to complete, and @quit@
-- QEMU. The agent owns the state file path
-- (\<basePath\>/\<vmName\>/state.qemu).
--
-- On success: 'ActionSaveDone' → 'VmSaved'.
-- On failure: 'ActionSaveFail' → 'VmError'; QEMU may still be alive
-- and an operator @vm reset@ is required to clean up.
--
-- 'attachVmMonitor' detects the QEMU exit via the agent's ledger
-- but its 'VmSaving' arm short-circuits — the save executor owns the
-- terminal flip.
handleVmSaveExecute :: ServerState -> Int64 -> IO Response
handleVmSaveExecute state vmId = runServerLogging state $ do
  validated <- liftIO $ handleVmSaveValidate state vmId
  case validated of
    Left errResp -> pure errResp
    Right currentStatus -> do
      logInfoN $ "Saving VM " <> T.pack (show vmId) <> " state to disk"
      liftIO $
        runSqlPool
          ( update
              (toSqlKey vmId :: VmId)
              [ M.VmStatus =. VmSaving
              , M.VmVsockCid =. Nothing
              , M.VmSpicePort =. Nothing
              ]
          )
          (ssDbPool state)
      outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmSave nac vmId
      let finishWithFailure :: T.Text -> LoggingT IO Response
          finishWithFailure msg = do
            logWarnN $ "vmSave failed for VM " <> T.pack (show vmId) <> ": " <> msg
            liftIO $ runSqlPool (setVmError vmId ("vmSave: " <> msg)) (ssDbPool state)
            pure $ RespInvalidTransition currentStatus msg
      case outer of
        Left err -> finishWithFailure err
        Right r -> case r of
          Left e -> finishWithFailure (T.pack (show e))
          Right () -> do
            logInfoN $ "VM " <> T.pack (show vmId) <> " saved"
            case validateTransition VmSaving ActionSaveDone of
              Right next -> do
                liftIO $ runSqlPool (setVmStatus vmId next) (ssDbPool state)
                pure $ RespVmStateChanged next
              Left e ->
                -- Unreachable: VmSaving + ActionSaveDone is allowed.
                pure $ RespError ("save-done validator rejected: " <> e)

-- | Handle VM reset command — the universal escape hatch.
--
-- Goes through 'validateTransition' so the FSM is the single
-- source of truth ('ActionReset' always returns 'Right VmStopped').
-- The per-from-state cancel side-effect dispatch lives here:
--
--   * 'VmRunning' / 'VmStarting' / 'VmStopping' / 'VmPaused' /
--     'VmLoading' — SIGTERM-then-SIGKILL QEMU via 'NOA.vmStopHard'.
--   * 'VmSaved' — drop the state file via 'NOA.deleteSavedState';
--     no QEMU to kill.
--   * 'VmSaving' — kill QEMU (in-flight QMP @migrate@ aborts when
--     the process dies). The save executor observes the agent RPC
--     failure and skips its own status flip (the row is already
--     VmStopped by the time the executor returns).
--   * 'VmMigrating' — kill QEMU on the source if any (the agent's
--     vmStopHard is idempotent), drop the state file. Partial
--     destination-side artifacts get cleaned up on the next
--     migrate attempt; doing it now would require cross-node
--     orchestration that the FSM lock no longer holds.
--   * 'VmStopped' / 'VmError' — DB write only; idempotent.
--
-- The status is committed first so the monitor thread observes
-- 'VmStopped' and skips its own reconciliation.
handleVmReset :: ServerState -> Int64 -> IO Response
handleVmReset state vmId = runServerLogging state $ do
  mVm <- liftIO $ runSqlPool (get (toSqlKey vmId :: VmId)) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm -> case validateTransition (vmStatus vm) ActionReset of
      Left errMsg -> pure $ RespInvalidTransition (vmStatus vm) errMsg
      Right _ -> do
        mClaim <- liftIO $ runSqlPool (claimVmReset vmId) (ssDbPool state)
        case mClaim of
          Nothing -> pure $ RespInvalidTransition (vmStatus vm) "VM lifecycle changed while reset was being admitted"
          Just (revision, _) -> do
            let fromStatus = vmStatus vm
            -- First fence the nodeagent, including the no-live-process case:
            -- this is what invalidates a start still between helper spawn and
            -- ledger publication. Do not claim stopped on transport failure.
            outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmStopHard nac vmId (Just revision)
            case outer of
              Left err -> pure $ RespError ("nodeagent unavailable; reset remains stopping: " <> err)
              Right (Left e) -> pure $ RespError ("vmStopHard: " <> T.pack (show e))
              Right (Right res)
                | NOA.vsrKind res `elem` [NOA.VmStopStopped, NOA.VmStopAlreadyStopped] -> do
                    when (fromStatus `elem` [VmSaved, VmMigrating]) $ do
                      _ <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.deleteSavedState nac (vmName vm)
                      pure ()
                    completed <- liftIO $ runSqlPool (completeVmReset vmId revision) (ssDbPool state)
                    if completed
                      then do
                        liftIO $ releaseManagedTaps state vmId
                        pure $ RespVmStateChanged VmStopped
                      else do
                        mCurrent <- liftIO $ runSqlPool (getVmWithStatus vmId) (ssDbPool state)
                        pure $ case mCurrent of
                          Nothing -> RespVmNotFound
                          Just (_, status) -> RespInvalidTransition status "VM lifecycle operation was superseded"
                | otherwise -> pure $ RespError ("vmStopHard: " <> NOA.vsrMessage res)

-- | Handle VM edit command
-- Only allowed when VM is stopped. Updates only the provided fields.
handleVmEdit
  :: ServerState
  -> Int64
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -- ^ rebootQuirk
  -> Maybe Text
  -- ^ cpuModel
  -> IO Response
handleVmEdit state vmId mCpus mRam mDesc mHeadless mGuestAgent mTpm mCloudInit mAutostart mRebootQuirk mCpuModel = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (vm, status) ->
      -- 'rebootQuirk' and 'cpuModel' are consumed only at the next
      -- 'vmStart' (via 'VmSpec'), so flipping them on a running VM
      -- has no effect until the next start; allow it without
      -- forcing a stop — matches 'autostart's relaxed-edit
      -- semantics.
      let hasRuntimeEdits =
            or
              [ isJust mCpus
              , isJust mRam
              , isJust mDesc
              , isJust mHeadless
              , isJust mGuestAgent
              , isJust mTpm
              , isJust mCloudInit
              ]
       in if hasRuntimeEdits && status /= VmStopped
            then pure RespVmMustBeStopped
            else do
              tpmDeleteResult <-
                if vmTpm vm && mTpm == Just False
                  then deleteTpmStateForVm state vmId (vmName vm)
                  else pure (Right ())
              case tpmDeleteResult of
                Left err -> pure (RespError err)
                Right () -> do
                  runSqlPool
                    ( editVm
                        vmId
                        mCpus
                        mRam
                        mDesc
                        mHeadless
                        mGuestAgent
                        mTpm
                        mCloudInit
                        mAutostart
                        mRebootQuirk
                        mCpuModel
                    )
                    (ssDbPool state)
                  pure RespVmEdited

--------------------------------------------------------------------------------
-- Database Operations
--------------------------------------------------------------------------------
-- (Shared read/update helpers live in "Corvus.Handlers.Vm.Db".)

-- | Insert a 'Vm' row on the resolved node. The caller
-- ('handleVmCreate') is responsible for resolving the node ref
-- (or deferring to the scheduler) before invoking this.
createVm
  :: Text
  -> M.NodeId
  -> Int
  -> Int
  -> Maybe Text
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -- ^ rebootQuirk
  -> Text
  -- ^ cpuModel
  -> Maybe Int
  -> SqlPersistT IO Int64
createVm name nodeKey cpuCount ramMb description headless guestAgent tpm cloudInit autostart rebootQuirk cpuModel vsockCid = do
  now <- liftIO getCurrentTime
  let vm =
        Vm
          { vmName = name
          , vmNodeId = nodeKey
          , vmCreatedAt = now
          , vmStatus = VmStopped
          , vmLifecycleRevision = 0
          , vmRuntimeGeneration = Nothing
          , vmCpuCount = cpuCount
          , vmRamMb = ramMb
          , vmDescription = description
          , vmHeadless = headless
          , vmGuestAgent = guestAgent
          , vmTpm = tpm
          , vmCloudInit = cloudInit
          , vmHealthcheck = Nothing
          , vmAutostart = autostart
          , vmSpicePort = Nothing
          , vmVsockCid = vsockCid
          , vmErrorMessage = Nothing
          , vmLastErrorAt = Nothing
          , vmRebootQuirk = rebootQuirk
          , vmCpuModel = cpuModel
          }
  key <- insert vm
  pure $ fromSqlKey key

-- | Get disk IDs writable-attached to this VM that aren't shared with
-- another VM or referenced by a template — i.e. exclusively owned by
-- this VM. The post-ephemeral @vm.delete@ path uses
-- 'getEphemeralAttachedDisks' instead; this query is kept available
-- for any future caller that needs the stricter exclusive-ownership
-- predicate.
--
-- A disk qualifies when:
--
--   * It is attached to this VM through at least one writable drive
--     (i.e. some @Drive@ with @driveReadOnly = False@). Read-only
--     attachments are typically shared infrastructure — installer
--     ISOs, OVMF firmware, base images shared across templates —
--     that the VM didn't author and that other workloads may still
--     need; deleting them would also unlink the underlying file.
--
--   * No other VM has a drive referencing it. Disks shared across
--     VMs are kept; the user can drop them explicitly via
--     @disk delete@.
--
--   * No template references it. A @TemplateDrive@ can reference a
--     disk either by id (@diskImageId@) or by name (@diskName@, used
--     when the apply pipeline registers a template ahead of the
--     disk's creation); both forms count as "in use".
getExclusiveDisks :: Int64 -> SqlPersistT IO [Int64]
getExclusiveDisks vmId = do
  let key = toSqlKey vmId :: VmId
  drives <- selectList [M.DriveVmId ==. key] []
  let writableDiskKeys =
        mapMaybe (driveDiskImageId . entityVal) $
          filter (not . driveReadOnly . entityVal) drives
  notShared <- filterM (fmap not . isSharedDisk vmId) (map fromSqlKey writableDiskKeys)
  filterM (fmap not . isUsedByTemplate) notShared
  where
    isSharedDisk :: Int64 -> Int64 -> SqlPersistT IO Bool
    isSharedDisk thisVmId diskId = do
      otherDrives <- selectList [M.DriveDiskImageId ==. Just (toSqlKey diskId), M.DriveVmId !=. toSqlKey thisVmId] [LimitTo 1]
      pure $ not (null otherDrives)

    isUsedByTemplate :: Int64 -> SqlPersistT IO Bool
    isUsedByTemplate diskId = do
      let diskKey = toSqlKey diskId :: DiskImageId
      byId <- selectList [M.TemplateDriveDiskImageId ==. Just diskKey] [LimitTo 1]
      if not (null byId)
        then pure True
        else do
          mDisk <- get diskKey
          case mDisk of
            Nothing -> pure False
            Just disk -> do
              byName <- selectList [M.TemplateDriveDiskName ==. Just (diskImageName disk)] [LimitTo 1]
              pure $ not (null byName)

-- | Return the IDs of every ephemeral 'DiskImage' attached to this
-- VM. Used by the default 'vm delete' path to reap cloud-init ISOs
-- and template-instantiated disks together with their VM. An
-- ephemeral image that is somehow also attached to another VM is
-- excluded (defence in depth — sharing an ephemeral isn't expected,
-- but if it happens we must not yank it out from under a sibling).
getEphemeralAttachedDisks :: Int64 -> SqlPersistT IO [Int64]
getEphemeralAttachedDisks vmId = do
  let key = toSqlKey vmId :: VmId
  drives <- selectList [M.DriveVmId ==. key] []
  let diskKeys = mapMaybe (driveDiskImageId . entityVal) drives
  ephemKeys <- filterM isEphemeral diskKeys
  let ephemIds = map fromSqlKey ephemKeys
  filterM (fmap not . isSharedDisk vmId) ephemIds
  where
    isEphemeral :: DiskImageId -> SqlPersistT IO Bool
    isEphemeral dk = do
      mDisk <- get dk
      pure $ maybe False diskImageEphemeral mDisk

    isSharedDisk :: Int64 -> Int64 -> SqlPersistT IO Bool
    isSharedDisk thisVmId diskId = do
      otherDrives <- selectList [M.DriveDiskImageId ==. Just (toSqlKey diskId), M.DriveVmId !=. toSqlKey thisVmId] [LimitTo 1]
      pure $ not (null otherDrives)

-- | Delete a VM and all associated resources
deleteVm :: Int64 -> SqlPersistT IO ()
deleteVm vmId = do
  let key = toSqlKey vmId :: VmId
  -- Drop any build-cache rows that pin this VM as the chain owner.
  -- Cascade is enforced at the application layer (no OnDelete in the
  -- schema); cache rows for a deleted bake VM would dangle otherwise.
  deleteWhere [M.BuildCacheEntryVmId ==. key]
  -- Delete cloud-init config
  deleteBy (M.UniqueCloudInitVm key)
  -- Delete SSH key associations
  deleteWhere [M.VmSshKeyVmId ==. key]
  -- Delete drives
  deleteWhere [M.DriveVmId ==. key]
  -- Delete network interfaces
  deleteWhere [M.NetworkInterfaceVmId ==. key]
  -- Delete shared directories
  deleteWhere [M.SharedDirVmId ==. key]
  -- Delete VM
  delete key

-- | List all VMs
listVms :: SqlPersistT IO [VmInfo]
listVms = do
  vms <- selectList [] [Asc M.VmName]
  -- Bulk-fetch the nodes so we can stamp the display name on each
  -- VM row without an N+1 of single-row gets. Empty fallback for
  -- a (rare) Vm whose Node row has been deleted under it.
  nodes <- selectList [] []
  let nameOf = Map.fromList [(entityKey n, M.nodeName (entityVal n)) | n <- nodes]
  pure $ map (toVmInfo nameOf) vms
  where
    toVmInfo nameOf (Entity key vm) =
      VmInfo
        { viId = fromSqlKey key
        , viName = vmName vm
        , viNode =
            NamedRef
              { nrId = fromSqlKey (vmNodeId vm)
              , nrName = Map.findWithDefault "(deleted)" (vmNodeId vm) nameOf
              }
        , viStatus = vmStatus vm
        , viCpuCount = vmCpuCount vm
        , viRamMb = vmRamMb vm
        , viHeadless = vmHeadless vm
        , viGuestAgent = vmGuestAgent vm
        , viTpm = vmTpm vm
        , viCloudInit = vmCloudInit vm
        , viHealthcheck = vmHealthcheck vm
        , viAutostart = vmAutostart vm
        , viRebootQuirk = vmRebootQuirk vm
        , viCpuModel = vmCpuModel vm
        }

-- | Get full VM details. Re-exported so 'Corvus.Handlers.Build' can
-- read the bake VM and expose its identity to provisioner shell steps
-- (see @CORVUS_BAKEVM*@ environment variables).
getVmDetails :: QemuConfig -> Int64 -> SqlPersistT IO (Maybe VmDetails)
getVmDetails config vmId = do
  let key = toSqlKey vmId :: VmId
  mVm <- get key
  case mVm of
    Nothing -> pure Nothing
    Just vm -> do
      drives <- selectList [M.DriveVmId ==. key] [Asc M.DriveId]
      netIfs <- selectList [M.NetworkInterfaceVmId ==. key] []
      -- Look up the node name for display. Sentinel on a missing
      -- row (race against a node delete) matches 'listVms'.
      mNode <- get (vmNodeId vm)
      let nodeName' = maybe "(deleted)" M.nodeName mNode
          -- VM's node basePath is used to absolutise relative
          -- DiskImageNode paths for the user-facing 'DriveInfo'
          -- — same convention as 'handleDiskShow' (drives lie on
          -- the VM's node, so anchor against its basePath, not
          -- the daemon's).
          vmNodeBasePath = maybe "" (T.unpack . M.nodeBasePath) mNode
      -- Get socket paths
      monitorSock <- liftIO $ getMonitorSocket config vmId
      serialSock <- liftIO $ getSerialSocket config vmId
      guestAgentSock <- liftIO $ getGuestAgentSocket config vmId
      -- Build drive info by fetching disk images
      driveInfos <- mapM (toDriveInfo (vmNodeId vm) vmNodeBasePath) drives
      -- Build NIC info by resolving each NIC's network reference
      netIfInfos <- mapM toNetIfInfo netIfs
      -- Get custom cloud-init config if present
      mCiConfig <- getBy (M.UniqueCloudInitVm key)
      let ciInfo =
            fmap
              ( \(Entity _ ci) ->
                  CloudInitInfo
                    { ciiUserData = cloudInitUserData ci
                    , ciiNetworkConfig = cloudInitNetworkConfig ci
                    , ciiInjectSshKeys = cloudInitInjectSshKeys ci
                    }
              )
              mCiConfig
      pure $
        Just
          VmDetails
            { vdId = vmId
            , vdName = vmName vm
            , vdNode = NamedRef {nrId = fromSqlKey (vmNodeId vm), nrName = nodeName'}
            , vdCreatedAt = vmCreatedAt vm
            , vdStatus = vmStatus vm
            , vdCpuCount = vmCpuCount vm
            , vdRamMb = vmRamMb vm
            , vdDescription = vmDescription vm
            , vdDrives = driveInfos
            , vdNetIfs = netIfInfos
            , vdHeadless = vmHeadless vm
            , vdMonitorSocket = T.pack monitorSock
            , vdSpicePort = vmSpicePort vm
            , vdVsockCid = vmVsockCid vm
            , vdSerialSocket = T.pack serialSock
            , vdGuestAgentSocket = T.pack guestAgentSock
            , vdGuestAgent = vmGuestAgent vm
            , vdTpm = vmTpm vm
            , vdCloudInit = vmCloudInit vm
            , vdCloudInitConfig = ciInfo
            , vdHealthcheck = vmHealthcheck vm
            , vdAutostart = vmAutostart vm
            , vdErrorMessage = vmErrorMessage vm
            , vdLastErrorAt = vmLastErrorAt vm
            , vdRebootQuirk = vmRebootQuirk vm
            , vdCpuModel = vmCpuModel vm
            , vdStats = Corvus.Protocol.zeroVmStats
            }
  where
    -- \^ Daemon handlers don't fill vdStats; the RPC layer
    -- looks up the latest sample from 'ssVmStatsRing' and
    -- threads it into 'toCapnpVmDetails' separately.

    toDriveInfo vmNode vmNodeBasePath (Entity driveKey drive) = do
      case driveDiskImageId drive of
        -- Ejected media drive: the tray is empty and no 'DiskImage'
        -- is attached.
        Nothing ->
          pure
            DriveInfo
              { diId = fromSqlKey driveKey
              , diDiskImage = Nothing
              , diInterface = driveInterface drive
              , diFilePath = T.empty
              , diFormat = FormatRaw
              , diMedia = driveMedia drive
              , diReadOnly = driveReadOnly drive
              , diCacheType = driveCacheType drive
              , diDiscard = driveDiscard drive
              }
        Just diskImageKey -> do
          mDiskImage <- get diskImageKey
          case mDiskImage of
            Nothing ->
              pure
                DriveInfo
                  { diId = fromSqlKey driveKey
                  , diDiskImage =
                      Just (NamedRef {nrId = fromSqlKey diskImageKey, nrName = "(deleted)"})
                  , diInterface = driveInterface drive
                  , diFilePath = "(deleted)"
                  , diFormat = FormatRaw
                  , diMedia = driveMedia drive
                  , diReadOnly = driveReadOnly drive
                  , diCacheType = driveCacheType drive
                  , diDiscard = driveDiscard drive
                  }
            Just diskImage -> do
              -- Resolve the file path from the DiskImageNode row for
              -- the VM's node — single-node deployments produce exactly
              -- one row, multi-node deployments resolve to the path on
              -- the VM's host. Stored form is relative-to-basePath
              -- (or absolute when registered outside basePath); we
              -- absolutise here against the VM's node basePath so the
              -- DTO matches what 'disks.show()' returns. Missing row
              -- yields the empty string, which the CLI renders as
              -- "(not present)".
              mPath <- diskImageNodeFilePathFor diskImageKey vmNode
              let absPath = case mPath of
                    Nothing -> T.empty
                    Just stored ->
                      let raw = T.unpack stored
                       in if "/" `isPrefixOf` raw
                            then stored
                            else T.pack (vmNodeBasePath </> raw)
              pure
                DriveInfo
                  { diId = fromSqlKey driveKey
                  , diDiskImage =
                      Just
                        ( NamedRef
                            { nrId = fromSqlKey diskImageKey
                            , nrName = diskImageName diskImage
                            }
                        )
                  , diInterface = driveInterface drive
                  , diFilePath = absPath
                  , diFormat = diskImageFormat diskImage
                  , diMedia = driveMedia drive
                  , diReadOnly = driveReadOnly drive
                  , diCacheType = driveCacheType drive
                  , diDiscard = driveDiscard drive
                  }
    toNetIfInfo (Entity netIfKey netIf) = do
      networkRef <- case networkInterfaceNetworkId netIf of
        Nothing -> pure Nothing
        Just nwKey -> do
          mNw <- get nwKey
          pure $
            fmap
              (\nw -> NamedRef {nrId = fromSqlKey nwKey, nrName = networkName nw})
              mNw
      pure
        NetIfInfo
          { niId = fromSqlKey netIfKey
          , niType = networkInterfaceInterfaceType netIf
          , niHostDevice = networkInterfaceHostDevice netIf
          , niMacAddress = networkInterfaceMacAddress netIf
          , niNetwork = networkRef
          , niGuestIpAddresses = networkInterfaceGuestIpAddresses netIf
          , niIpAddress = networkInterfaceIpAddress netIf
          }

-- | Edit VM properties. Only updates fields that are Just.
editVm
  :: Int64
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -- ^ rebootQuirk
  -> Maybe Text
  -- ^ cpuModel
  -> SqlPersistT IO ()
editVm vmId mCpus mRam mDesc mHeadless mGuestAgent mTpm mCloudInit mAutostart mRebootQuirk mCpuModel = do
  let key = toSqlKey vmId :: VmId
      updates =
        maybe [] (\cpus -> [M.VmCpuCount =. cpus]) mCpus
          ++ maybe [] (\ram -> [M.VmRamMb =. ram]) mRam
          ++ maybe [] (\desc -> [M.VmDescription =. Just desc]) mDesc
          ++ maybe [] (\h -> [M.VmHeadless =. h]) mHeadless
          ++ maybe [] (\ga -> [M.VmGuestAgent =. ga]) mGuestAgent
          ++ maybe [] (\tpm -> [M.VmTpm =. tpm]) mTpm
          ++ maybe [] (\ci -> [M.VmCloudInit =. ci]) mCloudInit
          ++ maybe [] (\a -> [M.VmAutostart =. a]) mAutostart
          ++ maybe [] (\rq -> [M.VmRebootQuirk =. rq]) mRebootQuirk
          ++ maybe [] (\cm -> [M.VmCpuModel =. cm]) mCpuModel
  case updates of
    [] -> pure ()
    us -> update key us

-- | Check if all networks referenced by a VM's network interfaces are running.
-- Returns Just networkName if a stopped network is found, Nothing if all are running.
checkNetworksRunning :: Int64 -> SqlPersistT IO (Maybe Text)
checkNetworksRunning vmId = do
  let vmKey = toSqlKey vmId :: VmId
  netIfs <- selectList [M.NetworkInterfaceVmId ==. vmKey] []
  let networkKeys = [nwKey | Entity _ ni <- netIfs, Just nwKey <- [networkInterfaceNetworkId ni]]
  go networkKeys
  where
    go [] = pure Nothing
    go (nwKey : rest) = do
      mNetwork <- get nwKey
      case mNetwork of
        Nothing -> pure $ Just "unknown (deleted)"
        Just network ->
          if networkRunning network
            then go rest
            else pure $ Just $ networkName network

--------------------------------------------------------------------------------
-- Action Types (simple handlers)
--------------------------------------------------------------------------------

data VmCreate = VmCreate
  { vcrName :: Text
  , vcrNodeRef :: Text
  -- ^ Reference to the target node (name or numeric id). Empty
  -- string / @"0"@ defers to
  -- 'Corvus.Handlers.Scheduler.pickNodeForVm'; non-empty is
  -- resolved by 'handleVmCreate'.
  , vcrCpuCount :: Int
  , vcrRamMb :: Int
  , vcrDescription :: Maybe Text
  , vcrHeadless :: Bool
  , vcrGuestAgent :: Bool
  , vcrTpm :: Bool
  , vcrCloudInit :: Bool
  , vcrAutostart :: Bool
  , vcrRebootQuirk :: Bool
  , vcrCpuModel :: Text
  -- ^ QEMU @-cpu@ model. Empty == use the daemon default
  -- ('host'); see the schema field comment on
  -- @schema/vm.capnp::VmInfo.cpuModel@ for the
  -- migration-safety trade-off.
  }

instance Action VmCreate where
  actionSubsystem _ = SubVm
  actionCommand _ = "create"
  actionEntityName = Just . vcrName
  actionExecute ctx a =
    handleVmCreate
      (acState ctx)
      (vcrName a)
      (vcrNodeRef a)
      (vcrCpuCount a)
      (vcrRamMb a)
      (vcrDescription a)
      (vcrHeadless a)
      (vcrGuestAgent a)
      (vcrTpm a)
      (vcrCloudInit a)
      (vcrAutostart a)
      (vcrRebootQuirk a)
      (vcrCpuModel a)

data VmDelete = VmDelete
  { vdelVmId :: Int64
  , vdelKeepDisks :: Bool
  -- ^ When 'False' (default), delete every ephemeral disk attached
  -- to the VM (cloud-init ISOs, template-instantiated disks). When
  -- 'True', leave all attached disks in place — including ephemeral
  -- ones — so the operator can debug or re-use them.
  }

instance Action VmDelete where
  actionSubsystem _ = SubVm
  actionCommand _ = "delete"
  actionEntityId = Just . fromIntegral . vdelVmId
  actionExecute ctx a = handleVmDelete ctx (vdelVmId a) (vdelKeepDisks a)

data VmEdit = VmEdit
  { vedVmId :: Int64
  , vedCpus :: Maybe Int
  , vedRam :: Maybe Int
  , vedDesc :: Maybe Text
  , vedHeadless :: Maybe Bool
  , vedGuestAgent :: Maybe Bool
  , vedTpm :: Maybe Bool
  , vedCloudInit :: Maybe Bool
  , vedAutostart :: Maybe Bool
  , vedRebootQuirk :: Maybe Bool
  , vedCpuModel :: Maybe Text
  }

instance Action VmEdit where
  actionSubsystem _ = SubVm
  actionCommand _ = "edit"
  actionEntityId = Just . fromIntegral . vedVmId
  actionExecute ctx a =
    handleVmEdit
      (acState ctx)
      (vedVmId a)
      (vedCpus a)
      (vedRam a)
      (vedDesc a)
      (vedHeadless a)
      (vedGuestAgent a)
      (vedTpm a)
      (vedCloudInit a)
      (vedAutostart a)
      (vedRebootQuirk a)
      (vedCpuModel a)

newtype VmPause = VmPause {vpVmId :: Int64}

instance Action VmPause where
  actionSubsystem _ = SubVm
  actionCommand _ = "pause"
  actionEntityId = Just . fromIntegral . vpVmId
  actionExecute ctx a = handleVmPause (acState ctx) (vpVmId a)

newtype VmSave = VmSave {vsaveVmId :: Int64}

instance Action VmSave where
  actionSubsystem _ = SubVm
  actionCommand _ = "save"
  actionEntityId = Just . fromIntegral . vsaveVmId
  actionValidate state a = do
    result <- handleVmSaveValidate state (vsaveVmId a)
    pure $ case result of
      Left errResp -> Just errResp
      Right _ -> Nothing
  actionExecute ctx a = handleVmSaveExecute (acState ctx) (vsaveVmId a)

newtype VmReset = VmReset {vrstVmId :: Int64}

instance Action VmReset where
  actionSubsystem _ = SubVm
  actionCommand _ = "reset"
  actionEntityId = Just . fromIntegral . vrstVmId
  actionExecute ctx a = handleVmReset (acState ctx) (vrstVmId a)

-- Complex handlers with validate/execute split

newtype VmStart = VmStart {vsVmId :: Int64}

instance Action VmStart where
  actionSubsystem _ = SubVm
  actionCommand _ = "start"
  actionEntityId = Just . fromIntegral . vsVmId
  actionValidate state a = do
    result <- handleVmStartValidate state (vsVmId a)
    pure $ case result of
      Left errResp -> Just errResp
      Right _ -> Nothing
  actionExecute ctx a = handleVmStartExecute ctx (vsVmId a)

-- | Stop a VM. @vstpTimeout@ is the graceful-shutdown window in
-- seconds (0 = skip graceful, hard-kill immediately).
data VmStop = VmStop
  { vstpVmId :: Int64
  , vstpTimeout :: Word32
  }

instance Action VmStop where
  actionSubsystem _ = SubVm
  actionCommand _ = "stop"
  actionEntityId = Just . fromIntegral . vstpVmId
  actionValidate state a = do
    result <- handleVmStopValidate state (vstpVmId a)
    pure $ case result of
      Left errResp -> Just errResp
      Right _ -> Nothing
  actionExecute ctx a = handleVmStopExecute ctx (vstpVmId a) (vstpTimeout a)

-- (Phase 3 refactor: the @StartVirtiofsd@ and @LaunchQemu@
-- subtask actions are gone — virtiofsd is implicit in 'VmSpec',
-- and QEMU spawning happens inline via 'NOA.vmStart' in
-- 'launchVmViaAgent' rather than as a separate subtask.)

-- | Per-node autostart pass. Called by the per-node supervisor's
-- nodeagent @onConnect@ callback the FIRST time it lands a
-- successful dial after the supervisor spawned — see
-- 'claimAutostartSlot' for the once-per-supervisor-lifetime gate.
--
-- By the time we get here, 'ssAgents' has the node's nodeagent
-- cap registered, so 'VmStart' downstream can allocate vsock
-- CIDs against the node without racing the daemon's startup task
-- (the pre-fix bug had the autostart loop firing before any agent
-- had connected). 'reattachVmMonitors' has also already run, so
-- any VMs the agent still has alive are getting their monitor
-- back; autostart strictly handles the @{stopped, saved}@ side.
--
-- Lives in the umbrella module (not in "Corvus.Handlers.Vm.Monitor")
-- because it issues 'VmStart' actions defined here, and the monitor
-- sub-module must not import back into the umbrella.
autostartVmsOnNode :: ServerState -> M.NodeId -> IO ()
autostartVmsOnNode state nodeId = do
  let pool = ssDbPool state
  vms <-
    runSqlPool
      ( selectList
          [ M.VmAutostart ==. True
          , M.VmNodeId ==. nodeId
          , M.VmStatus <-. [M.VmStopped, M.VmSaved]
          ]
          [Asc M.VmName]
      )
      pool
  runServerLogging state $
    unless (null vms) $ do
      logInfoN $
        "Autostarting " <> T.pack (show (length vms)) <> " VM(s) on node " <> T.pack (show (fromSqlKey nodeId))
      forM_ vms $ \(Entity vmKey vm) -> do
        resp <- liftIO $ runAction state autostartClientName (VmStart (fromSqlKey vmKey))
        case classifyResponse resp of
          (TaskError, Just err) ->
            logWarnN $ "Failed to autostart VM " <> vmName vm <> ": " <> err
          _ -> logInfoN $ "Autostarted VM " <> vmName vm

-- | @client_name@ on task rows produced by per-node autostart.
-- Surfaces in @crv task history@ so operators can tell
-- autostart-driven starts apart from operator-issued ones.
autostartClientName :: Text
autostartClientName = "system-autostart"
