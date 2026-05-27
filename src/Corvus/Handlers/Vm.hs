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

    -- * Handlers
  , handleVmList
  , handleVmShow
  , handleVmCreate
  , handleVmDelete
  , attachVmMonitor
  , reattachVmMonitors
  , handleVmPause
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
  )
where

import Corvus.Action

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Monad (filterM, unless, when)
import qualified Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.Handlers.CloudInit (RegenerateCloudInit (..))
import Corvus.Handlers.Disk (DiskDelete (..))
import Corvus.Handlers.Disk.Db (diskImageNodeFilePathFor)
import Corvus.Handlers.Resolve (resolveNode, validateName)
import Corvus.Handlers.Scheduler (pickNodeForVm)
import Corvus.Model (DriveFormat (..), VmStatus (..))
import Corvus.Model hiding (DriveFormat, VmStatus)
import qualified Corvus.Model as M
import Corvus.Model.VmState (VmAction (..), validateTransition)
import qualified Corvus.NetAgentClient as NA
import qualified Corvus.NetAgentClient.Spec as Spec
import Corvus.Node.SpicePort (withAllocatedSpicePort)
import Corvus.Node.VsockCid (withAllocatedVsockCid)
import qualified Corvus.NodeAgentClient as NOA
import qualified Corvus.NodeAgentClient.Spec as NSpec
import Corvus.NodeRouting (withVmNetAgent, withVmNodeAgent)
import Corvus.Protocol
import Corvus.Qemu
import Corvus.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import Data.Int (Int64)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Word (Word32)
import Database.Persist
import Database.Persist.Postgresql (SqlBackend, runSqlPool)
import Database.Persist.Sql (SqlPersistT)
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), withBinaryFile)

-- | VM statuses in which a user may attach to the console, HMP monitor,
-- or SPICE viewer. Anything non-@stopped@ where QEMU is (or should soon
-- be) alive — deliberately excludes 'VmPaused' (no live I/O) and
-- 'VmError' (QEMU has already died).
viewableStatuses :: [VmStatus]
viewableStatuses = [VmRunning, VmStarting, VmStopping]

-- | True when the VM is in a state that accepts console/monitor/view attach.
isViewable :: VmStatus -> Bool
isViewable = (`elem` viewableStatuses)

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
  -- ^ rebootQuirk
  -> IO Response
handleVmCreate state name nodeRefText cpuCount ramMb description headless guestAgent cloudInit autostart rebootQuirk =
  case validateName "VM" name of
    Left err -> pure $ RespError err
    Right () -> do
      let pool = ssDbPool state
      -- Empty text == operator did not pass @--node@; capnp's
      -- unset-EntityRef default ('byId 0') also lands here.
      -- Either way, defer to the scheduler.
      eNodeKey <-
        if T.null nodeRefText || nodeRefText == "0"
          then pickNodeForVm state ramMb
          else do
            r <- resolveNode (Ref nodeRefText) pool
            pure $ fmap (M.toSqlKey :: Int64 -> M.NodeId) r
      case eNodeKey of
        Left err -> pure $ RespError err
        Right nodeKey -> do
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
                  (createVm name nodeKey cpuCount ramMb description headless guestAgent cloudInit autostart rebootQuirk (Just cid))
                  pool
            case r of
              Right vmId -> pure (Right vmId)
              Left _ -> do
                vmId <-
                  runSqlPool
                    (createVm name nodeKey cpuCount ramMb description headless guestAgent cloudInit autostart rebootQuirk Nothing)
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
    Just (_, status) ->
      if status `elem` [VmRunning, VmStarting, VmStopping, VmPaused]
        then pure RespVmRunning
        else do
          disksToDelete <-
            if keepDisks
              then pure []
              else runSqlPool (getEphemeralAttachedDisks vmId) (ssDbPool state)
          -- Delete VM and its associations (drives, netifs, etc.)
          runSqlPool (deleteVm vmId) (ssDbPool state)
          -- Reap each ephemeral disk as a subtask.
          mapM_ (runActionAsSubtask ctx . DiskDelete) disksToDelete
          pure RespVmDeleted

-- | Validate that a VM can be started. Returns the VM and current status, or an error response.
-- Checks: VM exists, state transition valid, all referenced networks are running.
handleVmStartValidate :: ServerState -> Int64 -> IO (Either Response (Vm, VmStatus))
handleVmStartValidate state vmId = do
  mVm <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure $ Left RespVmNotFound
    Just (vm, currentStatus)
      | vmMigrating vm ->
          pure $
            Left $
              RespError "VM is being migrated; wait for the migration to complete"
      | otherwise ->
          case validateTransition currentStatus ActionStart of
            Left errMsg -> pure $ Left $ RespInvalidTransition currentStatus errMsg
            Right _ -> do
              -- Check that all referenced networks are running (only for cold start)
              if currentStatus == VmStopped
                then do
                  networkCheck <- runSqlPool (checkNetworksRunning vmId) (ssDbPool state)
                  case networkCheck of
                    Just networkName ->
                      pure $ Left $ RespInvalidTransition VmStopped $ "Network '" <> networkName <> "' is not running"
                    Nothing -> pure $ Right (vm, currentStatus)
                else pure $ Right (vm, currentStatus)

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
    Right (vm, currentStatus) ->
      case currentStatus of
        VmPaused -> runServerLogging state $ resumeFromPaused state vmId
        _ -> runServerLogging state $ do
          resp <- startQemuAndMonitor ctx vmId vm
          case resp of
            RespVmStateChanged VmStarting ->
              -- The agent's vmStart now returns after QEMU spawn,
              -- before the first QGA ping. Block here until the
              -- push channel finishes the transition — either to
              -- 'VmRunning' (ping landed) or 'VmError' (first
              -- ping never came / QEMU crashed early). Without
              -- this poll, callers passing @wait=true@ would see
              -- the RPC return at 'VmStarting' and immediately
              -- fail their first @vm.exec@ because QGA isn't yet
              -- reachable.
              liftIO $ waitForStartCompletion state vmId
            RespVmStateChanged _ -> pure resp
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
            Just VmStarting -> do
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
resumeFromPaused :: ServerState -> Int64 -> LoggingT IO Response
resumeFromPaused state vmId = do
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
        liftIO $ runSqlPool (setVmStatus vmId VmRunning) (ssDbPool state)
        pure $ RespVmStateChanged VmRunning

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
startQemuAndMonitor :: ActionContext -> Int64 -> Vm -> LoggingT IO Response
startQemuAndMonitor ctx vmId vm = do
  let state = acState ctx
      pool = ssDbPool state
  ensureCloudInitIso ctx vmId vm
  spiceResult <- allocateSpicePortIfNeeded state vmId vm
  case spiceResult of
    Left err -> recordPreStartFailure state vmId pool "Failed to allocate SPICE port" err
    Right _ -> do
      cidResult <- liftIO $ ensureFreeVsockCid state vmId vm
      case cidResult of
        Left err ->
          recordPreStartFailure state vmId pool "Failed to secure a free vsock CID" err
        Right _ -> launchVmViaAgent state vmId vm pool

-- | Generate the NoCloud cloud-init ISO for this VM (if cloud-init
-- is enabled and the ISO isn't already attached). Failures are
-- non-fatal — the subtask records its own error.
ensureCloudInitIso :: ActionContext -> Int64 -> Vm -> LoggingT IO ()
ensureCloudInitIso ctx vmId vm = when (vmCloudInit vm) $ do
  hasIso <- liftIO $ runSqlPool (hasCloudInitIso vmId) (ssDbPool (acState ctx))
  unless hasIso $ do
    logInfoN $ "Generating cloud-init ISO for VM " <> T.pack (show vmId)
    _ <- liftIO $ runActionAsSubtask ctx (RegenerateCloudInit vmId (vmName vm))
    pure ()

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
        runSqlPool (update (toSqlKey vmId :: VmId) [M.VmSpicePort =. Just port]) (ssDbPool state)
        pure port
      pure $ case alloc of
        Left err -> Left err
        Right port -> Right (Just port)

-- | A pre-launch step failed: log, persist the error on the VM row,
-- and produce the wire response.
recordPreStartFailure
  :: ServerState
  -> Int64
  -> Pool SqlBackend
  -> Text
  -- ^ message prefix (\"Failed to …\")
  -> Text
  -- ^ underlying error
  -> LoggingT IO Response
recordPreStartFailure _state vmId pool prefix err = do
  let msg = prefix <> ": " <> err
  logWarnN msg
  liftIO $ runSqlPool (setVmError vmId msg) pool
  pure $ RespError msg

-- | Assemble 'VmSpec' from DB rows and call 'NOA.vmStart'.
launchVmViaAgent
  :: ServerState
  -> Int64
  -> Vm
  -> Pool SqlBackend
  -> LoggingT IO Response
launchVmViaAgent state vmId vm pool = do
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

  mSpec <- liftIO $ NSpec.assembleVmSpec pool cfg netAgentForSpec vmId waitMs
  case mSpec of
    Left err
      | "disappeared from DB" `T.isInfixOf` err -> do
          logWarnN $ "VM " <> T.pack (show vmId) <> " disappeared from DB during start"
          pure RespVmNotFound
      | otherwise -> do
          logWarnN $ "VM " <> T.pack (show vmId) <> ": assembleVmSpec: " <> err
          liftIO $ runSqlPool (setVmError vmId err) pool
          pure $ RespError err
    Right spec -> do
      -- Set 'VmStarting' before invoking the agent. The agent's
      -- 'vmStart' RPC blocks for QEMU spawn and (when guestAgent
      -- is set) the first QGA ping — that window can run from
      -- ~1 s up to the cloud-init bootstrap budget. Without this
      -- pre-call write, the DB stays at 'VmStopped' for the
      -- entire wait and then jumps straight to 'VmRunning' — so
      -- 'crv vm show' can't distinguish "still spawning" from
      -- "fully up". Mirrors how the stop path sets 'VmStopping'
      -- before 'NOA.vmStopGraceful'.
      liftIO $ runSqlPool (setVmStatus vmId VmStarting) pool
      outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmStart nac spec
      case outer of
        Left err -> do
          logWarnN $ "nodeagent unavailable; cannot start VM: " <> err
          liftIO $ runSqlPool (setVmError vmId err) pool
          pure $ RespError err
        Right r -> case r of
          Left e -> do
            let msg = "vmStart: " <> T.pack (show e)
            logWarnN $ "vmStart failed for VM " <> T.pack (show vmId) <> ": " <> T.pack (show e)
            liftIO $ runSqlPool (setVmError vmId msg) pool
            -- 'RespError' (not 'RespInvalidTransition') so the Cap'n
            -- Proto wire layer throws on the client; matches the
            -- pre-Phase-4 daemon-side failure semantics that the
            -- @start-virtiofsd@ subtask used to surface.
            pure $ RespError msg
          Right info -> do
            let pid = fromIntegral (NOA.vriQemuPid info) :: Int
            -- Post-split semantics: the agent's 'vmStart' returns
            -- once QEMU is spawned, *not* once QGA is reachable.
            -- The first-QGA-ping wait was moved into a forked
            -- watcher inside the agent (see 'doVmStart'). When the
            -- ping lands, the watcher dispatches a single-VM
            -- snapshot via the existing push channel and the
            -- daemon's 'VmStatusSink' promotes our DB row from
            -- 'VmStarting' to 'VmRunning'. Goal: unblock the
            -- agent's session RPC thread so it can process N
            -- concurrent vmStart calls without queuing behind one
            -- VM's first-boot latency.
            --
            -- VMs without a guest agent never trigger that push
            -- (the watcher only fires when 'waitForGuestAgentMs >
            -- 0'); promote synchronously here for them.
            liftIO $ attachVmMonitor state vmId
            if vmGuestAgent vm
              then pure $ RespVmStateChanged VmStarting
              else do
                liftIO $ runSqlPool (setVmStarted vmId VmRunning pid) (ssDbPool state)
                pure $ RespVmStateChanged VmRunning

-- | Re-validate the VM's stored vsock CID against the live host
-- kernel before launching QEMU, and reallocate if necessary.
--
-- Two corvus daemons (or a parallel test harness) sharing a host
-- can independently allocate the same CID from their own databases
-- because the host probe at create time isn't atomic with persisting
-- the value. The kernel enforces uniqueness when QEMU opens
-- @/dev/vhost-vsock@; the loser gets EADDRINUSE and the VM lands in
-- 'VmError'. Re-probing here closes that race in the common case
-- (the only way to fail now is for two daemons to call this function
-- in lockstep, which is rare in practice).
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
      withAllocatedVsockCid state nid $ \newCid -> do
        runSqlPool (update (toSqlKey vmId :: VmId) [M.VmVsockCid =. Just newCid]) pool
        pure newCid

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
handleVmStopExecute :: ServerState -> Int64 -> IO Response
handleVmStopExecute state vmId = do
  validated <- handleVmStopValidate state vmId
  case validated of
    Left errResp -> pure errResp
    Right (_vm, currentStatus) -> runServerLogging state $ do
      liftIO $ runSqlPool (update (toSqlKey vmId :: VmId) [M.VmStatus =. VmStopping]) (ssDbPool state)
      -- 'vmStopGraceful' on the agent issues QMP @system_powerdown@
      -- (ACPI) and blocks until QEMU exits. That's the canonical
      -- guest shutdown path; the previous daemon-side QGA
      -- @guest-shutdown@ pre-call was belt-and-suspenders and is
      -- gone now that the agent owns QGA.
      outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmStopGraceful nac vmId 300
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
            NOA.VmStopStopped -> do
              liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
              liftIO $ releaseManagedTaps state vmId
              pure $ RespVmStateChanged VmStopped
            NOA.VmStopAlreadyStopped -> do
              liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
              liftIO $ releaseManagedTaps state vmId
              pure $ RespVmStateChanged VmStopped
            NOA.VmStopTimeout -> do
              logWarnN $
                "VM "
                  <> T.pack (show vmId)
                  <> " did not exit within graceful window; force-stopping"
              outerHard <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmStopHard nac vmId
              case outerHard of
                Left err ->
                  pure $ RespInvalidTransition currentStatus ("vmStopHard: " <> err)
                Right rh -> case rh of
                  Right _ -> do
                    liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
                    liftIO $ releaseManagedTaps state vmId
                    pure $ RespVmStateChanged VmStopped
                  Left e ->
                    pure $ RespInvalidTransition currentStatus ("vmStopHard: " <> T.pack (show e))
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

-- | Handle VM reset command.
-- Asks the agent to SIGTERM-then-SIGKILL QEMU + every virtiofsd
-- helper for this vmId, then marks the VM stopped. With the agent
-- owning all PIDs, the daemon doesn't need to look them up.
--
-- The status is moved to 'VmStopped' before the @vmStopHard@ RPC
-- fires, so the background monitor thread observes the terminal
-- state when QEMU exits and skips its own reconciliation
-- (mirrors the old @clearVmPid@ signal that the @Vm.pid@ column
-- carried before it was dropped).
handleVmReset :: ServerState -> Int64 -> IO Response
handleVmReset state vmId = runServerLogging state $ do
  mVm <- liftIO $ runSqlPool (get (toSqlKey vmId :: VmId)) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just _ -> do
      -- Commit the terminal status first; the monitor checks
      -- status before reconciling and will back off when it sees
      -- the row is already stopped.
      liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)

      outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmStopHard nac vmId
      case outer of
        Left err ->
          logWarnN $
            "nodeagent unavailable; reset only updates DB state for VM "
              <> T.pack (show vmId)
              <> ": "
              <> err
        Right r -> case r of
          Left e ->
            logWarnN $ "vmStopHard failed for VM " <> T.pack (show vmId) <> ": " <> T.pack (show e)
          Right res -> case NOA.vsrKind res of
            NOA.VmStopStopped ->
              logInfoN $ "VM " <> T.pack (show vmId) <> " process killed"
            NOA.VmStopAlreadyStopped ->
              logDebugN $ "VM " <> T.pack (show vmId) <> " was not in the agent's ledger"
            _ ->
              logWarnN $ "vmStopHard returned: " <> NOA.vsrMessage res

      -- Drop netd-allocated TAPs synchronously *before* returning,
      -- so a follow-up @vmDelete@ that wipes the NIC rows doesn't
      -- race the background monitor's own (now no-op) cleanup pass.
      liftIO $ releaseManagedTaps state vmId

      pure $ RespVmStateChanged VmStopped

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
  -- ^ rebootQuirk
  -> IO Response
handleVmEdit state vmId mCpus mRam mDesc mHeadless mGuestAgent mCloudInit mAutostart mRebootQuirk = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (_, status) ->
      -- 'rebootQuirk' is consumed only at the next 'vmStart' (via
      -- 'VmSpec'), so flipping it on a running VM has no effect
      -- until the next start; allow it without forcing a stop —
      -- matches 'autostart's relaxed-edit semantics.
      let hasRuntimeEdits =
            or
              [ isJust mCpus
              , isJust mRam
              , isJust mDesc
              , isJust mHeadless
              , isJust mGuestAgent
              , isJust mCloudInit
              ]
       in if hasRuntimeEdits && status /= VmStopped
            then pure RespVmMustBeStopped
            else do
              runSqlPool
                ( editVm
                    vmId
                    mCpus
                    mRam
                    mDesc
                    mHeadless
                    mGuestAgent
                    mCloudInit
                    mAutostart
                    mRebootQuirk
                )
                (ssDbPool state)
              pure RespVmEdited

-- | Handle cloud-init ISO generation/regeneration for a VM
handleVmCloudInit :: ServerState -> Text -> Int64 -> IO Response
handleVmCloudInit state clientName vmId = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (vm, _) ->
      if not (vmCloudInit vm)
        then pure $ RespError "Cloud-init is not enabled on this VM"
        else do
          ciResp <- runAction state clientName (RegenerateCloudInit vmId (vmName vm))
          case ciResp of
            RespError err -> pure $ RespError $ "Cloud-init ISO generation failed: " <> err
            _ -> pure RespVmEdited

-- | Validate that the VM can be addressed for serial console
-- attachment (running + headless). The agent owns the ring buffer;
-- this only does the user-facing-message validation.
handleSerialConsole :: ServerState -> Int64 -> IO Response
handleSerialConsole state vmId = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  pure $ case result of
    Nothing -> RespVmNotFound
    Just (vm, status)
      | not (isViewable status) ->
          RespError $ "VM is not running (status: " <> enumToText status <> ")"
      | not (vmHeadless vm) ->
          RespError "VM is not headless — use SPICE viewer instead"
      | otherwise -> RespSerialConsoleOk

-- | Validate the VM for serial-console flush (same predicate as
-- attach). The actual flush is dispatched through the agent.
handleSerialConsoleFlush :: ServerState -> Int64 -> IO Response
handleSerialConsoleFlush state vmId = do
  resp <- handleSerialConsole state vmId
  pure $ case resp of
    RespSerialConsoleOk -> RespSerialConsoleFlushed
    other -> other

-- | Validate that the VM is running for HMP monitor attachment.
-- Headlessness doesn't matter: HMP exists for both headless and
-- graphical VMs.
handleHmpMonitor :: ServerState -> Int64 -> IO Response
handleHmpMonitor state vmId = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  pure $ case result of
    Nothing -> RespVmNotFound
    Just (_, status)
      | not (isViewable status) ->
          RespError $ "VM is not running (status: " <> enumToText status <> ")"
      | otherwise -> RespHmpMonitorOk

-- | Validate the VM for HMP-monitor flush; actual flush dispatches
-- through the agent.
handleHmpMonitorFlush :: ServerState -> Int64 -> IO Response
handleHmpMonitorFlush state vmId = do
  resp <- handleHmpMonitor state vmId
  pure $ case resp of
    RespHmpMonitorOk -> RespHmpMonitorFlushed
    other -> other

-- | Inject Ctrl+Alt+Del into a running VM via QMP. Delivered through
-- the daemon's QMP client so it works regardless of whether the
-- caller is on the daemon host.
handleVmSendCtrlAltDel :: ServerState -> Int64 -> IO Response
handleVmSendCtrlAltDel state vmId = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (_, status)
      | not (isViewable status) -> pure RespVmNotRunning
      | otherwise -> do
          qmpResult <- qmpSendCtrlAltDel (ssQemuConfig state) vmId
          case qmpResult of
            QmpSuccess -> pure RespOk
            QmpError err -> pure $ RespError $ "QMP send-key failed: " <> err
            QmpConnectionFailed err -> pure $ RespError $ "QMP connection failed: " <> err

-- | Grant a short-lived SPICE connection for a running non-headless VM.
--
-- Generates a fresh 18-byte (24-char URL-safe base64) random password,
-- installs it via QMP @set_password@, and schedules expiry via
-- @expire_password@ so an unused grant disappears on its own. The
-- daemon never persists the password — it lives in QEMU's in-memory
-- SPICE state until it expires or is rotated by the next grant.
handleVmViewGrant :: ServerState -> Int64 -> IO Response
handleVmViewGrant state vmId = do
  let pool = ssDbPool state
      cfg = ssQemuConfig state
  mVm <- runSqlPool (get (toSqlKey vmId :: VmId)) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm
      | vmHeadless vm -> pure RespVmHeadless
      | not (isViewable (vmStatus vm)) -> pure RespVmNotRunning
      | otherwise -> case vmSpicePort vm of
          Nothing -> pure $ RespError "VM has no SPICE port assigned (daemon bug)"
          Just spicePort -> do
            pw <- generateSpicePassword
            let ttl = 120 :: Int
            outer <- withVmNodeAgent state vmId $ \nac ->
              NOA.vmSetSpiceTicket nac vmId pw (fromIntegral ttl)
            case outer of
              Left err -> pure $ RespError err
              Right r -> case r of
                Left e ->
                  pure $ RespError $ "vmSetSpiceTicket: " <> T.pack (show e)
                Right () ->
                  pure $
                    RespVmViewGrant
                      { host = qcSpiceBindAddress cfg
                      , port = spicePort
                      , password = pw
                      , ttlSeconds = ttl
                      }

-- | Read 18 bytes from @/dev/urandom@ and encode as URL-safe base64
-- (24 printable characters, no padding issues in SPICE tickets).
generateSpicePassword :: IO Text
generateSpicePassword = do
  bytes <- withBinaryFile "/dev/urandom" ReadMode $ \h -> BS.hGet h 18
  pure $ TE.decodeUtf8 $ B64URL.encode bytes

--------------------------------------------------------------------------------
-- Database Operations
--------------------------------------------------------------------------------

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

-- | Check whether the VM has a cloud-init ISO disk attached
hasCloudInitIso :: Int64 -> SqlPersistT IO Bool
hasCloudInitIso vmId = do
  let key = toSqlKey vmId :: VmId
  drives <- selectList [M.DriveVmId ==. key, M.DriveMedia ==. Just MediaCdrom] []
  -- Check if any CDROM drive's disk name ends with "-cloud-init"
  results <- mapM checkDrive drives
  pure $ or results
  where
    checkDrive (Entity _ drive) = do
      mDisk <- get (driveDiskImageId drive)
      pure $ case mDisk of
        Just disk -> "-cloud-init" `T.isSuffixOf` diskImageName disk
        Nothing -> False

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
  -- ^ rebootQuirk
  -> Maybe Int
  -> SqlPersistT IO Int64
createVm name nodeKey cpuCount ramMb description headless guestAgent cloudInit autostart rebootQuirk vsockCid = do
  now <- liftIO getCurrentTime
  let vm =
        Vm
          { vmName = name
          , vmNodeId = nodeKey
          , vmCreatedAt = now
          , vmStatus = VmStopped
          , vmCpuCount = cpuCount
          , vmRamMb = ramMb
          , vmDescription = description
          , vmHeadless = headless
          , vmGuestAgent = guestAgent
          , vmCloudInit = cloudInit
          , vmHealthcheck = Nothing
          , vmAutostart = autostart
          , vmSpicePort = Nothing
          , vmVsockCid = vsockCid
          , vmErrorMessage = Nothing
          , vmLastErrorAt = Nothing
          , vmMigrating = False
          , vmRebootQuirk = rebootQuirk
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
        map (driveDiskImageId . entityVal) $
          filter (not . driveReadOnly . entityVal) drives
  notShared <- filterM (fmap not . isSharedDisk vmId) (map fromSqlKey writableDiskKeys)
  filterM (fmap not . isUsedByTemplate) notShared
  where
    isSharedDisk :: Int64 -> Int64 -> SqlPersistT IO Bool
    isSharedDisk thisVmId diskId = do
      otherDrives <- selectList [M.DriveDiskImageId ==. toSqlKey diskId, M.DriveVmId !=. toSqlKey thisVmId] [LimitTo 1]
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
  let diskKeys = map (driveDiskImageId . entityVal) drives
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
      otherDrives <- selectList [M.DriveDiskImageId ==. toSqlKey diskId, M.DriveVmId !=. toSqlKey thisVmId] [LimitTo 1]
      pure $ not (null otherDrives)

-- | Delete a VM and all associated resources
deleteVm :: Int64 -> SqlPersistT IO ()
deleteVm vmId = do
  let key = toSqlKey vmId :: VmId
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
        , viNodeId = fromSqlKey (vmNodeId vm)
        , viNodeName = Map.findWithDefault "(deleted)" (vmNodeId vm) nameOf
        , viStatus = vmStatus vm
        , viCpuCount = vmCpuCount vm
        , viRamMb = vmRamMb vm
        , viHeadless = vmHeadless vm
        , viGuestAgent = vmGuestAgent vm
        , viCloudInit = vmCloudInit vm
        , viHealthcheck = vmHealthcheck vm
        , viAutostart = vmAutostart vm
        , viRebootQuirk = vmRebootQuirk vm
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
      drives <- selectList [M.DriveVmId ==. key] []
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
            , vdNodeId = fromSqlKey (vmNodeId vm)
            , vdNodeName = nodeName'
            , vdCreatedAt = vmCreatedAt vm
            , vdStatus = vmStatus vm
            , vdCpuCount = vmCpuCount vm
            , vdRamMb = vmRamMb vm
            , vdDescription = vmDescription vm
            , vdDrives = driveInfos
            , vdNetIfs = map toNetIfInfo netIfs
            , vdHeadless = vmHeadless vm
            , vdMonitorSocket = T.pack monitorSock
            , vdSpicePort = vmSpicePort vm
            , vdVsockCid = vmVsockCid vm
            , vdSerialSocket = T.pack serialSock
            , vdGuestAgentSocket = T.pack guestAgentSock
            , vdGuestAgent = vmGuestAgent vm
            , vdCloudInit = vmCloudInit vm
            , vdCloudInitConfig = ciInfo
            , vdHealthcheck = vmHealthcheck vm
            , vdAutostart = vmAutostart vm
            , vdErrorMessage = vmErrorMessage vm
            , vdLastErrorAt = vmLastErrorAt vm
            , vdRebootQuirk = vmRebootQuirk vm
            }
  where
    toDriveInfo vmNode vmNodeBasePath (Entity driveKey drive) = do
      let diskImageKey = driveDiskImageId drive
      mDiskImage <- get diskImageKey
      case mDiskImage of
        Nothing ->
          pure
            DriveInfo
              { diId = fromSqlKey driveKey
              , diDiskImageId = fromSqlKey diskImageKey
              , diDiskImageName = "(deleted)"
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
              , diDiskImageId = fromSqlKey diskImageKey
              , diDiskImageName = diskImageName diskImage
              , diInterface = driveInterface drive
              , diFilePath = absPath
              , diFormat = diskImageFormat diskImage
              , diMedia = driveMedia drive
              , diReadOnly = driveReadOnly drive
              , diCacheType = driveCacheType drive
              , diDiscard = driveDiscard drive
              }
    toNetIfInfo (Entity netIfKey netIf) =
      NetIfInfo
        { niId = fromSqlKey netIfKey
        , niType = networkInterfaceInterfaceType netIf
        , niHostDevice = networkInterfaceHostDevice netIf
        , niMacAddress = networkInterfaceMacAddress netIf
        , niNetworkId = fromSqlKey <$> networkInterfaceNetworkId netIf
        , niNetworkName = Nothing -- Not resolved in VM details view
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
  -- ^ rebootQuirk
  -> SqlPersistT IO ()
editVm vmId mCpus mRam mDesc mHeadless mGuestAgent mCloudInit mAutostart mRebootQuirk = do
  let key = toSqlKey vmId :: VmId
      updates =
        maybe [] (\cpus -> [M.VmCpuCount =. cpus]) mCpus
          ++ maybe [] (\ram -> [M.VmRamMb =. ram]) mRam
          ++ maybe [] (\desc -> [M.VmDescription =. Just desc]) mDesc
          ++ maybe [] (\h -> [M.VmHeadless =. h]) mHeadless
          ++ maybe [] (\ga -> [M.VmGuestAgent =. ga]) mGuestAgent
          ++ maybe [] (\ci -> [M.VmCloudInit =. ci]) mCloudInit
          ++ maybe [] (\a -> [M.VmAutostart =. a]) mAutostart
          ++ maybe [] (\rq -> [M.VmRebootQuirk =. rq]) mRebootQuirk
  case updates of
    [] -> pure ()
    us -> update key us

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

-- | Tell the agent to drop every netd-allocated TAP attached to
-- the given VM (both managed and bridge NICs). Used by the post-
-- QEMU-exit supervisor thread. Best-effort: errors are logged via
-- the agent client, not propagated to the caller — the VM is
-- gone either way.
releaseManagedTaps :: ServerState -> Int64 -> IO ()
releaseManagedTaps state vmId = do
  let vmKey = toSqlKey vmId :: VmId
  ifaces <-
    runSqlPool
      (selectList [M.NetworkInterfaceVmId ==. vmKey] [])
      (ssDbPool state)
  let netdMediated = filter (isNetdMediated . entityVal) ifaces
      isNetdMediated ni =
        M.networkInterfaceInterfaceType ni == M.NetBridge
          || isJust (M.networkInterfaceNetworkId ni)
  _ <- withVmNetAgent state vmId $ \nac ->
    mapM_
      ( \(Entity ifaceKey _) ->
          let tapName = Spec.corvusTapName (fromSqlKey ifaceKey)
           in Control.Monad.void (NA.deleteTap nac tapName)
      )
      netdMediated
  pure ()

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
  , vcrCloudInit :: Bool
  , vcrAutostart :: Bool
  , vcrRebootQuirk :: Bool
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
      (vcrCloudInit a)
      (vcrAutostart a)
      (vcrRebootQuirk a)

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
  , vedCloudInit :: Maybe Bool
  , vedAutostart :: Maybe Bool
  , vedRebootQuirk :: Maybe Bool
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
      (vedCloudInit a)
      (vedAutostart a)
      (vedRebootQuirk a)

newtype VmPause = VmPause {vpVmId :: Int64}

instance Action VmPause where
  actionSubsystem _ = SubVm
  actionCommand _ = "pause"
  actionEntityId = Just . fromIntegral . vpVmId
  actionExecute ctx a = handleVmPause (acState ctx) (vpVmId a)

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

newtype VmStop = VmStop {vstpVmId :: Int64}

instance Action VmStop where
  actionSubsystem _ = SubVm
  actionCommand _ = "stop"
  actionEntityId = Just . fromIntegral . vstpVmId
  actionValidate state a = do
    result <- handleVmStopValidate state (vstpVmId a)
    pure $ case result of
      Left errResp -> Just errResp
      Right _ -> Nothing
  actionExecute ctx a = handleVmStopExecute (acState ctx) (vstpVmId a)

-- (Phase 3 refactor: the @StartVirtiofsd@ and @LaunchQemu@
-- subtask actions are gone — virtiofsd is implicit in 'VmSpec',
-- and QEMU spawning happens inline via 'NOA.vmStart' in
-- 'launchVmViaAgent' rather than as a separate subtask.)

-- | Poll the node agent every 1 s for VM liveness via
-- 'NOA.vmStatus'. Returns when the agent reports anything other
-- than 'VmAgentRunning' (stopped / errored / unknown all count as
-- "gone"), or when the agent itself disappears.
pollVmUntilExit :: ServerState -> Int64 -> IO ExitOutcome
pollVmUntilExit state vmId = loop
  where
    loop = do
      outer <- withVmNodeAgent state vmId $ \nac -> NOA.vmStatus nac vmId
      case outer of
        Left _ -> pure ExitAgentGone
        Right r -> case r of
          Left _ -> pure ExitAgentGone
          Right status -> case NOA.vasState status of
            NOA.VmAgentRunning -> threadDelay 1000000 >> loop
            NOA.VmAgentStopped -> pure ExitClean
            NOA.VmAgentErrored ->
              pure (ExitErrored (fromIntegral (NOA.vasLastExitCode status)))
            NOA.VmAgentUnknown -> pure ExitVanished

-- | How a VM's monitor loop concluded. Drives the DB-status
-- reconciliation in 'attachVmMonitor'.
data ExitOutcome
  = ExitClean
  | ExitErrored !Int
  | ExitVanished
  | ExitAgentGone

-- | Fork a background thread that waits for VM @vmId@ to exit on
-- the agent side, then reconciles DB state.
--
-- Called by the VM-start handler right after 'NOA.vmStart' returns,
-- and by 'reattachVmMonitors' for each VM the daemon finds already
-- running when it (re)connects to the agent.
attachVmMonitor :: ServerState -> Int64 -> IO ()
attachVmMonitor state vmId = do
  _ <- forkIO $ runServerLogging state $ do
    logDebugN $ "Polling VM " <> T.pack (show vmId) <> " liveness via nodeagent"
    outcome <- liftIO $ pollVmUntilExit state vmId
    -- Skip reconciliation if a competing handler (e.g.
    -- 'handleVmReset') already committed a terminal status.
    -- Replaces the old "Vm.pid was cleared" signal.
    mStatus <- liftIO $ runSqlPool (getVmStatusOnly vmId) (ssDbPool state)
    case mStatus of
      Nothing ->
        logDebugN $ "VM " <> T.pack (show vmId) <> " was deleted; monitor exiting"
      Just VmStopped ->
        logDebugN $
          "VM "
            <> T.pack (show vmId)
            <> " already marked stopped (likely by reset); skipping status update"
      Just VmError ->
        logDebugN $
          "VM "
            <> T.pack (show vmId)
            <> " already marked error; skipping status update"
      Just _ -> case outcome of
        ExitClean -> do
          logInfoN $ "VM " <> T.pack (show vmId) <> " exited normally"
          liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
        ExitErrored code -> do
          let msg = "QEMU exited with error code " <> T.pack (show code)
          logWarnN $
            "VM "
              <> T.pack (show vmId)
              <> " "
              <> msg
          liftIO $ runSqlPool (setVmError vmId msg) (ssDbPool state)
        ExitVanished -> do
          logInfoN $
            "VM "
              <> T.pack (show vmId)
              <> " no longer in agent ledger; marking stopped"
          liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
        ExitAgentGone ->
          logDebugN $
            "VM "
              <> T.pack (show vmId)
              <> " monitor exiting: agent disconnected"
    -- Tell netd it can drop the VM's managed TAPs.
    liftIO $ releaseManagedTaps state vmId
  pure ()

-- | On daemon (re)connect to the agent: walk the DB for every VM
-- whose intent is "should be running" (status in
-- @{Starting, Running, Paused}@), ask the agent for current
-- status, and reconcile:
--
--   * 'VmAgentRunning' — agent still has the VM; re-attach the
--     monitor thread.
--   * 'VmAgentStopped' / 'VmAgentErrored' — agent observed the
--     exit while the daemon was down; reflect it in the DB.
--   * 'VmAgentUnknown' — agent has no record (e.g. it restarted
--     and reaped the orphan QEMU on startup). Re-issue 'vmStart'
--     to honour the daemon's intent. 'vmStart' is idempotent, so
--     this is also safe if the agent had the VM and we're just
--     catching up.
--
-- Paused VMs lose their pause state across an agent restart —
-- they come back as VmRunning. Documented trade-off; symmetric
-- to "agent restart = VM restart" from the parent plan.
reattachVmMonitors :: ServerState -> IO ()
reattachVmMonitors state = do
  let pool = ssDbPool state
  candidates <-
    runSqlPool
      ( selectList
          [ M.VmStatus
              <-. [VmStarting, VmRunning, VmPaused]
          ]
          []
      )
      pool
  runServerLogging state $
    Control.Monad.forM_ candidates $ \(Entity vmKey vm) -> do
      let vmId = fromSqlKey vmKey
      outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> do
        rstat <- NOA.vmStatus nac vmId
        pure (nac, rstat)
      case outer of
        Left err ->
          logDebugN $
            "Skipping reattach for VM " <> vmName vm <> ": " <> err
        Right (nac, r) -> case r of
          Right status -> case NOA.vasState status of
            NOA.VmAgentRunning -> do
              logInfoN $
                "Re-attaching monitor for VM " <> vmName vm
              liftIO $ attachVmMonitor state vmId
            NOA.VmAgentStopped -> do
              logInfoN $
                "VM "
                  <> vmName vm
                  <> " exited cleanly while daemon was disconnected; reconciling"
              liftIO $ runSqlPool (setVmStopped vmId) pool
            NOA.VmAgentErrored -> do
              let msg =
                    "QEMU exited with error code "
                      <> T.pack (show (NOA.vasLastExitCode status))
                      <> " (observed while daemon was disconnected)"
              logWarnN $
                "VM " <> vmName vm <> ": " <> msg
              liftIO $ runSqlPool (setVmError vmId msg) pool
            NOA.VmAgentUnknown -> do
              logInfoN $
                "VM "
                  <> vmName vm
                  <> " not in agent ledger; re-issuing vmStart to honour DB intent"
              reapplyVm state nac vmId vm
          Left e ->
            logWarnN $
              "vmStatus RPC failed for VM "
                <> vmName vm
                <> ": "
                <> T.pack (show e)

-- | Re-issue 'vmStart' for one VM. Assembles 'VmSpec' from the
-- DB (same path 'launchVmViaAgent' uses on a cold start),
-- dispatches, and attaches the monitor on success. On any
-- failure the row lands in 'VmError' — a follow-up @crv vm
-- start@ can recover it.
reapplyVm :: ServerState -> NOA.NodeAgentClient -> Int64 -> Vm -> LoggingT IO ()
reapplyVm state nac vmId vm = do
  let pool = ssDbPool state
      cfg = ssQemuConfig state
  mNetAgent <- liftIO $ lookupNetAgentMaybe state (M.vmNodeId vm)
  needsNetd <- liftIO $ runSqlPool (hasNetdMediatedNetIf vmId) pool
  let netAgentForSpec = if needsNetd then mNetAgent else Nothing
      waitMs =
        if vmGuestAgent vm then 300000 else 0
  mSpec <- liftIO $ NSpec.assembleVmSpec pool cfg netAgentForSpec vmId waitMs
  case mSpec of
    Left err
      | "disappeared from DB" `T.isInfixOf` err -> do
          logWarnN $
            "VM "
              <> vmName vm
              <> " disappeared from DB during reapply; marking stopped"
          liftIO $ runSqlPool (setVmStopped vmId) pool
      | otherwise -> do
          logWarnN $
            "VM " <> vmName vm <> " reapply: assembleVmSpec failed: " <> err
          liftIO $ runSqlPool (setVmError vmId err) pool
    Right spec -> do
      r <- liftIO $ NOA.vmStart nac spec
      case r of
        Right info -> do
          logInfoN $ "VM " <> vmName vm <> " re-applied via vmStart"
          let pid = fromIntegral (NOA.vriQemuPid info) :: Int
          liftIO $ runSqlPool (setVmStarted vmId VmRunning pid) pool
          liftIO $ attachVmMonitor state vmId
        Left e -> do
          let msg = "vmStart reapply: " <> T.pack (show e)
          logWarnN $
            "vmStart reapply failed for VM "
              <> vmName vm
              <> ": "
              <> T.pack (show e)
          liftIO $ runSqlPool (setVmError vmId msg) pool
