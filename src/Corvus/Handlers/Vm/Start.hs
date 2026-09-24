{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.Vm.Start
  ( VmStart (..)
  , autostartVmsOnNode
  , autostartClientName
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Disk (DiskDelete (..))
import Corvus.Handlers.Disk.Db (diskImageNodeFilePathFor)
import Corvus.Handlers.Resolve (ResolveError (..), resolveErrorMessage, resolveNode, validateName)
import Corvus.Handlers.Scheduler (pickNodeForVm)
import Corvus.Handlers.Vm.CloudInit (ensureCloudInitIso)
import Corvus.Handlers.Vm.Console (generateSpicePassword)
import Corvus.Handlers.Vm.Db
import Corvus.Handlers.Vm.Monitor (attachVmMonitor, releaseManagedTaps)
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
  netAgentForSpec <- resolveStartNetAgent
  mSpec <- assembleStartSpec netAgentForSpec
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
    -- Managed and bridge NICs need netd while assembling the spec. A VM
    -- without either kind of NIC can still start while netd is unavailable.
    resolveStartNetAgent = do
      needsNetd <- liftIO $ runSqlPool (hasNetdMediatedNetIf vmId) pool
      mNetAgent <- liftIO $ lookupNetAgentMaybe state (M.vmNodeId vm)
      when (needsNetd && isNothing mNetAgent) $
        logWarnN $
          "VM "
            <> T.pack (show vmId)
            <> " has a managed or bridge NIC but netd is unavailable"
      pure $ if needsNetd then mNetAgent else Nothing

    -- The nodeagent wait budget covers cold boot through the first QGA
    -- response. Cloud-init's first installation of qemu-guest-agent gets a
    -- longer budget; steady-state and pre-installed-agent boots use 90 s.
    assembleStartSpec netAgentForSpec = do
      let firstBoot = isNothing (vmHealthcheck vm)
          cloudInitBootstrap = firstBoot && vmCloudInit vm
          waitMs
            | not (vmGuestAgent vm) = 0
            | cloudInitBootstrap = 300000
            | otherwise = 90000
      liftIO $
        NSpec.assembleVmSpec
          pool
          (ssQemuConfig state)
          netAgentForSpec
          vmId
          (vmLifecycleRevision vm)
          (fromMaybe (vmLifecycleRevision vm) (vmRuntimeGeneration vm))
          waitMs

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

-- | Per-node autostart pass. Called by the per-node supervisor's nodeagent
-- @onConnect@ callback the first time it lands a successful dial after the
-- supervisor spawned. This lets nodes reconnecting after a daemon restart
-- start their autostart VMs without requiring an explicit node-state
-- transition.
--
-- The node supervisor owns the once-per-supervisor fencing: it claims the
-- autostart slot before invoking this function, and retries only if this pass
-- fails. Keeping the fence there avoids coupling this operation to a
-- particular connection attempt while ensuring that transient agent failures
-- remain retryable.
--
-- This lives in the start handler because it issues normal 'VmStart' Actions,
-- preserving task recording and the ordinary launch path.
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
