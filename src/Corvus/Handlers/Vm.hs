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
  , handleVmStartValidate
  , handleVmStartExecute
  , attachVmMonitor
  , reattachVmMonitors
  , handleVmStopValidate
  , handleVmStopExecute
  , handleVmPause
  , handleVmReset
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
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (filterM, unless, when)
import qualified Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.Handlers.CloudInit (RegenerateCloudInit (..))
import Corvus.Handlers.Disk (DiskDelete (..))
import Corvus.Handlers.Resolve (validateName)
import Corvus.Model (DriveFormat (..), VmStatus (..))
import Corvus.Model hiding (DriveFormat, VmStatus)
import qualified Corvus.Model as M
import Corvus.Model.VmState (VmAction (..), validateTransition)
import qualified Corvus.NetAgentClient as NA
import qualified Corvus.NetAgentClient.Spec as Spec
import Corvus.Node.SocketBuffer (flushBuffer, startSocketBufferThread)
import Corvus.Node.SpicePort (withAllocatedSpicePort)
import Corvus.Node.VsockCid (hostHasVhostVsock, isHostFree, withAllocatedVsockCid)
import qualified Corvus.NodeAgentClient as NOA
import qualified Corvus.NodeAgentClient.Spec as NSpec
import Corvus.Protocol
import Corvus.Qemu
import Corvus.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Word (Word32)
import Database.Persist
import Database.Persist.Postgresql (SqlBackend, runSqlPool)
import Database.Persist.Sql (SqlPersistT)
import System.IO (IOMode (ReadMode), withBinaryFile)

-- | Ring-buffer capacity for a headless VM's serial console scrollback (1 MiB).
serialBufferCapacity :: Int
serialBufferCapacity = 1048576

-- | Ring-buffer capacity for a VM's HMP monitor scrollback (64 KiB).
monitorBufferCapacity :: Int
monitorBufferCapacity = 65536

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

-- | Handle VM create command
handleVmCreate :: ServerState -> Text -> Int -> Int -> Maybe Text -> Bool -> Bool -> Bool -> Bool -> IO Response
handleVmCreate state name cpuCount ramMb description headless guestAgent cloudInit autostart =
  case validateName "VM" name of
    Left err -> pure $ RespError err
    Right () -> do
      hasVsock <- hostHasVhostVsock
      if not hasVsock
        then do
          vmId <- runSqlPool (createVm name cpuCount ramMb description headless guestAgent cloudInit autostart Nothing) (ssDbPool state)
          pure $ RespVmCreated vmId
        else do
          -- Allocate + persist the CID atomically (under ssVsockCidLock)
          -- so concurrent vm.create / vm.start handlers never pick the
          -- same CID and race at QEMU bind time.
          result <- withAllocatedVsockCid state $ \cid ->
            runSqlPool
              (createVm name cpuCount ramMb description headless guestAgent cloudInit autostart (Just cid))
              (ssDbPool state)
          case result of
            Left err -> pure $ RespError err
            Right vmId -> pure $ RespVmCreated vmId

-- | Handle VM delete command
handleVmDelete :: ActionContext -> Int64 -> Bool -> IO Response
handleVmDelete ctx vmId deleteDisks = do
  let state = acState ctx
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (_, status) ->
      if status `elem` [VmRunning, VmStarting, VmStopping, VmPaused]
        then pure RespVmRunning
        else do
          -- Collect disks to delete before removing drives
          disksToDelete <-
            if deleteDisks
              then runSqlPool (getExclusiveDisks vmId) (ssDbPool state)
              else pure []
          -- Delete VM and its associations (drives, netifs, etc.)
          runSqlPool (deleteVm vmId) (ssDbPool state)
          -- Delete exclusive disks as subtasks
          mapM_ (\diskId -> runActionAsSubtask state (DiskDelete diskId) (acTaskId ctx)) disksToDelete
          pure RespVmDeleted

-- | Validate that a VM can be started. Returns the VM and current status, or an error response.
-- Checks: VM exists, state transition valid, all referenced networks are running.
handleVmStartValidate :: ServerState -> Int64 -> IO (Either Response (Vm, VmStatus))
handleVmStartValidate state vmId = do
  mVm <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure $ Left RespVmNotFound
    Just (vm, currentStatus) ->
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
handleVmStartExecute :: ServerState -> Int64 -> TaskId -> IO Response
handleVmStartExecute state vmId parentTaskId = do
  validated <- handleVmStartValidate state vmId
  case validated of
    Left errResp -> pure errResp
    Right (vm, currentStatus) ->
      case currentStatus of
        VmPaused -> runServerLogging state $ resumeFromPaused state vmId
        _ -> runServerLogging state $ do
          resp <- startQemuAndMonitor state vmId vm parentTaskId
          case resp of
            RespVmStateChanged _ ->
              -- Steady-state healthchecks now arrive via the
              -- agent's 'subscribeVmStatus' push — see
              -- 'Corvus.Handlers.VmStatusSink'. No per-VM poller
              -- thread to spawn here.
              pure $ RespVmStateChanged VmRunning
            _ -> pure resp

-- | Resume a paused VM via @vmResume@ (agent issues QMP @cont@).
resumeFromPaused :: ServerState -> Int64 -> LoggingT IO Response
resumeFromPaused state vmId = do
  mAgent <- liftIO $ readTVarIO (ssNodeAgent state)
  case mAgent of
    Nothing -> do
      logWarnN $ "nodeagent unavailable; cannot resume VM " <> T.pack (show vmId)
      pure $ RespInvalidTransition VmPaused "nodeagent unavailable"
    Just nac -> do
      r <- liftIO $ NOA.vmResume nac vmId
      case r of
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
startQemuAndMonitor :: ServerState -> Int64 -> Vm -> TaskId -> LoggingT IO Response
startQemuAndMonitor state vmId vm parentTaskId = do
  let pool = ssDbPool state

  -- 1. Cloud-init ISO regeneration.
  when (vmCloudInit vm) $ do
    hasCloudInitDisk <- liftIO $ runSqlPool (hasCloudInitIso vmId) pool
    unless hasCloudInitDisk $ do
      logInfoN $ "Generating cloud-init ISO for VM " <> T.pack (show vmId)
      _ <- liftIO $ runActionAsSubtask state (RegenerateCloudInit vmId (vmName vm)) parentTaskId
      pure ()

  -- 2. SPICE port allocation + persist.
  spiceResult <-
    if vmHeadless vm
      then pure (Right Nothing)
      else do
        alloc <- liftIO $
          withAllocatedSpicePort state $ \port -> do
            runSqlPool (update (toSqlKey vmId :: VmId) [M.VmSpicePort =. Just port]) pool
            pure port
        case alloc of
          Left err -> pure (Left err)
          Right port -> pure (Right (Just port))

  case spiceResult of
    Left err -> do
      logWarnN $ "SPICE port allocation failed: " <> err
      liftIO $ runSqlPool (setVmError vmId) pool
      pure $ RespError $ "Failed to allocate SPICE port: " <> err
    Right _ -> do
      -- 3. CID re-validation.
      cidResult <- liftIO $ ensureFreeVsockCid state vmId vm
      case cidResult of
        Left err -> do
          logWarnN $ "Vsock CID re-allocation failed: " <> err
          liftIO $ runSqlPool (setVmError vmId) pool
          pure $ RespError $ "Failed to secure a free vsock CID: " <> err
        Right _ -> launchVmViaAgent state vmId vm pool

-- | Assemble 'VmSpec' from DB rows and call 'NOA.vmStart'.
launchVmViaAgent
  :: ServerState
  -> Int64
  -> Vm
  -> Pool SqlBackend
  -> LoggingT IO Response
launchVmViaAgent state vmId vm pool = do
  -- Managed NICs need the netd cap so 'assembleVmSpec' can
  -- pre-allocate persistent TAPs. If the VM has none, we don't
  -- care whether netd is up.
  hasManagedNic <- liftIO $ runSqlPool (hasManagedNetworkInterface vmId) pool
  mNetAgent <- liftIO $ readTVarIO (ssNetAgent state)
  when (hasManagedNic && isNothing mNetAgent) $
    logWarnN $
      "VM "
        <> T.pack (show vmId)
        <> " needs managed NIC but netd is unavailable"
  let netAgentForSpec = if hasManagedNic then mNetAgent else Nothing
  -- Wait-for-first-ping timeout. Reuse the healthcheck interval
  -- as a coarse boot deadline; fall back to 60 s when the
  -- interval is zero (healthcheck disabled).
  let cfg = ssQemuConfig state
      waitMs =
        if vmGuestAgent vm
          then
            let interval = qcHealthcheckInterval cfg
             in if interval > 0
                  then fromIntegral (interval * 1000) :: Word32
                  else 60000
          else 0

  mSpec <- liftIO $ NSpec.assembleVmSpec pool cfg netAgentForSpec vmId waitMs
  case mSpec of
    Nothing -> do
      logWarnN $ "VM " <> T.pack (show vmId) <> " disappeared from DB during start"
      pure RespVmNotFound
    Just spec -> do
      mAgent <- liftIO $ readTVarIO (ssNodeAgent state)
      case mAgent of
        Nothing -> do
          logWarnN "nodeagent unavailable; cannot start VM"
          liftIO $ runSqlPool (setVmError vmId) pool
          pure $ RespError "nodeagent unavailable"
        Just nac -> do
          r <- liftIO $ NOA.vmStart nac spec
          case r of
            Left e -> do
              logWarnN $ "vmStart failed for VM " <> T.pack (show vmId) <> ": " <> T.pack (show e)
              liftIO $ runSqlPool (setVmError vmId) pool
              pure $ RespInvalidTransition VmError $ "vmStart: " <> T.pack (show e)
            Right info -> do
              let pid = fromIntegral (NOA.vriQemuPid info) :: Int
              -- With vmStart blocking for first ping when guestAgent
              -- is set, by the time we get here the VM really is
              -- VmRunning. Skip the legacy VmStarting transition.
              liftIO $ runSqlPool (setVmStarted vmId VmRunning pid) (ssDbPool state)
              liftIO $ attachVmMonitor state vmId
              -- Wire up chardev ring buffers.
              let logLevel = ssLogLevel state
              when (vmHeadless vm) $ liftIO $ do
                serialSock <- getSerialSocket cfg vmId
                startSocketBufferThread cfg vmId serialSock (ssSerialBuffers state) serialBufferCapacity "serial" logLevel
              liftIO $ do
                monitorSock <- getMonitorSocket cfg vmId
                startSocketBufferThread cfg vmId monitorSock (ssMonitorBuffers state) monitorBufferCapacity "monitor" logLevel
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
  hasVsock <- hostHasVhostVsock
  if not hasVsock
    then -- Host has no vhost_vsock kernel module; the VM's QEMU args
    -- skip the vhost-vsock-pci device entirely. Returning a bogus
    -- CID is fine because the caller only inspects Left vs Right.
      pure (Right 0)
    else case vmVsockCid vm of
      Nothing -> reallocate pool
      Just cid -> do
        free <- isHostFree cid
        if free
          then pure (Right cid)
          else reallocate pool
  where
    reallocate pool =
      withAllocatedVsockCid state $ \newCid -> do
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
      mAgent <- liftIO $ readTVarIO (ssNodeAgent state)
      case mAgent of
        Nothing -> do
          logWarnN "nodeagent unavailable; cannot stop VM"
          liftIO $ runSqlPool (update (toSqlKey vmId :: VmId) [M.VmStatus =. currentStatus]) (ssDbPool state)
          pure $ RespInvalidTransition currentStatus "nodeagent unavailable"
        Just nac -> stopViaAgent nac currentStatus
  where
    stopViaAgent nac currentStatus = do
      r <- liftIO $ NOA.vmStopGraceful nac vmId 300
      case r of
        Left e -> do
          logWarnN $ "vmStopGraceful RPC failed: " <> T.pack (show e)
          pure $ RespInvalidTransition currentStatus ("vmStopGraceful: " <> T.pack (show e))
        Right res -> case NOA.vsrKind res of
          NOA.VmStopStopped -> do
            liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
            pure $ RespVmStateChanged VmStopped
          NOA.VmStopAlreadyStopped -> do
            liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
            pure $ RespVmStateChanged VmStopped
          NOA.VmStopTimeout -> do
            logWarnN $
              "VM "
                <> T.pack (show vmId)
                <> " did not exit within graceful window; force-stopping"
            rh <- liftIO $ NOA.vmStopHard nac vmId
            case rh of
              Right _ -> do
                liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
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
          mAgent <- liftIO $ readTVarIO (ssNodeAgent state)
          case mAgent of
            Nothing -> do
              logWarnN $ "nodeagent unavailable; cannot pause VM " <> T.pack (show vmId)
              pure $ RespInvalidTransition currentStatus "nodeagent unavailable"
            Just nac -> do
              r <- liftIO $ NOA.vmPause nac vmId
              case r of
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

      mAgent <- liftIO $ readTVarIO (ssNodeAgent state)
      case mAgent of
        Nothing ->
          logWarnN $ "nodeagent unavailable; reset only updates DB state for VM " <> T.pack (show vmId)
        Just nac -> do
          r <- liftIO $ NOA.vmStopHard nac vmId
          case r of
            Left e ->
              logWarnN $ "vmStopHard failed for VM " <> T.pack (show vmId) <> ": " <> T.pack (show e)
            Right res -> case NOA.vsrKind res of
              NOA.VmStopStopped ->
                logInfoN $ "VM " <> T.pack (show vmId) <> " process killed"
              NOA.VmStopAlreadyStopped ->
                logDebugN $ "VM " <> T.pack (show vmId) <> " was not in the agent's ledger"
              _ ->
                logWarnN $ "vmStopHard returned: " <> NOA.vsrMessage res

      pure $ RespVmStateChanged VmStopped

-- | Handle VM edit command
-- Only allowed when VM is stopped. Updates only the provided fields.
handleVmEdit :: ServerState -> Int64 -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO Response
handleVmEdit state vmId mCpus mRam mDesc mHeadless mGuestAgent mCloudInit mAutostart = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (_, status) ->
      let hasNonAutostartEdits = or [isJust mCpus, isJust mRam, isJust mDesc, isJust mHeadless, isJust mGuestAgent, isJust mCloudInit]
       in if hasNonAutostartEdits && status /= VmStopped
            then pure RespVmMustBeStopped
            else do
              runSqlPool (editVm vmId mCpus mRam mDesc mHeadless mGuestAgent mCloudInit mAutostart) (ssDbPool state)
              pure RespVmEdited

-- | Handle cloud-init ISO generation/regeneration for a VM
handleVmCloudInit :: ServerState -> Int64 -> IO Response
handleVmCloudInit state vmId = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (vm, _) ->
      if not (vmCloudInit vm)
        then pure $ RespError "Cloud-init is not enabled on this VM"
        else do
          ciResp <- runAction state (RegenerateCloudInit vmId (vmName vm))
          case ciResp of
            RespError err -> pure $ RespError $ "Cloud-init ISO generation failed: " <> err
            _ -> pure RespVmEdited

-- | Handle serial console attach request.
-- Validates that the VM is running and headless, and that a buffer handle exists.
handleSerialConsole :: ServerState -> Int64 -> IO Response
handleSerialConsole state vmId = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (vm, status)
      | not (isViewable status) ->
          pure $ RespError $ "VM is not running (status: " <> enumToText status <> ")"
      | not (vmHeadless vm) ->
          pure $ RespError "VM is not headless — use SPICE viewer instead"
      | otherwise -> do
          buffers <- readTVarIO (ssSerialBuffers state)
          case Map.lookup vmId buffers of
            Nothing -> pure $ RespError "Serial console buffer not available"
            Just _ -> pure RespSerialConsoleOk

-- | Handle serial console buffer flush request.
handleSerialConsoleFlush :: ServerState -> Int64 -> IO Response
handleSerialConsoleFlush state vmId = do
  buffers <- readTVarIO (ssSerialBuffers state)
  case Map.lookup vmId buffers of
    Nothing -> pure $ RespError "Serial console buffer not available"
    Just handle -> do
      flushBuffer (sbhBuffer handle)
      pure RespSerialConsoleFlushed

-- | Handle HMP monitor attach request.
-- Validates that the VM is running and that a monitor buffer is registered.
-- Headlessness doesn't matter: HMP exists for both headless and graphical VMs.
handleHmpMonitor :: ServerState -> Int64 -> IO Response
handleHmpMonitor state vmId = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (_, status)
      | not (isViewable status) ->
          pure $ RespError $ "VM is not running (status: " <> enumToText status <> ")"
      | otherwise -> do
          buffers <- readTVarIO (ssMonitorBuffers state)
          case Map.lookup vmId buffers of
            Nothing -> pure $ RespError "HMP monitor buffer not available"
            Just _ -> pure RespHmpMonitorOk

-- | Handle HMP monitor buffer flush request.
handleHmpMonitorFlush :: ServerState -> Int64 -> IO Response
handleHmpMonitorFlush state vmId = do
  buffers <- readTVarIO (ssMonitorBuffers state)
  case Map.lookup vmId buffers of
    Nothing -> pure $ RespError "HMP monitor buffer not available"
    Just handle -> do
      flushBuffer (sbhBuffer handle)
      pure RespHmpMonitorFlushed

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
            mAgent <- readTVarIO (ssNodeAgent state)
            case mAgent of
              Nothing -> pure $ RespError "nodeagent unavailable"
              Just nac -> do
                r <- NOA.vmSetSpiceTicket nac vmId pw (fromIntegral ttl)
                case r of
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
-- The @_pid@ parameter is kept for caller-side symmetry but is no
-- longer persisted — the agent owns every PID. Drop the parameter
-- on the next breaking change.
setVmStarted :: Int64 -> VmStatus -> Int -> SqlPersistT IO ()
setVmStarted vmId status _pid = do
  let key = toSqlKey vmId :: VmId
  update key [M.VmStatus =. status]

-- | Set VM status to stopped and clear healthcheck, SPICE port,
-- and guest network data.
setVmStopped :: Int64 -> SqlPersistT IO ()
setVmStopped vmId = do
  let key = toSqlKey vmId :: VmId
  update key [M.VmStatus =. VmStopped, M.VmHealthcheck =. Nothing, M.VmSpicePort =. Nothing]
  updateWhere
    [M.NetworkInterfaceVmId ==. key]
    [M.NetworkInterfaceGuestIpAddresses =. Nothing]

-- | Set VM status to error and clear healthcheck, SPICE port,
-- and guest network data.
setVmError :: Int64 -> SqlPersistT IO ()
setVmError vmId = do
  let key = toSqlKey vmId :: VmId
  update key [M.VmStatus =. VmError, M.VmHealthcheck =. Nothing, M.VmSpicePort =. Nothing]
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

-- | Set VM status (without changing PID)
setVmStatus :: Int64 -> VmStatus -> SqlPersistT IO ()
setVmStatus vmId status = do
  let key = toSqlKey vmId :: VmId
  update key [M.VmStatus =. status]

-- | Create a new VM
createVm :: Text -> Int -> Int -> Maybe Text -> Bool -> Bool -> Bool -> Bool -> Maybe Int -> SqlPersistT IO Int64
createVm name cpuCount ramMb description headless guestAgent cloudInit autostart vsockCid = do
  now <- liftIO getCurrentTime
  let vm =
        Vm
          { vmName = name
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
          }
  key <- insert vm
  pure $ fromSqlKey key

-- | Get disk IDs attached to this VM that are eligible for cleanup
-- under @vm.delete --delete-disks@.
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
  pure $ map toVmInfo vms
  where
    toVmInfo (Entity key vm) =
      VmInfo
        { viId = fromSqlKey key
        , viName = vmName vm
        , viStatus = vmStatus vm
        , viCpuCount = vmCpuCount vm
        , viRamMb = vmRamMb vm
        , viHeadless = vmHeadless vm
        , viGuestAgent = vmGuestAgent vm
        , viCloudInit = vmCloudInit vm
        , viHealthcheck = vmHealthcheck vm
        , viAutostart = vmAutostart vm
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
      -- Get socket paths
      monitorSock <- liftIO $ getMonitorSocket config vmId
      serialSock <- liftIO $ getSerialSocket config vmId
      guestAgentSock <- liftIO $ getGuestAgentSocket config vmId
      -- Build drive info by fetching disk images
      driveInfos <- mapM toDriveInfo drives
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
            }
  where
    toDriveInfo (Entity driveKey drive) = do
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
        Just diskImage ->
          pure
            DriveInfo
              { diId = fromSqlKey driveKey
              , diDiskImageId = fromSqlKey diskImageKey
              , diDiskImageName = diskImageName diskImage
              , diInterface = driveInterface drive
              , diFilePath = diskImageFilePath diskImage
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
        }

-- | Edit VM properties. Only updates fields that are Just.
editVm :: Int64 -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> SqlPersistT IO ()
editVm vmId mCpus mRam mDesc mHeadless mGuestAgent mCloudInit mAutostart = do
  let key = toSqlKey vmId :: VmId
      updates =
        maybe [] (\cpus -> [M.VmCpuCount =. cpus]) mCpus
          ++ maybe [] (\ram -> [M.VmRamMb =. ram]) mRam
          ++ maybe [] (\desc -> [M.VmDescription =. Just desc]) mDesc
          ++ maybe [] (\h -> [M.VmHeadless =. h]) mHeadless
          ++ maybe [] (\ga -> [M.VmGuestAgent =. ga]) mGuestAgent
          ++ maybe [] (\ci -> [M.VmCloudInit =. ci]) mCloudInit
          ++ maybe [] (\a -> [M.VmAutostart =. a]) mAutostart
  case updates of
    [] -> pure ()
    us -> update key us

-- | Check if a VM has any managed (NetManaged) network interfaces with a networkId.
hasManagedNetworkInterface :: Int64 -> SqlPersistT IO Bool
hasManagedNetworkInterface vmId = do
  let vmKey = toSqlKey vmId :: VmId
  cnt <- count [M.NetworkInterfaceVmId ==. vmKey, M.NetworkInterfaceNetworkId !=. Nothing]
  pure $ cnt > 0

-- | Tell the agent to drop every managed TAP attached to the
-- given VM. Used by the post-QEMU-exit supervisor thread.
-- Best-effort: errors are logged via the agent client, not
-- propagated to the caller — the VM is gone either way.
releaseManagedTaps :: ServerState -> Int64 -> IO ()
releaseManagedTaps state vmId = do
  mAgent <- readTVarIO (ssNetAgent state)
  case mAgent of
    Nothing -> pure ()
    Just nac -> do
      let vmKey = toSqlKey vmId :: VmId
      ifaces <-
        runSqlPool
          ( selectList
              [ M.NetworkInterfaceVmId ==. vmKey
              , M.NetworkInterfaceNetworkId !=. Nothing
              ]
              []
          )
          (ssDbPool state)
      mapM_
        ( \(Entity ifaceKey _) ->
            let tapName = Spec.corvusTapName (fromSqlKey ifaceKey)
             in Control.Monad.void (NA.deleteTap nac tapName)
        )
        ifaces

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
  , vcrCpuCount :: Int
  , vcrRamMb :: Int
  , vcrDescription :: Maybe Text
  , vcrHeadless :: Bool
  , vcrGuestAgent :: Bool
  , vcrCloudInit :: Bool
  , vcrAutostart :: Bool
  }

instance Action VmCreate where
  actionSubsystem _ = SubVm
  actionCommand _ = "create"
  actionEntityName = Just . vcrName
  actionExecute ctx a =
    handleVmCreate
      (acState ctx)
      (vcrName a)
      (vcrCpuCount a)
      (vcrRamMb a)
      (vcrDescription a)
      (vcrHeadless a)
      (vcrGuestAgent a)
      (vcrCloudInit a)
      (vcrAutostart a)

data VmDelete = VmDelete
  { vdelVmId :: Int64
  , vdelDeleteDisks :: Bool
  }

instance Action VmDelete where
  actionSubsystem _ = SubVm
  actionCommand _ = "delete"
  actionEntityId = Just . fromIntegral . vdelVmId
  actionExecute ctx a = handleVmDelete ctx (vdelVmId a) (vdelDeleteDisks a)

data VmEdit = VmEdit
  { vedVmId :: Int64
  , vedCpus :: Maybe Int
  , vedRam :: Maybe Int
  , vedDesc :: Maybe Text
  , vedHeadless :: Maybe Bool
  , vedGuestAgent :: Maybe Bool
  , vedCloudInit :: Maybe Bool
  , vedAutostart :: Maybe Bool
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
  actionExecute ctx a = handleVmStartExecute (acState ctx) (vsVmId a) (acTaskId ctx)

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
      mAgent <- readTVarIO (ssNodeAgent state)
      case mAgent of
        Nothing -> pure ExitAgentGone
        Just nac -> do
          r <- NOA.vmStatus nac vmId
          case r of
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
          logWarnN $
            "VM "
              <> T.pack (show vmId)
              <> " exited with error code "
              <> T.pack (show code)
          liftIO $ runSqlPool (setVmError vmId) (ssDbPool state)
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
    -- Close persistent guest agent connection (QEMU is gone)
    liftIO $ closeGuestAgentConn (ssGuestAgentConns state) vmId
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
  mAgent <- readTVarIO (ssNodeAgent state)
  case mAgent of
    Nothing -> pure ()
    Just nac -> do
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
          r <- liftIO $ NOA.vmStatus nac vmId
          case r of
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
                logWarnN $
                  "VM "
                    <> vmName vm
                    <> " exited with error while daemon was disconnected (code "
                    <> T.pack (show (NOA.vasLastExitCode status))
                    <> ")"
                liftIO $ runSqlPool (setVmError vmId) pool
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
  mNetAgent <- liftIO $ readTVarIO (ssNetAgent state)
  hasManagedNic <- liftIO $ runSqlPool (hasManagedNetworkInterface vmId) pool
  let netAgentForSpec = if hasManagedNic then mNetAgent else Nothing
      waitMs =
        if vmGuestAgent vm
          then
            let interval = qcHealthcheckInterval cfg
             in if interval > 0
                  then fromIntegral (interval * 1000) :: Word32
                  else 60000
          else 0
  mSpec <- liftIO $ NSpec.assembleVmSpec pool cfg netAgentForSpec vmId waitMs
  case mSpec of
    Nothing -> do
      logWarnN $
        "VM "
          <> vmName vm
          <> " disappeared from DB during reapply; marking stopped"
      liftIO $ runSqlPool (setVmStopped vmId) pool
    Just spec -> do
      r <- liftIO $ NOA.vmStart nac spec
      case r of
        Right info -> do
          logInfoN $ "VM " <> vmName vm <> " re-applied via vmStart"
          let pid = fromIntegral (NOA.vriQemuPid info) :: Int
          liftIO $ runSqlPool (setVmStarted vmId VmRunning pid) pool
          liftIO $ attachVmMonitor state vmId
        Left e -> do
          logWarnN $
            "vmStart reapply failed for VM "
              <> vmName vm
              <> ": "
              <> T.pack (show e)
          liftIO $ runSqlPool (setVmError vmId) pool
