{-# LANGUAGE OverloadedStrings #-}

-- | VM management handlers.
-- This module contains handlers for VM lifecycle operations:
-- list, show, start, stop, pause, reset.
module Corvus.Handlers.Vm
  ( -- * Handlers
    handleVmList,
    handleVmShow,
    handleVmStart,
    handleVmStop,
    handleVmPause,
    handleVmReset,

    -- * State machine
    VmAction (..),
    validateTransition,
  )
where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugN, logInfoN, logWarnN, runStdoutLoggingT)
import Corvus.Model (VmStatus (..))
import Corvus.Model hiding (VmStatus)
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu
import Corvus.Types
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlPersistT)
import System.Exit (ExitCode (..))
import System.Process (waitForProcess)

--------------------------------------------------------------------------------
-- State Machine: Valid VM state transitions for client commands
--------------------------------------------------------------------------------

-- | VM action triggered by client
data VmAction = ActionStart | ActionStop | ActionPause | ActionReset
  deriving (Eq, Show)

-- | Check if a state transition is valid for client commands.
-- Returns Right newStatus if valid, Left errorMessage if invalid.
--
-- Transition rules:
--   * Reset: always allowed, sets to Stopped
--   * From Stopped: can only Start
--   * From Running: can Stop or Pause
--   * From Paused: can only Start (resume)
--   * From Error: only Reset is allowed
validateTransition :: VmStatus -> VmAction -> Either Text VmStatus
validateTransition currentStatus action = case (currentStatus, action) of
  -- Reset is always allowed, sets to Stopped
  (_, ActionReset) -> Right VmStopped
  -- From Stopped: can only Start
  (VmStopped, ActionStart) -> Right VmRunning
  (VmStopped, ActionStop) -> Left "VM is already stopped"
  (VmStopped, ActionPause) -> Left "Cannot pause a stopped VM"
  -- From Running: can Stop or Pause
  (VmRunning, ActionStart) -> Left "VM is already running"
  (VmRunning, ActionStop) -> Right VmStopped
  (VmRunning, ActionPause) -> Right VmPaused
  -- From Paused: can only Start (resume)
  (VmPaused, ActionStart) -> Right VmRunning
  (VmPaused, ActionStop) -> Left "Cannot stop a paused VM, reset instead"
  (VmPaused, ActionPause) -> Left "VM is already paused"
  -- From Error: only Reset is allowed (handled above)
  (VmError, ActionStart) -> Left "Cannot start VM in error state, reset first"
  (VmError, ActionStop) -> Left "Cannot stop VM in error state, reset first"
  (VmError, ActionPause) -> Left "Cannot pause VM in error state, reset first"

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
  result <- runSqlPool (getVmDetails vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just details -> pure $ RespVmDetails details

-- | Handle VM start command
-- If stopped: set to running, start QEMU in background thread, monitor for exit
-- If paused: send QMP continue, set to running
handleVmStart :: ServerState -> Int64 -> IO Response
handleVmStart state vmId = runStdoutLoggingT $ do
  mVm <- liftIO $ runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just (_, currentStatus) ->
      case validateTransition currentStatus ActionStart of
        Left errMsg -> pure $ RespInvalidTransition currentStatus errMsg
        Right _ ->
          case currentStatus of
            VmStopped -> do
              -- Start virtiofsd processes for shared directories
              virtiofsdResult <- startVirtiofsdProcesses (ssDbPool state) defaultQemuConfig vmId
              case virtiofsdResult of
                VirtiofsdSomeFailed -> do
                  logWarnN $ "Some virtiofsd processes failed to start for VM " <> T.pack (show vmId)
                _ -> pure ()

              -- Start the VM using QEMU
              result <- startVm (ssDbPool state) defaultQemuConfig vmId
              case result of
                VmStarted pid ph -> do
                  logInfoN $ "VM " <> T.pack (show vmId) <> " started with PID " <> T.pack (show pid)
                  -- Set status to running and save PID first
                  liftIO $ runSqlPool (setVmRunning vmId pid) (ssDbPool state)
                  -- Fork a thread to wait for process exit
                  _ <- liftIO $ forkIO $ runStdoutLoggingT $ do
                    logDebugN $ "Waiting for VM " <> T.pack (show vmId) <> " process to exit"
                    exitCode <- liftIO $ waitForProcess ph
                    case exitCode of
                      ExitSuccess -> do
                        logInfoN $ "VM " <> T.pack (show vmId) <> " exited normally"
                        liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
                      ExitFailure code -> do
                        logWarnN $ "VM " <> T.pack (show vmId) <> " exited with error code " <> T.pack (show code)
                        liftIO $ runSqlPool (setVmError vmId) (ssDbPool state)
                  pure $ RespVmStateChanged VmRunning
                VmNotFound -> pure RespVmNotFound
                VmStartError err -> do
                  logWarnN $ "Failed to start VM " <> T.pack (show vmId) <> ": " <> err
                  liftIO $ runSqlPool (setVmError vmId) (ssDbPool state)
                  pure $ RespInvalidTransition VmError $ "Failed to start: " <> err
            VmPaused -> do
              -- Resume using QMP
              qmpResult <- liftIO $ qmpContinue vmId
              case qmpResult of
                QmpSuccess -> do
                  logInfoN $ "VM " <> T.pack (show vmId) <> " resumed"
                  liftIO $ runSqlPool (setVmStatus vmId VmRunning) (ssDbPool state)
                  pure $ RespVmStateChanged VmRunning
                QmpError err -> do
                  logWarnN $ "QMP error resuming VM " <> T.pack (show vmId) <> ": " <> err
                  pure $ RespInvalidTransition currentStatus $ "QMP error: " <> err
                QmpConnectionFailed err -> do
                  logWarnN $ "QMP connection failed for VM " <> T.pack (show vmId) <> ": " <> err
                  pure $ RespInvalidTransition currentStatus $ "QMP connection failed: " <> err
            _ -> pure $ RespInvalidTransition currentStatus "Unexpected state"

-- | Handle VM stop command
-- Send graceful shutdown via QMP (status will change when process exits)
handleVmStop :: ServerState -> Int64 -> IO Response
handleVmStop state vmId = runStdoutLoggingT $ do
  mVm <- liftIO $ runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just (_, currentStatus) ->
      case validateTransition currentStatus ActionStop of
        Left errMsg -> pure $ RespInvalidTransition currentStatus errMsg
        Right _ -> do
          -- Send graceful shutdown via QMP
          logDebugN $ "Sending shutdown command to VM " <> T.pack (show vmId)
          qmpResult <- liftIO $ qmpShutdown vmId
          case qmpResult of
            QmpSuccess -> do
              logInfoN $ "VM " <> T.pack (show vmId) <> " shutdown initiated"
              -- Don't change status here - background thread will set it when process exits
              pure $ RespVmStateChanged VmRunning
            QmpError err -> do
              logWarnN $ "QMP error stopping VM " <> T.pack (show vmId) <> ": " <> err
              pure $ RespInvalidTransition currentStatus $ "QMP error: " <> err
            QmpConnectionFailed err -> do
              logWarnN $ "QMP connection failed for VM " <> T.pack (show vmId) <> ": " <> err
              pure $ RespInvalidTransition currentStatus $ "QMP connection failed: " <> err

-- | Handle VM pause command
-- Send QMP stop command
handleVmPause :: ServerState -> Int64 -> IO Response
handleVmPause state vmId = runStdoutLoggingT $ do
  mVm <- liftIO $ runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just (_, currentStatus) ->
      case validateTransition currentStatus ActionPause of
        Left errMsg -> pure $ RespInvalidTransition currentStatus errMsg
        Right _ -> do
          -- Pause VM via QMP
          logDebugN $ "Sending pause command to VM " <> T.pack (show vmId)
          qmpResult <- liftIO $ qmpStop vmId
          case qmpResult of
            QmpSuccess -> do
              logInfoN $ "VM " <> T.pack (show vmId) <> " paused"
              liftIO $ runSqlPool (setVmStatus vmId VmPaused) (ssDbPool state)
              pure $ RespVmStateChanged VmPaused
            QmpError err -> do
              logWarnN $ "QMP error pausing VM " <> T.pack (show vmId) <> ": " <> err
              pure $ RespInvalidTransition currentStatus $ "QMP error: " <> err
            QmpConnectionFailed err -> do
              logWarnN $ "QMP connection failed for VM " <> T.pack (show vmId) <> ": " <> err
              pure $ RespInvalidTransition currentStatus $ "QMP connection failed: " <> err

-- | Handle VM reset command
-- Kill QEMU and virtiofsd processes, set status to stopped
handleVmReset :: ServerState -> Int64 -> IO Response
handleVmReset state vmId = runStdoutLoggingT $ do
  mVm <- liftIO $ runSqlPool (getVmWithPid vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just (_, mPid) -> do
      -- Kill QEMU process if we have a PID
      case mPid of
        Just pid -> do
          killResult <- killVmProcess vmId pid
          case killResult of
            KillSuccess -> logInfoN $ "VM " <> T.pack (show vmId) <> " process killed"
            KillNotRunning -> logDebugN $ "VM " <> T.pack (show vmId) <> " process not running"
            KillError err -> logWarnN $ "Error killing VM " <> T.pack (show vmId) <> ": " <> err
        Nothing ->
          logDebugN $ "VM " <> T.pack (show vmId) <> " has no PID stored"

      -- Kill all virtiofsd processes for this VM
      killVirtiofsdProcesses (ssDbPool state) vmId

      -- Update status regardless
      liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
      pure $ RespVmStateChanged VmStopped

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

-- | Get VM with its PID
getVmWithPid :: Int64 -> SqlPersistT IO (Maybe (Vm, Maybe Int))
getVmWithPid vmId = do
  let key = toSqlKey vmId :: VmId
  mVm <- get key
  pure $ case mVm of
    Nothing -> Nothing
    Just vm -> Just (vm, vmPid vm)

-- | Set VM status to running and save PID
setVmRunning :: Int64 -> Int -> SqlPersistT IO ()
setVmRunning vmId pid = do
  let key = toSqlKey vmId :: VmId
  update key [M.VmStatus =. VmRunning, M.VmPid =. Just pid]

-- | Set VM status to stopped and clear PID
setVmStopped :: Int64 -> SqlPersistT IO ()
setVmStopped vmId = do
  let key = toSqlKey vmId :: VmId
  update key [M.VmStatus =. VmStopped, M.VmPid =. Nothing]

-- | Set VM status to error
setVmError :: Int64 -> SqlPersistT IO ()
setVmError vmId = do
  let key = toSqlKey vmId :: VmId
  update key [M.VmStatus =. VmError]

-- | Set VM status (without changing PID)
setVmStatus :: Int64 -> VmStatus -> SqlPersistT IO ()
setVmStatus vmId status = do
  let key = toSqlKey vmId :: VmId
  update key [M.VmStatus =. status]

-- | List all VMs
listVms :: SqlPersistT IO [VmInfo]
listVms = do
  vms <- selectList [] [Asc M.VmName]
  pure $ map toVmInfo vms
  where
    toVmInfo (Entity key vm) =
      VmInfo
        { viId = fromSqlKey key,
          viName = vmName vm,
          viStatus = vmStatus vm,
          viCpuCount = vmCpuCount vm,
          viRamMb = vmRamMb vm
        }

-- | Get full VM details
getVmDetails :: Int64 -> SqlPersistT IO (Maybe VmDetails)
getVmDetails vmId = do
  let key = toSqlKey vmId :: VmId
  mVm <- get key
  case mVm of
    Nothing -> pure Nothing
    Just vm -> do
      drives <- selectList [M.DriveVmId ==. key] []
      netIfs <- selectList [M.NetworkInterfaceVmId ==. key] []
      -- Get socket paths
      monitorSock <- liftIO $ getMonitorSocket vmId
      spiceSock <- liftIO $ getSpiceSocket vmId
      pure $
        Just
          VmDetails
            { vdId = vmId,
              vdName = vmName vm,
              vdCreatedAt = vmCreatedAt vm,
              vdStatus = vmStatus vm,
              vdCpuCount = vmCpuCount vm,
              vdRamMb = vmRamMb vm,
              vdDescription = vmDescription vm,
              vdDrives = map toDriveInfo drives,
              vdNetIfs = map toNetIfInfo netIfs,
              vdMonitorSocket = T.pack monitorSock,
              vdSpiceSocket = T.pack spiceSock
            }
  where
    toDriveInfo (Entity key drive) =
      DriveInfo
        { diId = fromSqlKey key,
          diInterface = driveInterface drive,
          diFilePath = driveFilePath drive,
          diFormat = driveFormat drive,
          diMedia = driveMedia drive,
          diReadOnly = driveReadOnly drive,
          diCacheType = driveCacheType drive,
          diDiscard = driveDiscard drive
        }
    toNetIfInfo (Entity key netIf) =
      NetIfInfo
        { niId = fromSqlKey key,
          niType = networkInterfaceInterfaceType netIf,
          niHostDevice = networkInterfaceHostDevice netIf,
          niMacAddress = networkInterfaceMacAddress netIf
        }
