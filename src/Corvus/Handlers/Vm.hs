{-# LANGUAGE OverloadedStrings #-}

-- | VM management handlers.
-- This module contains handlers for VM lifecycle operations:
-- list, show, start, stop, pause, reset.
module Corvus.Handlers.Vm
  ( -- * Handlers
    handleVmList
  , handleVmShow
  , handleVmCreate
  , handleVmDelete
  , handleVmStart
  , handleVmStop
  , handleVmPause
  , handleVmReset
  , handleVmEdit
  , handleVmCloudInit

    -- * State machine
  , VmAction (..)
  , validateTransition
  )
where

import Control.Concurrent (forkIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugN, logInfoN, logWarnN)
import Corvus.Handlers.GuestAgentPoller (startGuestAgentPoller)
import Corvus.Handlers.SshKey (regenerateCloudInitIso)
import Corvus.Model (DriveFormat (..), VmStatus (..))
import Corvus.Model hiding (DriveFormat, VmStatus)
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu
import Corvus.Types
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
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
  result <- runSqlPool (getVmDetails (ssQemuConfig state) vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just details -> pure $ RespVmDetails details

-- | Handle VM create command
handleVmCreate :: ServerState -> Text -> Int -> Int -> Maybe Text -> Bool -> Bool -> Bool -> IO Response
handleVmCreate state name cpuCount ramMb description headless guestAgent cloudInit = do
  vmId <- runSqlPool (createVm name cpuCount ramMb description headless guestAgent cloudInit) (ssDbPool state)
  pure $ RespVmCreated vmId

-- | Handle VM delete command
handleVmDelete :: ServerState -> Int64 -> IO Response
handleVmDelete state vmId = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (_, status) ->
      if status == VmRunning || status == VmPaused
        then pure RespVmRunning
        else do
          runSqlPool (deleteVm vmId) (ssDbPool state)
          pure RespVmDeleted

-- | Handle VM start command
-- If stopped: set to running, start QEMU in background thread, monitor for exit
-- If paused: send QMP continue, set to running
handleVmStart :: ServerState -> Int64 -> IO Response
handleVmStart state vmId = runServerLogging state $ do
  mVm <- liftIO $ runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just (vm, currentStatus) ->
      case validateTransition currentStatus ActionStart of
        Left errMsg -> pure $ RespInvalidTransition currentStatus errMsg
        Right _ ->
          case currentStatus of
            VmStopped -> do
              -- Verify all referenced networks are running
              networkCheck <- liftIO $ runSqlPool (checkNetworksRunning vmId) (ssDbPool state)
              case networkCheck of
                Just networkName -> do
                  logWarnN $ "VM " <> T.pack (show vmId) <> " references stopped network: " <> networkName
                  pure $ RespInvalidTransition VmStopped $ "Network '" <> networkName <> "' is not running"
                Nothing -> do
                  -- Generate cloud-init ISO if enabled and not yet attached
                  when (vmCloudInit vm) $ do
                    hasCloudInitDisk <- liftIO $ runSqlPool (hasCloudInitIso vmId) (ssDbPool state)
                    unless hasCloudInitDisk $ do
                      logInfoN $ "Generating cloud-init ISO for VM " <> T.pack (show vmId)
                      _ <- liftIO $ regenerateCloudInitIso (ssQemuConfig state) (ssDbPool state) vmId (vmName vm) (ssLogLevel state)
                      pure ()

                  -- Start virtiofsd processes for shared directories
                  virtiofsdResult <- startVirtiofsdProcesses (ssDbPool state) (ssQemuConfig state) vmId
                  case virtiofsdResult of
                    VirtiofsdSomeFailed -> do
                      logWarnN $ "Some virtiofsd processes failed to start for VM " <> T.pack (show vmId)
                    _ -> pure ()

                  -- Start the VM using QEMU
                  result <- startVm (ssDbPool state) (ssQemuConfig state) vmId
                  case result of
                    VmStarted pid ph -> do
                      -- Set status to running and save PID first
                      liftIO $ runSqlPool (setVmRunning vmId pid) (ssDbPool state)
                      -- Start guest agent poller if guest agent is enabled and interval > 0
                      -- (interval=0 disables the poller, useful for tests to avoid QGA socket contention)
                      when (vmGuestAgent vm && qcHealthcheckInterval (ssQemuConfig state) > 0) $
                        liftIO $
                          startGuestAgentPoller (ssDbPool state) (ssQemuConfig state) (qcHealthcheckInterval $ ssQemuConfig state) vmId (ssLogLevel state)
                      -- Fork a thread to wait for process exit
                      _ <- liftIO $ forkIO $ runServerLogging state $ do
                        logDebugN $ "Waiting for VM " <> T.pack (show vmId) <> " process to exit"
                        exitCode <- liftIO $ waitForProcess ph
                        -- Check if the VM was intentionally stopped/reset.
                        -- If PID was cleared, another handler already set the final status.
                        mCurrentPid <- liftIO $ runSqlPool (getVmPid vmId) (ssDbPool state)
                        case mCurrentPid of
                          Nothing ->
                            -- PID was cleared by handleVmReset — it already set the status
                            logDebugN $ "VM " <> T.pack (show vmId) <> " was reset externally, skipping status update"
                          Just _ ->
                            -- Normal exit — we're responsible for setting the final status
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
              qmpResult <- liftIO $ qmpContinue (ssQemuConfig state) vmId
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
-- Uses guest-shutdown via QGA if the guest agent is enabled, otherwise
-- falls back to ACPI system_powerdown via QMP.
-- Status will change when the QEMU process exits (background thread).
handleVmStop :: ServerState -> Int64 -> IO Response
handleVmStop state vmId = runServerLogging state $ do
  mVm <- liftIO $ runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just (vm, currentStatus) ->
      case validateTransition currentStatus ActionStop of
        Left errMsg -> pure $ RespInvalidTransition currentStatus errMsg
        Right _ ->
          if vmGuestAgent vm
            then do
              -- Use guest agent for clean shutdown (like running "poweroff" inside the guest)
              logDebugN $ "Sending guest-shutdown to VM " <> T.pack (show vmId)
              ok <- liftIO $ guestShutdown (ssQemuConfig state) vmId
              if ok
                then do
                  logInfoN $ "VM " <> T.pack (show vmId) <> " guest-shutdown initiated"
                  pure $ RespVmStateChanged VmRunning
                else do
                  logWarnN $ "Guest-shutdown failed for VM " <> T.pack (show vmId) <> ", falling back to QMP"
                  shutdownViaQmp vmId currentStatus
            else shutdownViaQmp vmId currentStatus
  where
    shutdownViaQmp vmId' currentStatus' = do
      logDebugN $ "Sending QMP shutdown command to VM " <> T.pack (show vmId')
      qmpResult <- liftIO $ qmpShutdown (ssQemuConfig state) vmId'
      case qmpResult of
        QmpSuccess -> do
          logInfoN $ "VM " <> T.pack (show vmId') <> " shutdown initiated"
          pure $ RespVmStateChanged VmRunning
        QmpError err -> do
          logWarnN $ "QMP error stopping VM " <> T.pack (show vmId') <> ": " <> err
          pure $ RespInvalidTransition currentStatus' $ "QMP error: " <> err
        QmpConnectionFailed err -> do
          logWarnN $ "QMP connection failed for VM " <> T.pack (show vmId') <> ": " <> err
          pure $ RespInvalidTransition currentStatus' $ "QMP connection failed: " <> err

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
          -- Pause VM via QMP
          logDebugN $ "Sending pause command to VM " <> T.pack (show vmId)
          qmpResult <- liftIO $ qmpStop (ssQemuConfig state) vmId
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
-- Kill QEMU and virtiofsd processes, wait for exit, set status to stopped.
handleVmReset :: ServerState -> Int64 -> IO Response
handleVmReset state vmId = runServerLogging state $ do
  mVm <- liftIO $ runSqlPool (getVmWithPid vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just (_, mPid) -> do
      -- Clear PID first so the background waitForProcess thread knows
      -- not to update the status when it sees the non-zero exit code.
      liftIO $ runSqlPool (clearVmPid vmId) (ssDbPool state)

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

      -- Update status
      liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
      pure $ RespVmStateChanged VmStopped

-- | Handle VM edit command
-- Only allowed when VM is stopped. Updates only the provided fields.
handleVmEdit :: ServerState -> Int64 -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO Response
handleVmEdit state vmId mCpus mRam mDesc mHeadless mGuestAgent mCloudInit = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (_, status) ->
      if status /= VmStopped
        then pure RespVmMustBeStopped
        else do
          runSqlPool (editVm vmId mCpus mRam mDesc mHeadless mGuestAgent mCloudInit) (ssDbPool state)
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
          ciResult <- regenerateCloudInitIso (ssQemuConfig state) (ssDbPool state) vmId (vmName vm) (ssLogLevel state)
          case ciResult of
            Left err -> pure $ RespError $ "Cloud-init ISO generation failed: " <> err
            Right _ -> pure RespVmEdited

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

-- | Get the current PID stored for a VM (Nothing if cleared or VM not found)
getVmPid :: Int64 -> SqlPersistT IO (Maybe Int)
getVmPid vmId = do
  let key = toSqlKey vmId :: VmId
  mVm <- get key
  pure $ mVm >>= vmPid

-- | Clear the PID field without changing status.
-- Used by handleVmReset to signal the background thread to skip status updates.
clearVmPid :: Int64 -> SqlPersistT IO ()
clearVmPid vmId = do
  let key = toSqlKey vmId :: VmId
  update key [M.VmPid =. Nothing]

-- | Set VM status to stopped and clear PID, healthcheck, and guest network data
setVmStopped :: Int64 -> SqlPersistT IO ()
setVmStopped vmId = do
  let key = toSqlKey vmId :: VmId
  update key [M.VmStatus =. VmStopped, M.VmPid =. Nothing, M.VmHealthcheck =. Nothing]
  updateWhere
    [M.NetworkInterfaceVmId ==. key]
    [M.NetworkInterfaceGuestIpAddresses =. Nothing]

-- | Set VM status to error and clear healthcheck and guest network data
setVmError :: Int64 -> SqlPersistT IO ()
setVmError vmId = do
  let key = toSqlKey vmId :: VmId
  update key [M.VmStatus =. VmError, M.VmHealthcheck =. Nothing]
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
createVm :: Text -> Int -> Int -> Maybe Text -> Bool -> Bool -> Bool -> SqlPersistT IO Int64
createVm name cpuCount ramMb description headless guestAgent cloudInit = do
  now <- liftIO getCurrentTime
  let vm =
        Vm
          { vmName = name
          , vmCreatedAt = now
          , vmStatus = VmStopped
          , vmCpuCount = cpuCount
          , vmRamMb = ramMb
          , vmDescription = description
          , vmPid = Nothing
          , vmHeadless = headless
          , vmGuestAgent = guestAgent
          , vmCloudInit = cloudInit
          , vmHealthcheck = Nothing
          }
  key <- insert vm
  pure $ fromSqlKey key

-- | Delete a VM and all associated resources
deleteVm :: Int64 -> SqlPersistT IO ()
deleteVm vmId = do
  let key = toSqlKey vmId :: VmId
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
        }

-- | Get full VM details
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
      spiceSock <- liftIO $ getSpiceSocket config vmId
      serialSock <- liftIO $ getSerialSocket config vmId
      guestAgentSock <- liftIO $ getGuestAgentSocket config vmId
      -- Build drive info by fetching disk images
      driveInfos <- mapM toDriveInfo drives
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
            , vdSpiceSocket = T.pack spiceSock
            , vdSerialSocket = T.pack serialSock
            , vdGuestAgentSocket = T.pack guestAgentSock
            , vdGuestAgent = vmGuestAgent vm
            , vdCloudInit = vmCloudInit vm
            , vdHealthcheck = vmHealthcheck vm
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
editVm :: Int64 -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> SqlPersistT IO ()
editVm vmId mCpus mRam mDesc mHeadless mGuestAgent mCloudInit = do
  let key = toSqlKey vmId :: VmId
      updates =
        maybe [] (\cpus -> [M.VmCpuCount =. cpus]) mCpus
          ++ maybe [] (\ram -> [M.VmRamMb =. ram]) mRam
          ++ maybe [] (\desc -> [M.VmDescription =. Just desc]) mDesc
          ++ maybe [] (\h -> [M.VmHeadless =. h]) mHeadless
          ++ maybe [] (\ga -> [M.VmGuestAgent =. ga]) mGuestAgent
          ++ maybe [] (\ci -> [M.VmCloudInit =. ci]) mCloudInit
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
          case networkVdeSwitchPid network of
            Just _ -> go rest
            Nothing -> pure $ Just $ networkName network
