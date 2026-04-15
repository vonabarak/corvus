{-# LANGUAGE OverloadedStrings #-}

-- | Attach and detach disk images to running or stopped VMs.
--
-- For VMs with a live QEMU process (running or paused), attach and
-- detach hot-plug through QMP: 'qmpBlockdevAdd' + 'qmpDeviceAddDrive'
-- for attach, 'qmpDeviceDel' + 'qmpBlockdevDel' for detach. Device
-- removal is asynchronous in QEMU, so blockdev removal is retried a
-- few times to avoid racing with @device_del@ completion.
--
-- For stopped VMs, only the 'Drive' row is touched.
module Corvus.Handlers.Disk.Attach
  ( -- * Action types
    DiskAttach (..)
  , DiskDetachByDisk (..)

    -- * Handlers
  , handleDiskAttach
  , handleDiskDetach
  )
where

import Corvus.Action
import Corvus.Handlers.Disk.Db (getOverlayIds)
import Corvus.Handlers.Disk.Path (resolveDiskPath)

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Model
import Corvus.Protocol
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Qemu.Qmp
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)

-- | Attach a disk to a VM.
handleDiskAttach
  :: ServerState
  -> Int64
  -> Int64
  -> DriveInterface
  -> Maybe DriveMedia
  -> Bool
  -> Bool
  -> CacheType
  -> IO Response
handleDiskAttach state vmId diskId interface media readOnly discard cache = runServerLogging state $ do
  logInfoN $
    "Attaching disk "
      <> T.pack (show diskId)
      <> " to VM "
      <> T.pack (show vmId)
      <> if readOnly then " (read-only)" else ""

  -- Check disk exists
  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      -- Check VM exists and get status
      mVm <- liftIO $ runSqlPool (get (toSqlKey vmId :: VmId)) (ssDbPool state)
      case mVm of
        Nothing -> pure RespVmNotFound
        Just vm -> do
          -- Check if disk is used as backing image for overlays
          -- We only allow attaching base images as read-only.
          overlayIds <- liftIO $ runSqlPool (getOverlayIds diskId) (ssDbPool state)
          if not (null overlayIds) && not readOnly
            then pure $ RespDiskHasOverlays overlayIds
            else do
              -- Create drive record (unique constraint: one drive per VM+disk pair)
              mDriveId <-
                liftIO $
                  runSqlPool
                    ( insertUnique
                        Drive
                          { driveVmId = toSqlKey vmId
                          , driveDiskImageId = toSqlKey diskId
                          , driveInterface = interface
                          , driveMedia = media
                          , driveReadOnly = readOnly
                          , driveCacheType = cache
                          , driveDiscard = discard
                          }
                    )
                    (ssDbPool state)
              case mDriveId of
                Nothing -> pure $ RespError "Disk is already attached to this VM"
                Just driveId -> do
                  -- If VM is running or paused, hot-plug via QMP (both have live QEMU process)
                  if vmStatus vm `elem` [VmRunning, VmPaused]
                    then do
                      logInfoN $ "VM is " <> enumToText (vmStatus vm) <> ", performing hot-plug"
                      let nodeName = "drive-" <> T.pack (show $ fromSqlKey driveId)
                          deviceId = "device-" <> T.pack (show $ fromSqlKey driveId)
                          format = diskImageFormat disk
                      filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk

                      -- Add block device
                      let qemuCfg = ssQemuConfig state
                      blockResult <- liftIO $ qmpBlockdevAdd qemuCfg vmId nodeName filePath format readOnly
                      case blockResult of
                        QmpSuccess -> do
                          -- Add device
                          deviceResult <- liftIO $ qmpDeviceAddDrive qemuCfg vmId deviceId nodeName interface
                          case deviceResult of
                            QmpSuccess -> do
                              logInfoN "Hot-plug successful"
                              pure $ RespDiskAttached $ fromSqlKey driveId
                            QmpError err -> do
                              logWarnN $ "Device add failed: " <> err
                              -- Try to clean up blockdev
                              _ <- liftIO $ qmpBlockdevDel qemuCfg vmId nodeName
                              -- Remove drive record
                              liftIO $ runSqlPool (delete driveId) (ssDbPool state)
                              pure $ RespError $ "Device add failed: " <> err
                            QmpConnectionFailed err -> do
                              liftIO $ runSqlPool (delete driveId) (ssDbPool state)
                              pure $ RespError $ "QMP connection failed: " <> err
                        QmpError err -> do
                          logWarnN $ "Blockdev add failed: " <> err
                          liftIO $ runSqlPool (delete driveId) (ssDbPool state)
                          pure $ RespError $ "Blockdev add failed: " <> err
                        QmpConnectionFailed err -> do
                          liftIO $ runSqlPool (delete driveId) (ssDbPool state)
                          pure $ RespError $ "QMP connection failed: " <> err
                    else do
                      logInfoN "VM is not active, disk attached to database only"
                      pure $ RespDiskAttached $ fromSqlKey driveId

-- | Detach a disk from a VM by disk image ID (looks up drive via UniqueDrive constraint).
handleDiskDetach :: ServerState -> Int64 -> Int64 -> IO Response
handleDiskDetach state vmId diskId = runServerLogging state $ do
  logInfoN $
    "Detaching disk image "
      <> T.pack (show diskId)
      <> " from VM "
      <> T.pack (show vmId)

  -- Look up drive by (vmId, diskImageId) unique constraint
  mDrive <- liftIO $ runSqlPool (getBy (UniqueDrive (toSqlKey vmId) (toSqlKey diskId))) (ssDbPool state)
  case mDrive of
    Nothing -> pure RespDriveNotFound
    Just (Entity driveKey drive) -> do
      let driveId = fromSqlKey driveKey
      -- Check VM status
      mVm <- liftIO $ runSqlPool (get (driveVmId drive)) (ssDbPool state)
      case mVm of
        Nothing -> pure RespVmNotFound
        Just vm -> do
          -- If VM is running or paused, hot-unplug via QMP (both have live QEMU process)
          if vmStatus vm `elem` [VmRunning, VmPaused]
            then do
              logInfoN $ "VM is " <> enumToText (vmStatus vm) <> ", performing hot-unplug"
              let deviceId = "device-" <> T.pack (show driveId)
                  nodeName = "drive-" <> T.pack (show driveId)

              -- Remove device first (device_del is async — QEMU returns
              -- immediately but the device is removed in the background)
              let qemuCfg = ssQemuConfig state
              deviceResult <- liftIO $ qmpDeviceDel qemuCfg vmId deviceId
              case deviceResult of
                QmpSuccess -> do
                  -- Remove block device, retrying if device_del hasn't completed yet
                  blockResult <- liftIO $ retryBlockdevDel qemuCfg vmId nodeName
                  case blockResult of
                    QmpSuccess -> do
                      liftIO $ runSqlPool (delete driveKey) (ssDbPool state)
                      logInfoN "Hot-unplug successful"
                      pure RespDiskOk
                    QmpError err -> do
                      logWarnN $ "Blockdev remove failed (device already removed): " <> err
                      -- Still remove from DB since device is gone
                      liftIO $ runSqlPool (delete driveKey) (ssDbPool state)
                      pure RespDiskOk
                    QmpConnectionFailed err ->
                      pure $ RespError $ "QMP connection failed: " <> err
                QmpError err -> do
                  logWarnN $ "Device remove failed: " <> err
                  pure $ RespError $ "Device remove failed: " <> err
                QmpConnectionFailed err ->
                  pure $ RespError $ "QMP connection failed: " <> err
            else do
              -- Just remove from database
              liftIO $ runSqlPool (delete driveKey) (ssDbPool state)
              logInfoN "Drive detached from database"
              pure RespDiskOk

-- | Retry @blockdev-del@ up to 5 times with 200ms delay between attempts.
--
-- QEMU's @device_del@ is asynchronous — the device may still be in use
-- when we try to remove its block backend immediately after @device_del@
-- returns. Each retry sleeps 200ms, then polls; errors other than
-- \"in use\" are returned immediately without retrying.
retryBlockdevDel :: QemuConfig -> Int64 -> T.Text -> IO QmpResult
retryBlockdevDel config vmId nodeName = go (5 :: Int)
  where
    go 0 = qmpBlockdevDel config vmId nodeName
    go n = do
      result <- qmpBlockdevDel config vmId nodeName
      case result of
        QmpSuccess -> pure QmpSuccess
        QmpError err
          | "in use" `T.isInfixOf` err -> do
              threadDelay 200000
              go (n - 1)
          | otherwise -> pure result
        other -> pure other

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

data DiskAttach = DiskAttach
  { datVmId :: Int64
  , datDiskId :: Int64
  , datInterface :: DriveInterface
  , datMedia :: Maybe DriveMedia
  , datReadOnly :: Bool
  , datDiscard :: Bool
  , datCache :: CacheType
  }

instance Action DiskAttach where
  actionSubsystem _ = SubDisk
  actionCommand _ = "attach"
  actionEntityId = Just . fromIntegral . datVmId
  actionExecute ctx a = handleDiskAttach (acState ctx) (datVmId a) (datDiskId a) (datInterface a) (datMedia a) (datReadOnly a) (datDiscard a) (datCache a)

data DiskDetachByDisk = DiskDetachByDisk
  { ddbVmId :: Int64
  , ddbDiskId :: Int64
  }

instance Action DiskDetachByDisk where
  actionSubsystem _ = SubDisk
  actionCommand _ = "detach"
  actionEntityId = Just . fromIntegral . ddbVmId
  actionExecute ctx a = handleDiskDetach (acState ctx) (ddbVmId a) (ddbDiskId a)
