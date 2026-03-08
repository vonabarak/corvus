{-# LANGUAGE OverloadedStrings #-}

-- | Disk image management handlers.
-- Handles disk image CRUD operations, snapshots, and attach/detach.
module Corvus.Handlers.Disk
  ( -- * Disk image handlers
    handleDiskCreate,
    handleDiskDelete,
    handleDiskResize,
    handleDiskList,
    handleDiskShow,

    -- * Snapshot handlers
    handleSnapshotCreate,
    handleSnapshotDelete,
    handleSnapshotRollback,
    handleSnapshotMerge,
    handleSnapshotList,

    -- * Attach/detach handlers
    handleDiskAttach,
    handleDiskDetach,
  )
where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugN, logInfoN, logWarnN, runStdoutLoggingT)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu.Config (defaultQemuConfig, getEffectiveBasePath)
import Corvus.Qemu.Image
import Corvus.Qemu.Qmp
import Corvus.Types
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlPersistT)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Disk Image Handlers
--------------------------------------------------------------------------------

-- | Create a new disk image
handleDiskCreate :: ServerState -> Text -> DriveFormat -> Int64 -> IO Response
handleDiskCreate state name format sizeMb = runStdoutLoggingT $ do
  logInfoN $ "Creating disk image: " <> name <> " (" <> T.pack (show sizeMb) <> " MB)"

  -- Generate file path
  basePath <- liftIO $ getEffectiveBasePath defaultQemuConfig
  let fileName = T.unpack name <> "." <> T.unpack (enumToText format)
      filePath = basePath </> fileName

  -- Create the actual image file
  result <- liftIO $ createImage filePath format sizeMb
  case result of
    ImageError err -> do
      logWarnN $ "Failed to create image: " <> err
      pure $ RespError err
    ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
    ImageNotFound -> pure $ RespError "Unexpected error during creation"
    ImageSuccess -> do
      -- Store in database
      now <- liftIO getCurrentTime
      diskId <-
        liftIO $
          runSqlPool
            ( insert
                DiskImage
                  { diskImageName = name,
                    diskImageFilePath = T.pack filePath,
                    diskImageFormat = format,
                    diskImageSizeMb = Just (fromIntegral sizeMb),
                    diskImageCreatedAt = now
                  }
            )
            (ssDbPool state)
      logInfoN $ "Created disk image with ID: " <> T.pack (show $ fromSqlKey diskId)
      pure $ RespDiskCreated $ fromSqlKey diskId

-- | Delete a disk image
handleDiskDelete :: ServerState -> Int64 -> IO Response
handleDiskDelete state diskId = runStdoutLoggingT $ do
  logInfoN $ "Deleting disk image: " <> T.pack (show diskId)

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      -- Check if disk is attached to any VMs
      attachedVms <- liftIO $ runSqlPool (getAttachedVms diskId) (ssDbPool state)
      if not (null attachedVms)
        then pure $ RespDiskInUse attachedVms
        else do
          -- Delete the file
          let filePath = T.unpack $ diskImageFilePath disk
          result <- liftIO $ deleteImage filePath
          case result of
            ImageError err -> do
              logWarnN $ "Failed to delete image file: " <> err
              pure $ RespError err
            ImageNotFound -> do
              logWarnN "Image file not found, removing from database anyway"
              liftIO $ runSqlPool (delete (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
              pure RespDiskOk
            _ -> do
              -- Delete from database
              liftIO $ runSqlPool (deleteDiskAndSnapshots diskId) (ssDbPool state)
              logInfoN $ "Deleted disk image: " <> T.pack (show diskId)
              pure RespDiskOk

-- | Resize a disk image (VM must be stopped)
handleDiskResize :: ServerState -> Int64 -> Int64 -> IO Response
handleDiskResize state diskId newSizeMb = runStdoutLoggingT $ do
  logInfoN $ "Resizing disk image " <> T.pack (show diskId) <> " to " <> T.pack (show newSizeMb) <> " MB"

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      -- Check if any attached VM is running
      runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
      if not (null runningVms)
        then pure RespVmMustBeStopped
        else do
          let filePath = T.unpack $ diskImageFilePath disk
          result <- liftIO $ resizeImage filePath newSizeMb
          case result of
            ImageSuccess -> do
              -- Update size in database
              liftIO $
                runSqlPool
                  (update (toSqlKey diskId :: DiskImageId) [M.DiskImageSizeMb =. Just (fromIntegral newSizeMb)])
                  (ssDbPool state)
              logInfoN "Disk resized successfully"
              pure RespDiskOk
            ImageError err -> do
              logWarnN $ "Failed to resize: " <> err
              pure $ RespError err
            ImageNotFound -> pure RespDiskNotFound
            ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg

-- | List all disk images
handleDiskList :: ServerState -> IO Response
handleDiskList state = do
  disks <- runSqlPool listDiskImages (ssDbPool state)
  pure $ RespDiskList disks

-- | Show disk image details
handleDiskShow :: ServerState -> Int64 -> IO Response
handleDiskShow state diskId = do
  mInfo <- runSqlPool (getDiskImageInfo diskId) (ssDbPool state)
  case mInfo of
    Nothing -> pure RespDiskNotFound
    Just info -> pure $ RespDiskInfo info

--------------------------------------------------------------------------------
-- Snapshot Handlers
--------------------------------------------------------------------------------

-- | Create a snapshot
handleSnapshotCreate :: ServerState -> Int64 -> Text -> IO Response
handleSnapshotCreate state diskId snapshotName = runStdoutLoggingT $ do
  logInfoN $ "Creating snapshot '" <> snapshotName <> "' for disk " <> T.pack (show diskId)

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      -- Check format
      when (diskImageFormat disk /= FormatQcow2) $
        logWarnN "Snapshot requested on non-qcow2 image"

      let filePath = T.unpack $ diskImageFilePath disk
      result <- liftIO $ createSnapshot filePath snapshotName
      case result of
        ImageSuccess -> do
          now <- liftIO getCurrentTime
          snapshotId <-
            liftIO $
              runSqlPool
                ( insert
                    Snapshot
                      { snapshotDiskImageId = toSqlKey diskId,
                        snapshotName = snapshotName,
                        snapshotCreatedAt = now,
                        snapshotSizeMb = Nothing
                      }
                )
                (ssDbPool state)
          logInfoN $ "Created snapshot with ID: " <> T.pack (show $ fromSqlKey snapshotId)
          pure $ RespSnapshotCreated $ fromSqlKey snapshotId
        ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
        ImageError err -> pure $ RespError err
        ImageNotFound -> pure RespDiskNotFound

-- | Delete a snapshot
handleSnapshotDelete :: ServerState -> Int64 -> Int64 -> IO Response
handleSnapshotDelete state diskId snapshotId = runStdoutLoggingT $ do
  logInfoN $ "Deleting snapshot " <> T.pack (show snapshotId) <> " from disk " <> T.pack (show diskId)

  mSnapshot <- liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
  case mSnapshot of
    Nothing -> pure RespSnapshotNotFound
    Just snapshot -> do
      -- Verify disk exists
      mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
      case mDisk of
        Nothing -> pure RespDiskNotFound
        Just disk -> do
          let filePath = T.unpack $ diskImageFilePath disk
          result <- liftIO $ deleteSnapshot filePath (snapshotName snapshot)
          case result of
            ImageSuccess -> do
              liftIO $ runSqlPool (delete (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
              logInfoN "Snapshot deleted"
              pure RespSnapshotOk
            ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
            ImageError err -> pure $ RespError err
            ImageNotFound -> pure RespDiskNotFound

-- | Rollback to a snapshot (VM must be stopped)
handleSnapshotRollback :: ServerState -> Int64 -> Int64 -> IO Response
handleSnapshotRollback state diskId snapshotId = runStdoutLoggingT $ do
  logInfoN $ "Rolling back disk " <> T.pack (show diskId) <> " to snapshot " <> T.pack (show snapshotId)

  -- Check if any attached VM is running
  runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
  if not (null runningVms)
    then pure RespVmMustBeStopped
    else do
      mSnapshot <- liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
      case mSnapshot of
        Nothing -> pure RespSnapshotNotFound
        Just snapshot -> do
          mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
          case mDisk of
            Nothing -> pure RespDiskNotFound
            Just disk -> do
              let filePath = T.unpack $ diskImageFilePath disk
              result <- liftIO $ rollbackSnapshot filePath (snapshotName snapshot)
              case result of
                ImageSuccess -> do
                  logInfoN "Rollback complete"
                  pure RespSnapshotOk
                ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                ImageError err -> pure $ RespError err
                ImageNotFound -> pure RespDiskNotFound

-- | Merge a snapshot (VM must be stopped)
handleSnapshotMerge :: ServerState -> Int64 -> Int64 -> IO Response
handleSnapshotMerge state diskId snapshotId = runStdoutLoggingT $ do
  logInfoN $ "Merging snapshot " <> T.pack (show snapshotId) <> " for disk " <> T.pack (show diskId)

  -- Check if any attached VM is running
  runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
  if not (null runningVms)
    then pure RespVmMustBeStopped
    else do
      mSnapshot <- liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
      case mSnapshot of
        Nothing -> pure RespSnapshotNotFound
        Just snapshot -> do
          mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
          case mDisk of
            Nothing -> pure RespDiskNotFound
            Just disk -> do
              let filePath = T.unpack $ diskImageFilePath disk
              result <- liftIO $ mergeSnapshot filePath (snapshotName snapshot)
              case result of
                ImageSuccess -> do
                  -- Remove snapshot from database after merge
                  liftIO $ runSqlPool (delete (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
                  logInfoN "Merge complete"
                  pure RespSnapshotOk
                ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                ImageError err -> pure $ RespError err
                ImageNotFound -> pure RespDiskNotFound

-- | List snapshots for a disk image
handleSnapshotList :: ServerState -> Int64 -> IO Response
handleSnapshotList state diskId = do
  mDisk <- runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just _ -> do
      snapshots <- runSqlPool (getSnapshots diskId) (ssDbPool state)
      pure $ RespSnapshotList snapshots

--------------------------------------------------------------------------------
-- Attach/Detach Handlers
--------------------------------------------------------------------------------

-- | Attach a disk to a VM
handleDiskAttach ::
  ServerState ->
  Int64 ->
  Int64 ->
  DriveInterface ->
  Maybe DriveMedia ->
  IO Response
handleDiskAttach state vmId diskId interface media = runStdoutLoggingT $ do
  logInfoN $
    "Attaching disk "
      <> T.pack (show diskId)
      <> " to VM "
      <> T.pack (show vmId)

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
          -- Create drive record
          driveId <-
            liftIO $
              runSqlPool
                ( insert
                    Drive
                      { driveVmId = toSqlKey vmId,
                        driveDiskImageId = toSqlKey diskId,
                        driveInterface = interface,
                        driveMedia = media,
                        driveReadOnly = False,
                        driveCacheType = CacheWriteback,
                        driveDiscard = True
                      }
                )
                (ssDbPool state)

          -- If VM is running, hot-plug via QMP
          if vmStatus vm == VmRunning
            then do
              logInfoN "VM is running, performing hot-plug"
              let nodeName = "drive-" <> T.pack (show $ fromSqlKey driveId)
                  deviceId = "device-" <> T.pack (show $ fromSqlKey driveId)
                  filePath = T.unpack $ diskImageFilePath disk
                  format = diskImageFormat disk

              -- Add block device
              blockResult <- liftIO $ qmpBlockdevAdd vmId nodeName filePath format
              case blockResult of
                QmpSuccess -> do
                  -- Add device
                  deviceResult <- liftIO $ qmpDeviceAddDrive vmId deviceId nodeName interface
                  case deviceResult of
                    QmpSuccess -> do
                      logInfoN "Hot-plug successful"
                      pure $ RespDiskAttached $ fromSqlKey driveId
                    QmpError err -> do
                      logWarnN $ "Device add failed: " <> err
                      -- Try to clean up blockdev
                      _ <- liftIO $ qmpBlockdevDel vmId nodeName
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
              logInfoN "VM is stopped, disk attached to database only"
              pure $ RespDiskAttached $ fromSqlKey driveId

-- | Detach a disk from a VM
handleDiskDetach :: ServerState -> Int64 -> Int64 -> IO Response
handleDiskDetach state vmId driveId = runStdoutLoggingT $ do
  logInfoN $
    "Detaching drive "
      <> T.pack (show driveId)
      <> " from VM "
      <> T.pack (show vmId)

  -- Check drive exists
  mDrive <- liftIO $ runSqlPool (get (toSqlKey driveId :: DriveId)) (ssDbPool state)
  case mDrive of
    Nothing -> pure RespDriveNotFound
    Just drive -> do
      -- Verify VM ID matches
      if driveVmId drive /= toSqlKey vmId
        then pure $ RespError "Drive is not attached to this VM"
        else do
          -- Check VM status
          mVm <- liftIO $ runSqlPool (get (toSqlKey vmId :: VmId)) (ssDbPool state)
          case mVm of
            Nothing -> pure RespVmNotFound
            Just vm -> do
              -- If VM is running, hot-unplug via QMP
              if vmStatus vm == VmRunning
                then do
                  logInfoN "VM is running, performing hot-unplug"
                  let deviceId = "device-" <> T.pack (show driveId)
                      nodeName = "drive-" <> T.pack (show driveId)

                  -- Remove device first
                  deviceResult <- liftIO $ qmpDeviceDel vmId deviceId
                  case deviceResult of
                    QmpSuccess -> do
                      -- Remove block device
                      blockResult <- liftIO $ qmpBlockdevDel vmId nodeName
                      case blockResult of
                        QmpSuccess -> do
                          liftIO $ runSqlPool (delete (toSqlKey driveId :: DriveId)) (ssDbPool state)
                          logInfoN "Hot-unplug successful"
                          pure RespDiskOk
                        QmpError err -> do
                          logWarnN $ "Blockdev remove failed (device already removed): " <> err
                          -- Still remove from DB since device is gone
                          liftIO $ runSqlPool (delete (toSqlKey driveId :: DriveId)) (ssDbPool state)
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
                  liftIO $ runSqlPool (delete (toSqlKey driveId :: DriveId)) (ssDbPool state)
                  logInfoN "Drive detached from database"
                  pure RespDiskOk

--------------------------------------------------------------------------------
-- Database Operations
--------------------------------------------------------------------------------

-- | Get VMs that have this disk attached
getAttachedVms :: Int64 -> SqlPersistT IO [Int64]
getAttachedVms diskId = do
  drives <- selectList [M.DriveDiskImageId ==. toSqlKey diskId] []
  pure $ map (fromSqlKey . driveVmId . entityVal) drives

-- | Get running VMs that have this disk attached
getRunningAttachedVms :: Int64 -> SqlPersistT IO [Int64]
getRunningAttachedVms diskId = do
  drives <- selectList [M.DriveDiskImageId ==. toSqlKey diskId] []
  let vmKeys = map (driveVmId . entityVal) drives
  runningVms <- selectList [M.VmId <-. vmKeys, M.VmStatus ==. VmRunning] []
  pure $ map (fromSqlKey . entityKey) runningVms

-- | Delete disk and its snapshots
deleteDiskAndSnapshots :: Int64 -> SqlPersistT IO ()
deleteDiskAndSnapshots diskId = do
  deleteWhere [M.SnapshotDiskImageId ==. toSqlKey diskId]
  delete (toSqlKey diskId :: DiskImageId)

-- | List all disk images with attachment info
listDiskImages :: SqlPersistT IO [DiskImageInfo]
listDiskImages = do
  disks <- selectList [] [Asc M.DiskImageName]
  forM disks $ \(Entity key disk) -> do
    attachedVms <- getAttachedVms (fromSqlKey key)
    pure $
      DiskImageInfo
        { diiId = fromSqlKey key,
          diiName = diskImageName disk,
          diiFilePath = diskImageFilePath disk,
          diiFormat = diskImageFormat disk,
          diiSizeMb = diskImageSizeMb disk,
          diiCreatedAt = diskImageCreatedAt disk,
          diiAttachedTo = attachedVms
        }

-- | Get disk image info
getDiskImageInfo :: Int64 -> SqlPersistT IO (Maybe DiskImageInfo)
getDiskImageInfo diskId = do
  mDisk <- get (toSqlKey diskId :: DiskImageId)
  case mDisk of
    Nothing -> pure Nothing
    Just disk -> do
      attachedVms <- getAttachedVms diskId
      pure $
        Just
          DiskImageInfo
            { diiId = diskId,
              diiName = diskImageName disk,
              diiFilePath = diskImageFilePath disk,
              diiFormat = diskImageFormat disk,
              diiSizeMb = diskImageSizeMb disk,
              diiCreatedAt = diskImageCreatedAt disk,
              diiAttachedTo = attachedVms
            }

-- | Get snapshots for a disk
getSnapshots :: Int64 -> SqlPersistT IO [SnapshotInfo]
getSnapshots diskId = do
  snapshots <- selectList [M.SnapshotDiskImageId ==. toSqlKey diskId] [Asc M.SnapshotCreatedAt]
  pure $
    map
      ( \(Entity key snap) ->
          SnapshotInfo
            { sniId = fromSqlKey key,
              sniName = snapshotName snap,
              sniCreatedAt = snapshotCreatedAt snap,
              sniSizeMb = snapshotSizeMb snap
            }
      )
      snapshots
