{-# LANGUAGE OverloadedStrings #-}

-- | Disk image management handlers.
-- Handles disk image CRUD operations, snapshots, and attach/detach.
module Corvus.Handlers.Disk
  ( -- * Disk image handlers
    handleDiskCreate,
    handleDiskCreateOverlay,
    handleDiskRegister,
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
import Corvus.Qemu.Config (QemuConfig, getEffectiveBasePath)
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
-- Path Utilities
--------------------------------------------------------------------------------

-- | Sanitize a disk image name to prevent path traversal attacks.
-- First removes dangerous characters (path separators, null bytes), then
-- removes parent directory references to handle cases like ".\0." -> ".."
sanitizeDiskName :: Text -> Either Text Text
sanitizeDiskName name
  | T.null sanitized = Left "Invalid disk name: name is empty after sanitization"
  | otherwise = Right sanitized
  where
    sanitized =
      T.replace ".." "" $
        T.filter (`notElem` ['/', '\\', '\0']) name

-- | Resolve a disk image file path, handling relative paths.
-- Relative paths (not starting with /) are resolved against the base path.
resolveDiskPath :: QemuConfig -> DiskImage -> IO FilePath
resolveDiskPath config disk = do
  basePath <- getEffectiveBasePath config
  let rawPath = T.unpack $ diskImageFilePath disk
  pure $
    if "/" `T.isPrefixOf` diskImageFilePath disk
      then rawPath
      else basePath </> rawPath

--------------------------------------------------------------------------------
-- Disk Image Handlers
--------------------------------------------------------------------------------

-- | Create a new disk image
handleDiskCreate :: ServerState -> Text -> DriveFormat -> Int64 -> IO Response
handleDiskCreate state name format sizeMb = runStdoutLoggingT $ do
  logInfoN $ "Creating disk image: " <> name <> " (" <> T.pack (show sizeMb) <> " MB)"

  -- Sanitize the name to prevent path traversal attacks
  case sanitizeDiskName name of
    Left err -> do
      logWarnN $ "Invalid disk name: " <> err
      pure $ RespError err
    Right safeName -> do
      -- Generate file path using sanitized name
      basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
      let fileName = T.unpack safeName <> "." <> T.unpack (enumToText format)
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
          -- Store in database with original name for display, but sanitized path
          now <- liftIO getCurrentTime
          diskId <-
            liftIO $
              runSqlPool
                ( insert
                    DiskImage
                      { diskImageName = safeName,
                        diskImageFilePath = T.pack filePath,
                        diskImageFormat = format,
                        diskImageSizeMb = Just (fromIntegral sizeMb),
                        diskImageCreatedAt = now,
                        diskImageBackingImageId = Nothing
                      }
                )
                (ssDbPool state)
          logInfoN $ "Created disk image with ID: " <> T.pack (show $ fromSqlKey diskId)
          pure $ RespDiskCreated $ fromSqlKey diskId

-- | Register an existing disk image file
handleDiskRegister ::
  ServerState ->
  Text ->
  Text ->
  DriveFormat ->
  Maybe Int64 ->
  IO Response
handleDiskRegister state name filePath format mSizeMb = runStdoutLoggingT $ do
  logInfoN $ "Registering disk image: " <> name <> " at " <> filePath

  -- Store in database
  now <- liftIO getCurrentTime
  mExisting <-
    liftIO $
      runSqlPool
        ( getBy (UniqueImagePath filePath)
        )
        (ssDbPool state)

  case mExisting of
    Just (Entity diskId _) -> do
      logInfoN $ "Disk image already registered with ID: " <> T.pack (show $ fromSqlKey diskId)
      pure $ RespDiskCreated $ fromSqlKey diskId
    Nothing -> do
      diskId <-
        liftIO $
          runSqlPool
            ( insert
                DiskImage
                  { diskImageName = name,
                    diskImageFilePath = filePath,
                    diskImageFormat = format,
                    diskImageSizeMb = fmap fromIntegral mSizeMb,
                    diskImageCreatedAt = now,
                    diskImageBackingImageId = Nothing
                  }
            )
            (ssDbPool state)
      logInfoN $ "Registered disk image with ID: " <> T.pack (show $ fromSqlKey diskId)
      pure $ RespDiskCreated $ fromSqlKey diskId

-- | Create a qcow2 overlay backed by an existing disk image
handleDiskCreateOverlay :: ServerState -> T.Text -> Int64 -> IO Response
handleDiskCreateOverlay state name baseDiskId = runStdoutLoggingT $ do
  logInfoN $ "Creating overlay '" <> name <> "' backed by disk " <> T.pack (show baseDiskId)

  case sanitizeDiskName name of
    Left err -> do
      logWarnN $ "Invalid overlay name: " <> err
      pure $ RespError err
    Right safeName -> do
      mBaseDisk <- liftIO $ runSqlPool (get (toSqlKey baseDiskId :: DiskImageId)) (ssDbPool state)
      case mBaseDisk of
        Nothing -> pure RespDiskNotFound
        Just baseDisk -> do
          rwDrives <-
            liftIO $
              runSqlPool
                ( selectList
                    [ M.DriveDiskImageId ==. toSqlKey baseDiskId,
                      M.DriveReadOnly ==. False
                    ]
                    []
                )
                (ssDbPool state)
          if not (null rwDrives)
            then do
              let vmIds = map (fromSqlKey . driveVmId . entityVal) rwDrives
              logWarnN $ "Base image is attached read-write to VMs: " <> T.pack (show vmIds)
              pure $ RespError "Cannot use as base: image is attached read-write to VM(s)"
            else do
              basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
              let overlayFileName = T.unpack safeName <> ".qcow2"
                  overlayFilePath = basePath </> overlayFileName
              baseFilePath <- liftIO $ resolveDiskPath (ssQemuConfig state) baseDisk
              result <- liftIO $ createOverlay overlayFilePath baseFilePath (diskImageFormat baseDisk)
              case result of
                ImageError err -> do
                  logWarnN $ "Failed to create overlay: " <> err
                  pure $ RespError err
                _ -> do
                  now <- liftIO getCurrentTime
                  diskId <-
                    liftIO $
                      runSqlPool
                        ( insert
                            DiskImage
                              { diskImageName = safeName,
                                diskImageFilePath = T.pack overlayFilePath,
                                diskImageFormat = FormatQcow2,
                                diskImageSizeMb = diskImageSizeMb baseDisk,
                                diskImageCreatedAt = now,
                                diskImageBackingImageId = Just (toSqlKey baseDiskId)
                              }
                        )
                        (ssDbPool state)
                  logInfoN $ "Created overlay with ID: " <> T.pack (show $ fromSqlKey diskId)
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
          -- Check if disk is used as backing image for overlays
          overlayIds <- liftIO $ runSqlPool (getOverlayIds diskId) (ssDbPool state)
          if not (null overlayIds)
            then pure $ RespDiskHasOverlays overlayIds
            else do
              -- Delete the file (resolve relative path against base path)
              filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
              result <- liftIO $ deleteImage filePath
              case result of
                ImageError err -> do
                  logWarnN $ "Failed to delete image file: " <> err
                  pure $ RespError err
                ImageNotFound -> do
                  logWarnN "Image file not found, removing from database anyway"
                  liftIO $ runSqlPool (deleteDiskAndSnapshots diskId) (ssDbPool state)
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
          -- Check if disk is used as backing image for overlays
          overlayIds <- liftIO $ runSqlPool (getOverlayIds diskId) (ssDbPool state)
          if not (null overlayIds)
            then pure $ RespDiskHasOverlays overlayIds
            else do
              filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
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
      -- Check format - only qcow2 supports snapshots
      if diskImageFormat disk /= FormatQcow2
        then do
          logWarnN "Snapshot requested on non-qcow2 image"
          pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
        else do
          -- Check if any attached VM is running or paused
          runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
          if not (null runningVms)
            then pure RespVmMustBeStopped
            else do
              filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
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

  -- First check disk exists
  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      -- Check format supports snapshots - only qcow2 supports snapshots
      if diskImageFormat disk /= FormatQcow2
        then pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
        else do
          -- Check if any attached VM is running or paused
          runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
          if not (null runningVms)
            then pure RespVmMustBeStopped
            else do
              -- Check snapshot exists
              mSnapshot <- liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
              case mSnapshot of
                Nothing -> pure RespSnapshotNotFound
                Just snapshot -> do
                  -- Verify snapshot belongs to this disk
                  if snapshotDiskImageId snapshot /= toSqlKey diskId
                    then pure RespSnapshotNotFound
                    else do
                      filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
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

  -- First check disk exists
  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      -- Check format supports snapshots - only qcow2 supports snapshots
      if diskImageFormat disk /= FormatQcow2
        then pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
        else do
          -- Check if any attached VM is running
          runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
          if not (null runningVms)
            then pure RespVmMustBeStopped
            else do
              -- Check snapshot exists
              mSnapshot <- liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
              case mSnapshot of
                Nothing -> pure RespSnapshotNotFound
                Just snapshot -> do
                  -- Verify snapshot belongs to this disk
                  if snapshotDiskImageId snapshot /= toSqlKey diskId
                    then pure RespSnapshotNotFound
                    else do
                      filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
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

  -- First check disk exists
  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      -- Check format supports snapshots - only qcow2 supports snapshots
      if diskImageFormat disk /= FormatQcow2
        then pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
        else do
          -- Check if any attached VM is running
          runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
          if not (null runningVms)
            then pure RespVmMustBeStopped
            else do
              -- Check snapshot exists
              mSnapshot <- liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
              case mSnapshot of
                Nothing -> pure RespSnapshotNotFound
                Just snapshot -> do
                  -- Verify snapshot belongs to this disk
                  if snapshotDiskImageId snapshot /= toSqlKey diskId
                    then pure RespSnapshotNotFound
                    else do
                      filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
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
  Bool ->
  Bool ->
  CacheType ->
  IO Response
handleDiskAttach state vmId diskId interface media readOnly discard cache = runStdoutLoggingT $ do
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
                            driveReadOnly = readOnly,
                            driveCacheType = cache,
                            driveDiscard = discard
                          }
                    )
                    (ssDbPool state)

              -- If VM is running or paused, hot-plug via QMP (both have live QEMU process)
              if vmStatus vm == VmRunning || vmStatus vm == VmPaused
                then do
                  logInfoN $ "VM is " <> enumToText (vmStatus vm) <> ", performing hot-plug"
                  let nodeName = "drive-" <> T.pack (show $ fromSqlKey driveId)
                      deviceId = "device-" <> T.pack (show $ fromSqlKey driveId)
                      format = diskImageFormat disk
                  filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk

                  -- Add block device
                  blockResult <- liftIO $ qmpBlockdevAdd vmId nodeName filePath format readOnly
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
                  logInfoN "VM is not active, disk attached to database only"
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
              -- If VM is running or paused, hot-unplug via QMP (both have live QEMU process)
              if vmStatus vm == VmRunning || vmStatus vm == VmPaused
                then do
                  logInfoN $ "VM is " <> enumToText (vmStatus vm) <> ", performing hot-unplug"
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
-- | Get VMs with active QEMU processes that have this disk attached
-- Both running and paused VMs have live QEMU processes holding disk files open
getRunningAttachedVms :: Int64 -> SqlPersistT IO [Int64]
getRunningAttachedVms diskId = do
  drives <- selectList [M.DriveDiskImageId ==. toSqlKey diskId] []
  let vmKeys = map (driveVmId . entityVal) drives
  activeVms <- selectList [M.VmId <-. vmKeys, M.VmStatus <-. [VmRunning, VmPaused]] []
  pure $ map (fromSqlKey . entityKey) activeVms

-- | Get overlay disk IDs that reference this disk as a backing image
getOverlayIds :: Int64 -> SqlPersistT IO [Int64]
getOverlayIds diskId = do
  overlays <- selectList [M.DiskImageBackingImageId ==. Just (toSqlKey diskId)] []
  pure $ map (fromSqlKey . entityKey) overlays

-- | Delete disk and its snapshots
deleteDiskAndSnapshots :: Int64 -> SqlPersistT IO ()
deleteDiskAndSnapshots diskId = do
  deleteWhere [M.SnapshotDiskImageId ==. toSqlKey diskId]
  delete (toSqlKey diskId :: DiskImageId)

-- | Resolve backing image name from optional key
getBackingImageName :: Maybe DiskImageId -> SqlPersistT IO (Maybe T.Text)
getBackingImageName Nothing = pure Nothing
getBackingImageName (Just backingKey) = do
  mBacking <- get backingKey
  pure $ fmap diskImageName mBacking

-- | List all disk images with attachment info
listDiskImages :: SqlPersistT IO [DiskImageInfo]
listDiskImages = do
  disks <- selectList [] [Asc M.DiskImageName]
  forM disks $ \(Entity key disk) -> do
    attachedVms <- getAttachedVms (fromSqlKey key)
    backingName <- getBackingImageName (diskImageBackingImageId disk)
    pure $
      DiskImageInfo
        { diiId = fromSqlKey key,
          diiName = diskImageName disk,
          diiFilePath = diskImageFilePath disk,
          diiFormat = diskImageFormat disk,
          diiSizeMb = diskImageSizeMb disk,
          diiCreatedAt = diskImageCreatedAt disk,
          diiAttachedTo = attachedVms,
          diiBackingImageId = fmap fromSqlKey (diskImageBackingImageId disk),
          diiBackingImageName = backingName
        }

-- | Get disk image info
getDiskImageInfo :: Int64 -> SqlPersistT IO (Maybe DiskImageInfo)
getDiskImageInfo diskId = do
  mDisk <- get (toSqlKey diskId :: DiskImageId)
  case mDisk of
    Nothing -> pure Nothing
    Just disk -> do
      attachedVms <- getAttachedVms diskId
      backingName <- getBackingImageName (diskImageBackingImageId disk)
      pure $
        Just
          DiskImageInfo
            { diiId = diskId,
              diiName = diskImageName disk,
              diiFilePath = diskImageFilePath disk,
              diiFormat = diskImageFormat disk,
              diiSizeMb = diskImageSizeMb disk,
              diiCreatedAt = diskImageCreatedAt disk,
              diiAttachedTo = attachedVms,
              diiBackingImageId = fmap fromSqlKey (diskImageBackingImageId disk),
              diiBackingImageName = backingName
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
