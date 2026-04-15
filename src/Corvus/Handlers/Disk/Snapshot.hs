{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Disk snapshot handlers (create / delete / rollback / merge / list).
--
-- Snapshots are a qcow2-only feature; the handlers reject other formats
-- with 'RespFormatNotSupported'. Mutating operations require the disk's
-- attached VMs to be stopped — live QEMU processes hold exclusive locks
-- on the backing file.
module Corvus.Handlers.Disk.Snapshot
  ( -- * Action types
    SnapshotCreate (..)
  , SnapshotDelete (..)
  , SnapshotRollback (..)
  , SnapshotMerge (..)

    -- * Handlers
  , handleSnapshotCreate
  , handleSnapshotDelete
  , handleSnapshotRollback
  , handleSnapshotMerge
  , handleSnapshotList
  )
where

import Corvus.Action
import Corvus.Handlers.Disk.Db (getRunningAttachedVms, getSnapshots)
import Corvus.Handlers.Disk.Path (resolveDiskPath)
import Corvus.Handlers.Resolve (resolveSnapshot, validateName)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Model
import Corvus.Protocol
import Corvus.Qemu.Image
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)

-- | Create a snapshot
handleSnapshotCreate :: ServerState -> Int64 -> Text -> IO Response
handleSnapshotCreate state diskId snapshotName =
  case validateName "Snapshot" snapshotName of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
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
                                  { snapshotDiskImageId = toSqlKey diskId
                                  , snapshotName = snapshotName
                                  , snapshotCreatedAt = now
                                  , snapshotSizeMb = Nothing
                                  }
                            )
                            (ssDbPool state)
                      logInfoN $ "Created snapshot with ID: " <> T.pack (show $ fromSqlKey snapshotId)
                      pure $ RespSnapshotCreated $ fromSqlKey snapshotId
                    ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                    ImageError err -> pure $ RespError err
                    ImageNotFound -> pure RespDiskNotFound

-- | Delete a snapshot
handleSnapshotDelete :: ServerState -> Int64 -> Ref -> IO Response
handleSnapshotDelete state diskId snapRef = runServerLogging state $ do
  logInfoN $ "Deleting snapshot '" <> unRef snapRef <> "' from disk " <> T.pack (show diskId)

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
              -- Resolve snapshot ref
              mSnapId <- liftIO $ resolveSnapshot snapRef diskId (ssDbPool state)
              case mSnapId of
                Left _ -> pure RespSnapshotNotFound
                Right snapshotId -> do
                  mSnapshot <- liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
                  case mSnapshot of
                    Nothing -> pure RespSnapshotNotFound
                    Just snapshot -> do
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
handleSnapshotRollback :: ServerState -> Int64 -> Ref -> IO Response
handleSnapshotRollback state diskId snapRef = runServerLogging state $ do
  logInfoN $ "Rolling back disk " <> T.pack (show diskId) <> " to snapshot '" <> unRef snapRef <> "'"

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      if diskImageFormat disk /= FormatQcow2
        then pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
        else do
          runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
          if not (null runningVms)
            then pure RespVmMustBeStopped
            else do
              mSnapId <- liftIO $ resolveSnapshot snapRef diskId (ssDbPool state)
              case mSnapId of
                Left _ -> pure RespSnapshotNotFound
                Right snapshotId -> do
                  mSnapshot <- liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
                  case mSnapshot of
                    Nothing -> pure RespSnapshotNotFound
                    Just snapshot -> do
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
handleSnapshotMerge :: ServerState -> Int64 -> Ref -> IO Response
handleSnapshotMerge state diskId snapRef = runServerLogging state $ do
  logInfoN $ "Merging snapshot '" <> unRef snapRef <> "' for disk " <> T.pack (show diskId)

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      if diskImageFormat disk /= FormatQcow2
        then pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
        else do
          runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
          if not (null runningVms)
            then pure RespVmMustBeStopped
            else do
              mSnapId <- liftIO $ resolveSnapshot snapRef diskId (ssDbPool state)
              case mSnapId of
                Left _ -> pure RespSnapshotNotFound
                Right snapshotId -> do
                  mSnapshot <- liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
                  case mSnapshot of
                    Nothing -> pure RespSnapshotNotFound
                    Just snapshot -> do
                      filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
                      result <- liftIO $ mergeSnapshot filePath (snapshotName snapshot)
                      case result of
                        ImageSuccess -> do
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
-- Action Types
--------------------------------------------------------------------------------

data SnapshotCreate = SnapshotCreate
  { scrDiskId :: Int64
  , scrName :: Text
  }

instance Action SnapshotCreate where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "create"
  actionEntityId = Just . fromIntegral . scrDiskId
  actionExecute ctx a = handleSnapshotCreate (acState ctx) (scrDiskId a) (scrName a)

data SnapshotDelete = SnapshotDelete
  { sdelDiskId :: Int64
  , sdelSnapRef :: Ref
  }

instance Action SnapshotDelete where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "delete"
  actionEntityId = Just . fromIntegral . sdelDiskId
  actionExecute ctx a = handleSnapshotDelete (acState ctx) (sdelDiskId a) (sdelSnapRef a)

data SnapshotRollback = SnapshotRollback
  { srlDiskId :: Int64
  , srlSnapRef :: Ref
  }

instance Action SnapshotRollback where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "rollback"
  actionEntityId = Just . fromIntegral . srlDiskId
  actionExecute ctx a = handleSnapshotRollback (acState ctx) (srlDiskId a) (srlSnapRef a)

data SnapshotMerge = SnapshotMerge
  { smrDiskId :: Int64
  , smrSnapRef :: Ref
  }

instance Action SnapshotMerge where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "merge"
  actionEntityId = Just . fromIntegral . smrDiskId
  actionExecute ctx a = handleSnapshotMerge (acState ctx) (smrDiskId a) (smrSnapRef a)
