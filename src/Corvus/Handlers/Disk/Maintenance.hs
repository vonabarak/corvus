{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Maintenance disk-image handlers.
module Corvus.Handlers.Disk.Maintenance
  ( DiskDelete (..)
  , DiskResize (..)
  , DiskRefresh (..)
  )
where

import Corvus.Action

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import Corvus.Handlers.Disk.Agent
  ( cloneImageViaAgent
  , createImageViaAgent
  , createOverlayViaAgent
  , deleteImageViaAgent
  , getImageInfoViaAgent
  , getImageSizeMbViaAgent
  , resizeImageViaAgent
  )
import Corvus.Handlers.Disk.Attach (DiskAttach (..), DiskDetachByDisk (..), handleDiskAttach, handleDiskDetach)
import Corvus.Handlers.Disk.Db (deleteDiskAndSnapshots, deleteDiskImageNodeRow, diskImageNodeFilePathFor, getAttachedVms, getBackingChainIds, getDiskImageInfo, getOverlayIds, getReadWriteAttachedVms, getRunningAttachedVms, hasPlacementOnNode, listDiskImageNodes, listDiskImages, recordDiskImageNode)
import Corvus.Handlers.Disk.Import (DiskImportAction (..), handleDiskImportCopy)
import Corvus.Handlers.Disk.Path (makeRelativeToBase, resolveDiskFilePath, resolveDiskFilePathPure, resolveDiskPath, sanitizeDiskName)
import Corvus.Handlers.Disk.Rebase (DiskRebase (..), handleDiskRebase)
import Corvus.Handlers.Disk.Snapshot (SnapshotCreate (..), SnapshotDelete (..), SnapshotMerge (..), SnapshotRollback (..), handleSnapshotCreate, handleSnapshotDelete, handleSnapshotList, handleSnapshotMerge, handleSnapshotRollback)
import Corvus.Handlers.Disk.Transfer (stageBackingChain, transferImageBetweenNodes)
import Corvus.Handlers.Resolve (ResolveError (..), resolveErrorMessage, resolveNode, validateName)
import Corvus.Handlers.Scheduler (pickNodeForDisk, pickNodeForExistingDisk)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Node.Image (ImageInfo (..), ImageResult (..), detectFormatFromPath)
import Corvus.Protocol
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (runSqlPool)
import System.FilePath (takeExtension, takeFileName, (</>))

-- | Refresh a disk image's size by querying qemu-img info
handleDiskRefresh :: ServerState -> Int64 -> IO Response
handleDiskRefresh state diskId = runServerLogging state $ do
  logInfoN $ "Refreshing disk image size: " <> T.pack (show diskId)
  mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
  case mNid of
    Left err -> pure $ RespError err
    Right nid -> do
      mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
      case mDisk of
        Nothing -> pure RespDiskNotFound
        Just _disk -> do
          let key = toSqlKey diskId :: DiskImageId
          resolvedPath <- liftIO $ resolveDiskPath (ssDbPool state) (ssQemuConfig state) key nid
          mSize <- liftIO $ getImageSizeMbViaAgent state nid resolvedPath
          case mSize of
            Nothing -> pure $ RespError "Could not determine disk image size"
            Just newSize -> do
              liftIO $
                runSqlPool
                  (update (toSqlKey diskId :: DiskImageId) [DiskImageSizeMb =. Just newSize])
                  (ssDbPool state)
              logInfoN $ "Updated size to " <> T.pack (show newSize) <> " MB"
              pure RespDiskOk

-- | Delete a disk image. Walks every 'DiskImageNode' placement
-- and asks each node's nodeagent to delete the on-disk file, then
-- drops the join rows + the logical image.
handleDiskDelete :: ServerState -> Int64 -> IO Response
handleDiskDelete state diskId = runServerLogging state $ do
  logInfoN $ "Deleting disk image: " <> T.pack (show diskId)

  let pool = ssDbPool state
      key = toSqlKey diskId :: DiskImageId
  mDisk <- liftIO $ runSqlPool (get key) pool
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just _disk -> do
      attachedVms <- liftIO $ runSqlPool (getAttachedVms diskId) pool
      if not (null attachedVms)
        then pure $ RespDiskInUse attachedVms
        else do
          overlayIds <- liftIO $ runSqlPool (getOverlayIds diskId) pool
          if not (null overlayIds)
            then pure $ RespDiskHasOverlays overlayIds
            else do
              placements <- liftIO $ runSqlPool (listDiskImageNodes key) pool
              -- Delete the on-disk file on every node we've ever
              -- recorded for this image. Per-node failures are
              -- logged but don't abort the rest of the cleanup —
              -- the operator can re-run @crv disk delete@ after
              -- the failing node comes back, and the DB drop at
              -- the end is idempotent.
              forM_ placements $ \(Entity _ row) -> do
                let nid = diskImageNodeNodeId row
                resolved <- liftIO $ resolveDiskPath pool (ssQemuConfig state) key nid
                result <- liftIO $ deleteImageViaAgent state nid resolved
                case result of
                  ImageError err ->
                    logWarnN $
                      "Failed to delete file on node "
                        <> T.pack (show (fromSqlKey nid))
                        <> ": "
                        <> err
                  ImageNotFound ->
                    logWarnN $
                      "Image file already gone on node "
                        <> T.pack (show (fromSqlKey nid))
                  _ -> pure ()
                liftIO $
                  runSqlPool (deleteDiskImageNodeRow key nid) pool
              liftIO $ runSqlPool (deleteDiskAndSnapshots diskId) pool
              logInfoN $ "Deleted disk image: " <> T.pack (show diskId)
              pure RespDiskOk

-- | Resize a disk image (VM must be stopped). Resizes on every
-- node that hosts a placement, then updates the logical size.
handleDiskResize :: ServerState -> Int64 -> Int64 -> IO Response
handleDiskResize state diskId newSizeMb = runServerLogging state $ do
  logInfoN $ "Resizing disk image " <> T.pack (show diskId) <> " to " <> T.pack (show newSizeMb) <> " MB"

  let key = toSqlKey diskId :: DiskImageId
      pool = ssDbPool state
  mDisk <- liftIO $ runSqlPool (get key) pool
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just _disk -> do
      runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) pool
      if not (null runningVms)
        then pure RespVmMustBeStopped
        else do
          overlayIds <- liftIO $ runSqlPool (getOverlayIds diskId) pool
          if not (null overlayIds)
            then pure $ RespDiskHasOverlays overlayIds
            else do
              placements <- liftIO $ runSqlPool (listDiskImageNodes key) pool
              outcomes <- liftIO $ forM placements $ \(Entity _ row) -> do
                let nid = diskImageNodeNodeId row
                filePath <- resolveDiskPath pool (ssQemuConfig state) key nid
                result <- resizeImageViaAgent state nid filePath newSizeMb
                pure (nid, result)
              let failures =
                    [ "node " <> T.pack (show (fromSqlKey nid)) <> ": " <> renderResizeFailure result
                    | (nid, result) <- outcomes
                    , result /= ImageSuccess
                    ]
              if not (null failures)
                then do
                  let msg = "Resize failed on " <> T.intercalate "; " failures <> ". Logical size was not updated."
                  logWarnN msg
                  pure $ RespError msg
                else do
                  liftIO $
                    runSqlPool
                      (update key [M.DiskImageSizeMb =. Just (fromIntegral newSizeMb)])
                      pool
                  logInfoN "Disk resized successfully on every placement"
                  pure RespDiskOk
  where
    renderResizeFailure = \case
      ImageSuccess -> ""
      ImageError err -> err
      ImageNotFound -> "image not found"
      ImageFormatNotSupported msg -> msg

newtype DiskDelete = DiskDelete {ddelDiskId :: Int64}

instance Action DiskDelete where
  actionSubsystem _ = SubDisk
  actionCommand _ = "delete"
  actionEntityId = Just . fromIntegral . ddelDiskId
  actionExecute ctx a = handleDiskDelete (acState ctx) (ddelDiskId a)

data DiskResize = DiskResize
  { drzDiskId :: Int64
  , drzNewSizeMb :: Int64
  }

instance Action DiskResize where
  actionSubsystem _ = SubDisk
  actionCommand _ = "resize"
  actionEntityId = Just . fromIntegral . drzDiskId
  actionExecute ctx a = handleDiskResize (acState ctx) (drzDiskId a) (drzNewSizeMb a)

newtype DiskRefresh = DiskRefresh {drfDiskId :: Int64}

instance Action DiskRefresh where
  actionSubsystem _ = SubDisk
  actionCommand _ = "refresh"
  actionEntityId = Just . fromIntegral . drfDiskId
  actionExecute ctx a = handleDiskRefresh (acState ctx) (drfDiskId a)
