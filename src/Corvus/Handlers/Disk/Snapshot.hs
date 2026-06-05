{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Disk snapshot handlers (create / delete / rollback / merge / list).
--
-- Snapshots are a qcow2-only feature; the handlers reject other formats
-- with 'RespFormatNotSupported'.
--
-- == Live vs offline routing
--
-- Create / delete / merge transparently route between two backends
-- based on whether the disk is currently attached to a running or
-- paused VM:
--
-- * No attached VM (or all attached VMs are stopped): the OFFLINE
--   path runs @qemu-img snapshot -c/-d@ via
--   'createSnapshotViaAgent' / 'deleteSnapshotViaAgent'.
-- * Exactly one running/paused VM has the disk attached: the LIVE
--   path runs @blockdev-snapshot-internal-sync@ /
--   @blockdev-snapshot-delete-internal-sync@ via QMP, optionally
--   bracketed with QGA @guest-fsfreeze-freeze@/@thaw@ per the
--   caller's 'QuiesceMode'.
--
-- ROLLBACK is asymmetric: QEMU exposes no online equivalent of
-- @qemu-img snapshot -a@, so the offline path is the only choice.
-- The optional @autoStop@ knob on 'handleSnapshotRollback' lets
-- the daemon orchestrate a graceful @VmStop@ → rollback →
-- @VmStart@ cycle so the operator still issues one command, but
-- the VM does cycle. With @autoStop=False@ the handler refuses
-- the call on a running/paused VM (today's behaviour preserved).
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
import Corvus.Handlers.Disk.Agent
  ( createSnapshotViaAgent
  , createSnapshotViaAgentLive
  , deleteSnapshotViaAgent
  , deleteSnapshotViaAgentLive
  , mergeSnapshotViaAgent
  , rollbackSnapshotViaAgent
  )
import Corvus.Handlers.Scheduler (pickNodeForExistingDisk)
import Corvus.Model
import Corvus.Node.Image (ImageResult (..))
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Protocol
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)

-- | Create a snapshot. Dispatches transparently between the
-- offline path (qemu-img on a stopped disk) and the live path
-- (QMP on a running or paused VM). 'QuiesceMode' only matters for
-- the live path; on the offline path it is ignored.
handleSnapshotCreate
  :: ServerState -> Int64 -> Text -> NOA.QuiesceMode -> IO Response
handleSnapshotCreate state diskId snapshotName quiesce =
  case validateName "Snapshot" snapshotName of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Creating snapshot '" <> snapshotName <> "' for disk " <> T.pack (show diskId)

      mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
      case mDisk of
        Nothing -> pure RespDiskNotFound
        Just disk
          | diskImageFormat disk /= FormatQcow2 -> do
              logWarnN "Snapshot requested on non-qcow2 image"
              pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
          | otherwise -> do
              mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
              case mNid of
                Left err -> pure $ RespError err
                Right nid -> do
                  filePath <-
                    liftIO $
                      resolveDiskPath
                        (ssDbPool state)
                        (ssQemuConfig state)
                        (toSqlKey diskId :: DiskImageId)
                        nid
                  runningVms <-
                    liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
                  (result, isLive, quiesced) <- case runningVms of
                    [] -> do
                      logInfoN "Snapshot path: offline (no running attached VM)"
                      r <- liftIO $ createSnapshotViaAgent state nid filePath snapshotName
                      pure (r, False, False)
                    (vmId : _) -> do
                      logInfoN $
                        "Snapshot path: live via QMP (VM "
                          <> T.pack (show vmId)
                          <> ", quiesce="
                          <> T.pack (show quiesce)
                          <> ")"
                      (r, q) <-
                        liftIO $
                          createSnapshotViaAgentLive
                            state
                            nid
                            filePath
                            snapshotName
                            vmId
                            quiesce
                      pure (r, True, q)
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
                                  , snapshotLive = isLive
                                  , snapshotQuiesced = quiesced
                                  }
                            )
                            (ssDbPool state)
                      logInfoN $
                        "Created snapshot with ID: "
                          <> T.pack (show $ fromSqlKey snapshotId)
                          <> (if isLive then " (live)" else " (offline)")
                          <> (if quiesced then ", quiesced" else "")
                      pure $ RespSnapshotCreated $ fromSqlKey snapshotId
                    ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                    ImageError err -> pure $ RespError err
                    ImageNotFound -> pure RespDiskNotFound

-- | Delete a snapshot. Routes to QMP delete on a running VM,
-- @qemu-img snapshot -d@ otherwise.
handleSnapshotDelete :: ServerState -> Int64 -> Ref -> IO Response
handleSnapshotDelete state diskId snapRef = runServerLogging state $ do
  logInfoN $ "Deleting snapshot '" <> unRef snapRef <> "' from disk " <> T.pack (show diskId)

  -- First check disk exists
  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk
      | diskImageFormat disk /= FormatQcow2 ->
          pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
      | otherwise -> do
          mSnapId <- liftIO $ resolveSnapshot snapRef diskId (ssDbPool state)
          case mSnapId of
            Left _ -> pure RespSnapshotNotFound
            Right snapshotId -> do
              mSnapshot <-
                liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
              case mSnapshot of
                Nothing -> pure RespSnapshotNotFound
                Just snapshot -> do
                  mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
                  case mNid of
                    Left err -> pure $ RespError err
                    Right nid -> do
                      filePath <-
                        liftIO $
                          resolveDiskPath
                            (ssDbPool state)
                            (ssQemuConfig state)
                            (toSqlKey diskId :: DiskImageId)
                            nid
                      runningVms <-
                        liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
                      result <- case runningVms of
                        [] -> do
                          logInfoN "Snapshot delete path: offline"
                          liftIO $
                            deleteSnapshotViaAgent state nid filePath (snapshotName snapshot)
                        (vmId : _) -> do
                          logInfoN $
                            "Snapshot delete path: live via QMP (VM "
                              <> T.pack (show vmId)
                              <> ")"
                          liftIO $
                            deleteSnapshotViaAgentLive
                              state
                              nid
                              filePath
                              (snapshotName snapshot)
                              vmId
                      case result of
                        ImageSuccess -> do
                          liftIO $
                            runSqlPool
                              (delete (toSqlKey snapshotId :: SnapshotId))
                              (ssDbPool state)
                          logInfoN "Snapshot deleted"
                          pure RespSnapshotOk
                        ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                        ImageError err -> pure $ RespError err
                        ImageNotFound -> pure RespDiskNotFound

-- | Offline rollback to a snapshot. Refuses the call when any
-- attached VM is running/paused — QEMU has no online rollback,
-- so the underlying @qemu-img snapshot -a@ would collide with
-- the live qemu's file lock.
--
-- For the orchestrated "stop → rollback → start" path see
-- 'Corvus.Handlers.Disk.SnapshotAutoStop.handleSnapshotRollbackAutoStop'.
handleSnapshotRollback :: ServerState -> Int64 -> Ref -> IO Response
handleSnapshotRollback state diskId snapRef = runServerLogging state $ do
  logInfoN $
    "Rolling back disk "
      <> T.pack (show diskId)
      <> " to snapshot '"
      <> unRef snapRef
      <> "'"

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk
      | diskImageFormat disk /= FormatQcow2 ->
          pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
      | otherwise -> do
          runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
          if not (null runningVms)
            then pure RespVmMustBeStopped
            else do
              mSnapId <- liftIO $ resolveSnapshot snapRef diskId (ssDbPool state)
              case mSnapId of
                Left _ -> pure RespSnapshotNotFound
                Right snapshotId -> do
                  mSnapshot <-
                    liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
                  case mSnapshot of
                    Nothing -> pure RespSnapshotNotFound
                    Just snapshot -> do
                      mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
                      case mNid of
                        Left err -> pure $ RespError err
                        Right nid -> do
                          filePath <-
                            liftIO $
                              resolveDiskPath
                                (ssDbPool state)
                                (ssQemuConfig state)
                                (toSqlKey diskId :: DiskImageId)
                                nid
                          result <-
                            liftIO $
                              rollbackSnapshotViaAgent
                                state
                                nid
                                filePath
                                (snapshotName snapshot)
                          case result of
                            ImageSuccess -> do
                              logInfoN "Rollback complete"
                              pure RespSnapshotOk
                            ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                            ImageError err -> pure $ RespError err
                            ImageNotFound -> pure RespDiskNotFound

-- | Merge a snapshot. On qcow2 the merge operation IS the
-- snapshot-delete (the current disk state is preserved; only the
-- snapshot record is dropped), so the dispatch mirrors
-- 'handleSnapshotDelete': QMP delete if any attached VM is
-- running/paused, @qemu-img snapshot -d@ otherwise.
handleSnapshotMerge :: ServerState -> Int64 -> Ref -> IO Response
handleSnapshotMerge state diskId snapRef = runServerLogging state $ do
  logInfoN $ "Merging snapshot '" <> unRef snapRef <> "' for disk " <> T.pack (show diskId)

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk
      | diskImageFormat disk /= FormatQcow2 ->
          pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
      | otherwise -> do
          mSnapId <- liftIO $ resolveSnapshot snapRef diskId (ssDbPool state)
          case mSnapId of
            Left _ -> pure RespSnapshotNotFound
            Right snapshotId -> do
              mSnapshot <-
                liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
              case mSnapshot of
                Nothing -> pure RespSnapshotNotFound
                Just snapshot -> do
                  mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
                  case mNid of
                    Left err -> pure $ RespError err
                    Right nid -> do
                      filePath <-
                        liftIO $
                          resolveDiskPath
                            (ssDbPool state)
                            (ssQemuConfig state)
                            (toSqlKey diskId :: DiskImageId)
                            nid
                      runningVms <-
                        liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
                      result <- case runningVms of
                        [] -> do
                          logInfoN "Snapshot merge path: offline"
                          liftIO $
                            mergeSnapshotViaAgent
                              state
                              nid
                              filePath
                              (snapshotName snapshot)
                        (vmId : _) -> do
                          logInfoN $
                            "Snapshot merge path: live via QMP (VM "
                              <> T.pack (show vmId)
                              <> ")"
                          liftIO $
                            deleteSnapshotViaAgentLive
                              state
                              nid
                              filePath
                              (snapshotName snapshot)
                              vmId
                      case result of
                        ImageSuccess -> do
                          liftIO $
                            runSqlPool
                              (delete (toSqlKey snapshotId :: SnapshotId))
                              (ssDbPool state)
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
  , scrQuiesce :: NOA.QuiesceMode
  }

instance Action SnapshotCreate where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "create"
  actionEntityId = Just . fromIntegral . scrDiskId
  actionExecute ctx a =
    handleSnapshotCreate (acState ctx) (scrDiskId a) (scrName a) (scrQuiesce a)

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
