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
import Corvus.Handlers.Disk.Db (diskImageNodeFilePathFor, getOverlayIds)
import Corvus.Handlers.Disk.Path (resolveDiskPath)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN)
import Corvus.Model
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (runSqlPool)

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
          -- Same-node invariant: the disk image must have a
          -- 'DiskImageNode' row pinning it to the VM's node.
          -- Without that, qemu on the VM's host has no file to
          -- open. Refuse with a clear message naming both sides
          -- so the operator can rsync + 'crv disk register
          -- --node <vm-node>' (or pick a VM on the right node).
          mPlacement <-
            liftIO $
              runSqlPool
                ( diskImageNodeFilePathFor
                    (toSqlKey diskId)
                    (vmNodeId vm)
                )
                (ssDbPool state)
          case mPlacement of
            Nothing ->
              pure $
                RespError $
                  "Disk image '"
                    <> diskImageName disk
                    <> "' is not present on node "
                    <> T.pack (show (fromSqlKey (vmNodeId vm)))
                    <> " where VM '"
                    <> vmName vm
                    <> "' lives"
            Just _ -> do
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
                      -- If VM is running or paused, hot-plug via the
                      -- agent's `vmAttachDrive` RPC (the agent owns
                      -- the QMP socket; the daemon can't connect to
                      -- it from an unprivileged uid).
                      if vmStatus vm `elem` [VmRunning, VmPaused]
                        then do
                          logInfoN $ "VM is " <> enumToText (vmStatus vm) <> ", performing hot-plug"
                          filePath <-
                            liftIO $
                              resolveDiskPath
                                (ssDbPool state)
                                (ssQemuConfig state)
                                (toSqlKey diskId)
                                (vmNodeId vm)
                          let driveIdInt = fromSqlKey driveId
                              fmtTxt = enumToText (diskImageFormat disk)
                              ifTxt = enumToText interface
                          outer <- liftIO $ withVmNodeAgent state vmId $ \nac ->
                            NOA.vmAttachDrive
                              nac
                              vmId
                              driveIdInt
                              (T.pack filePath)
                              fmtTxt
                              ifTxt
                              readOnly
                          case outer of
                            Left err -> do
                              liftIO $ runSqlPool (delete driveId) (ssDbPool state)
                              pure $ RespError err
                            Right r -> case r of
                              Right () -> do
                                logInfoN "Hot-plug successful"
                                pure $ RespDiskAttached driveIdInt
                              Left e -> do
                                liftIO $ runSqlPool (delete driveId) (ssDbPool state)
                                pure $ RespError $ "vmAttachDrive: " <> T.pack (show e)
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
          -- Hot-unplug via the agent's vmDetachDrive RPC (the
          -- agent owns the QMP socket). The blockdev-del busy
          -- retry-loop lives agent-side.
          if vmStatus vm `elem` [VmRunning, VmPaused]
            then do
              logInfoN $ "VM is " <> enumToText (vmStatus vm) <> ", performing hot-unplug"
              outer <- liftIO $ withVmNodeAgent state vmId $ \nac ->
                NOA.vmDetachDrive nac vmId driveId
              case outer of
                Left err -> pure $ RespError err
                Right r -> case r of
                  Right () -> do
                    liftIO $ runSqlPool (delete driveKey) (ssDbPool state)
                    logInfoN "Hot-unplug successful"
                    pure RespDiskOk
                  Left e ->
                    pure $ RespError $ "vmDetachDrive: " <> T.pack (show e)
            else do
              -- Just remove from database
              liftIO $ runSqlPool (delete driveKey) (ssDbPool state)
              logInfoN "Drive detached from database"
              pure RespDiskOk

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
