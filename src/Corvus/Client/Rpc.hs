{-# LANGUAGE OverloadedStrings #-}

-- | RPC call functions for the Corvus client.
module Corvus.Client.Rpc
  ( -- * Basic commands
    sendPing,
    getStatus,
    requestShutdown,

    -- * VM listing
    listVms,
    showVm,

    -- * VM actions
    VmActionResult (..),
    vmStart,
    vmStop,
    vmPause,
    vmReset,

    -- * Disk operations
    DiskResult (..),
    diskCreate,
    diskDelete,
    diskResize,
    diskList,
    diskShow,
    diskAttach,
    diskDetach,

    -- * Snapshot operations
    SnapshotResult (..),
    snapshotCreate,
    snapshotDelete,
    snapshotRollback,
    snapshotMerge,
    snapshotList,
  )
where

import Corvus.Client.Connection
import Corvus.Model (DriveFormat, DriveInterface, DriveMedia, VmStatus)
import Corvus.Protocol
import Data.Int (Int64)
import Data.Text (Text)

-- | Result of a VM action
data VmActionResult
  = -- | Action succeeded, new status
    VmActionSuccess !VmStatus
  | -- | VM not found
    VmActionNotFound
  | -- | Invalid transition: current status and error
    VmActionInvalid !VmStatus !Text
  deriving (Eq, Show)

-- | Send a ping request
sendPing :: Connection -> IO (Either ConnectionError ())
sendPing conn = do
  result <- sendRequest conn ReqPing
  case result of
    Left err -> pure $ Left err
    Right RespPong -> pure $ Right ()
    Right (RespError msg) -> pure $ Left $ ServerError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Get daemon status
getStatus :: Connection -> IO (Either ConnectionError StatusInfo)
getStatus conn = do
  result <- sendRequest conn ReqStatus
  case result of
    Left err -> pure $ Left err
    Right (RespStatus info) -> pure $ Right info
    Right (RespError msg) -> pure $ Left $ ServerError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Request daemon shutdown
requestShutdown :: Connection -> IO (Either ConnectionError Bool)
requestShutdown conn = do
  result <- sendRequest conn ReqShutdown
  case result of
    Left err -> pure $ Left err
    Right (RespShutdownAck ack) -> pure $ Right ack
    Right (RespError msg) -> pure $ Left $ ServerError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | List all VMs
listVms :: Connection -> IO (Either ConnectionError [VmInfo])
listVms conn = do
  result <- sendRequest conn ReqListVms
  case result of
    Left err -> pure $ Left err
    Right (RespVmList vms) -> pure $ Right vms
    Right (RespError msg) -> pure $ Left $ ServerError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Show VM details
showVm :: Connection -> Int64 -> IO (Either ConnectionError (Maybe VmDetails))
showVm conn vmId = do
  result <- sendRequest conn (ReqShowVm vmId)
  case result of
    Left err -> pure $ Left err
    Right (RespVmDetails details) -> pure $ Right (Just details)
    Right RespVmNotFound -> pure $ Right Nothing
    Right (RespError msg) -> pure $ Left $ ServerError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Helper for VM action responses
handleVmActionResponse :: Either ConnectionError Response -> Either ConnectionError VmActionResult
handleVmActionResponse result = case result of
  Left err -> Left err
  Right (RespVmStateChanged newStatus) -> Right $ VmActionSuccess newStatus
  Right RespVmNotFound -> Right VmActionNotFound
  Right (RespInvalidTransition currentStatus errMsg) -> Right $ VmActionInvalid currentStatus errMsg
  Right (RespError msg) -> Left $ ServerError msg
  Right _ -> Left $ DecodeFailed "Unexpected response"

-- | Start a VM (stopped/paused -> running)
vmStart :: Connection -> Int64 -> IO (Either ConnectionError VmActionResult)
vmStart conn vmId = handleVmActionResponse <$> sendRequest conn (ReqVmStart vmId)

-- | Stop a VM (running -> stopped)
vmStop :: Connection -> Int64 -> IO (Either ConnectionError VmActionResult)
vmStop conn vmId = handleVmActionResponse <$> sendRequest conn (ReqVmStop vmId)

-- | Pause a VM (running -> paused)
vmPause :: Connection -> Int64 -> IO (Either ConnectionError VmActionResult)
vmPause conn vmId = handleVmActionResponse <$> sendRequest conn (ReqVmPause vmId)

-- | Reset a VM (any -> stopped)
vmReset :: Connection -> Int64 -> IO (Either ConnectionError VmActionResult)
vmReset conn vmId = handleVmActionResponse <$> sendRequest conn (ReqVmReset vmId)

--------------------------------------------------------------------------------
-- Disk Operations
--------------------------------------------------------------------------------

-- | Result of a disk operation
data DiskResult
  = -- | Operation succeeded
    DiskOk
  | -- | Disk created with new ID
    DiskCreated !Int64
  | -- | Disk info
    DiskInfo !DiskImageInfo
  | -- | List of disk images
    DiskListResult ![DiskImageInfo]
  | -- | Drive attached with new drive ID
    DriveAttached !Int64
  | -- | Disk not found
    DiskNotFound
  | -- | Drive not found
    DriveNotFound
  | -- | Disk is in use by VMs
    DiskInUse ![Int64]
  | -- | VM must be stopped
    VmMustBeStopped
  | -- | Format not supported for operation
    FormatNotSupported !Text
  | -- | VM not found
    DiskVmNotFound
  | -- | Error with message
    DiskError !Text
  deriving (Eq, Show)

-- | Helper for disk operation responses
handleDiskResponse :: Either ConnectionError Response -> Either ConnectionError DiskResult
handleDiskResponse result = case result of
  Left err -> Left err
  Right RespDiskOk -> Right DiskOk
  Right (RespDiskCreated diskId) -> Right $ DiskCreated diskId
  Right (RespDiskInfo info) -> Right $ DiskInfo info
  Right (RespDiskList disks) -> Right $ DiskListResult disks
  Right (RespDiskAttached driveId) -> Right $ DriveAttached driveId
  Right RespDiskNotFound -> Right DiskNotFound
  Right RespDriveNotFound -> Right DriveNotFound
  Right (RespDiskInUse vmIds) -> Right $ DiskInUse vmIds
  Right RespVmMustBeStopped -> Right VmMustBeStopped
  Right (RespFormatNotSupported msg) -> Right $ FormatNotSupported msg
  Right RespVmNotFound -> Right DiskVmNotFound
  Right (RespError msg) -> Left $ ServerError msg
  Right _ -> Left $ DecodeFailed "Unexpected response"

-- | Create a new disk image
diskCreate :: Connection -> Text -> DriveFormat -> Int64 -> IO (Either ConnectionError DiskResult)
diskCreate conn name format sizeMb =
  handleDiskResponse <$> sendRequest conn (ReqDiskCreate name format sizeMb)

-- | Delete a disk image
diskDelete :: Connection -> Int64 -> IO (Either ConnectionError DiskResult)
diskDelete conn diskId = handleDiskResponse <$> sendRequest conn (ReqDiskDelete diskId)

-- | Resize a disk image
diskResize :: Connection -> Int64 -> Int64 -> IO (Either ConnectionError DiskResult)
diskResize conn diskId newSizeMb =
  handleDiskResponse <$> sendRequest conn (ReqDiskResize diskId newSizeMb)

-- | List all disk images
diskList :: Connection -> IO (Either ConnectionError DiskResult)
diskList conn = handleDiskResponse <$> sendRequest conn ReqDiskList

-- | Show disk image details
diskShow :: Connection -> Int64 -> IO (Either ConnectionError DiskResult)
diskShow conn diskId = handleDiskResponse <$> sendRequest conn (ReqDiskShow diskId)

-- | Attach a disk to a VM
diskAttach ::
  Connection ->
  Int64 ->
  Int64 ->
  DriveInterface ->
  Maybe DriveMedia ->
  IO (Either ConnectionError DiskResult)
diskAttach conn vmId diskId interface media =
  handleDiskResponse <$> sendRequest conn (ReqDiskAttach vmId diskId interface media)

-- | Detach a disk from a VM
diskDetach :: Connection -> Int64 -> Int64 -> IO (Either ConnectionError DiskResult)
diskDetach conn vmId driveId =
  handleDiskResponse <$> sendRequest conn (ReqDiskDetach vmId driveId)

--------------------------------------------------------------------------------
-- Snapshot Operations
--------------------------------------------------------------------------------

-- | Result of a snapshot operation
data SnapshotResult
  = -- | Operation succeeded
    SnapshotOk
  | -- | Snapshot created with new ID
    SnapshotCreated !Int64
  | -- | List of snapshots
    SnapshotListResult ![SnapshotInfo]
  | -- | Snapshot not found
    SnapshotNotFound
  | -- | Disk not found
    SnapshotDiskNotFound
  | -- | VM must be stopped
    SnapshotVmMustBeStopped
  | -- | Format not supported
    SnapshotFormatNotSupported !Text
  | -- | Error with message
    SnapshotError !Text
  deriving (Eq, Show)

-- | Helper for snapshot operation responses
handleSnapshotResponse :: Either ConnectionError Response -> Either ConnectionError SnapshotResult
handleSnapshotResponse result = case result of
  Left err -> Left err
  Right RespSnapshotOk -> Right SnapshotOk
  Right (RespSnapshotCreated snapId) -> Right $ SnapshotCreated snapId
  Right (RespSnapshotList snaps) -> Right $ SnapshotListResult snaps
  Right RespSnapshotNotFound -> Right SnapshotNotFound
  Right RespDiskNotFound -> Right SnapshotDiskNotFound
  Right RespVmMustBeStopped -> Right SnapshotVmMustBeStopped
  Right (RespFormatNotSupported msg) -> Right $ SnapshotFormatNotSupported msg
  Right (RespError msg) -> Left $ ServerError msg
  Right _ -> Left $ DecodeFailed "Unexpected response"

-- | Create a snapshot
snapshotCreate :: Connection -> Int64 -> Text -> IO (Either ConnectionError SnapshotResult)
snapshotCreate conn diskId name =
  handleSnapshotResponse <$> sendRequest conn (ReqSnapshotCreate diskId name)

-- | Delete a snapshot
snapshotDelete :: Connection -> Int64 -> Int64 -> IO (Either ConnectionError SnapshotResult)
snapshotDelete conn diskId snapshotId =
  handleSnapshotResponse <$> sendRequest conn (ReqSnapshotDelete diskId snapshotId)

-- | Rollback to a snapshot
snapshotRollback :: Connection -> Int64 -> Int64 -> IO (Either ConnectionError SnapshotResult)
snapshotRollback conn diskId snapshotId =
  handleSnapshotResponse <$> sendRequest conn (ReqSnapshotRollback diskId snapshotId)

-- | Merge a snapshot
snapshotMerge :: Connection -> Int64 -> Int64 -> IO (Either ConnectionError SnapshotResult)
snapshotMerge conn diskId snapshotId =
  handleSnapshotResponse <$> sendRequest conn (ReqSnapshotMerge diskId snapshotId)

-- | List snapshots for a disk
snapshotList :: Connection -> Int64 -> IO (Either ConnectionError SnapshotResult)
snapshotList conn diskId =
  handleSnapshotResponse <$> sendRequest conn (ReqSnapshotList diskId)
