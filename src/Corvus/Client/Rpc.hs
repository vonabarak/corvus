{-# LANGUAGE OverloadedStrings #-}

-- | RPC call functions for the Corvus client.
module Corvus.Client.Rpc
  ( -- * Basic commands
    sendPing
  , getStatus
  , requestShutdown

    -- * VM listing
  , listVms
  , showVm

    -- * VM lifecycle
  , VmCreateResult (..)
  , VmDeleteResult (..)
  , vmCreate
  , vmDelete

    -- * VM actions
  , VmActionResult (..)
  , vmStart
  , vmStop
  , vmPause
  , vmReset

    -- * VM edit
  , VmEditResult (..)
  , vmEdit

    -- * Disk operations
  , DiskResult (..)
  , diskCreate
  , diskCreateOverlay
  , diskRegister
  , diskDelete
  , diskResize
  , diskClone
  , diskList
  , diskShow
  , diskAttach
  , diskDetach

    -- * Snapshot operations
  , SnapshotResult (..)
  , snapshotCreate
  , snapshotDelete
  , snapshotRollback
  , snapshotMerge
  , snapshotList

    -- * Shared directory operations
  , SharedDirResult (..)
  , sharedDirAdd
  , sharedDirRemove
  , sharedDirList

    -- * Network interface operations
  , NetIfResult (..)
  , netIfAdd
  , netIfRemove
  , netIfList

    -- * SSH key operations
  , SshKeyResult (..)
  , sshKeyCreate
  , sshKeyDelete
  , sshKeyList
  , sshKeyAttach
  , sshKeyDetach
  , sshKeyListForVm

    -- * Template operations
  , TemplateResult (..)
  , templateCreate
  , templateDelete
  , templateList
  , templateShow
  , templateInstantiate

    -- * Virtual network operations
  , NetworkResult (..)
  , networkCreate
  , networkDelete
  , networkStart
  , networkStop
  , networkList
  , networkShow
  )
where

import Corvus.Client.Connection
import Corvus.Model (CacheType, DriveFormat, DriveInterface, DriveMedia, NetInterfaceType, SharedDirCache, VmStatus)
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

--------------------------------------------------------------------------------
-- VM Lifecycle
--------------------------------------------------------------------------------

-- | Result of VM create
data VmCreateResult
  = VmCreated !Int64
  | VmCreateError !Text
  deriving (Eq, Show)

-- | Result of VM delete
data VmDeleteResult
  = VmDeleted
  | VmDeleteNotFound
  | VmDeleteRunning
  | VmDeleteError !Text
  deriving (Eq, Show)

-- | Create a new VM
vmCreate :: Connection -> Text -> Int -> Int -> Maybe Text -> Bool -> IO (Either ConnectionError VmCreateResult)
vmCreate conn name cpuCount ramMb description headless = do
  result <- sendRequest conn (ReqVmCreate name cpuCount ramMb description headless)
  case result of
    Left err -> pure $ Left err
    Right (RespVmCreated vmId) -> pure $ Right $ VmCreated vmId
    Right (RespError msg) -> pure $ Right $ VmCreateError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Delete a VM
vmDelete :: Connection -> Int64 -> IO (Either ConnectionError VmDeleteResult)
vmDelete conn vmId = do
  result <- sendRequest conn (ReqVmDelete vmId)
  case result of
    Left err -> pure $ Left err
    Right RespVmDeleted -> pure $ Right VmDeleted
    Right RespVmNotFound -> pure $ Right VmDeleteNotFound
    Right RespVmRunning -> pure $ Right VmDeleteRunning
    Right (RespError msg) -> pure $ Right $ VmDeleteError msg
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
-- VM Edit
--------------------------------------------------------------------------------

-- | Result of VM edit
data VmEditResult
  = VmEdited
  | VmEditNotFound
  | VmEditMustBeStopped
  | VmEditError !Text
  deriving (Eq, Show)

-- | Edit VM properties. Only provided (Just) fields are updated.
vmEdit
  :: Connection
  -> Int64
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> Maybe Bool
  -> IO (Either ConnectionError VmEditResult)
vmEdit conn vmId mCpus mRam mDesc mHeadless = do
  result <- sendRequest conn (ReqVmEdit vmId mCpus mRam mDesc mHeadless)
  case result of
    Left err -> pure $ Left err
    Right RespVmEdited -> pure $ Right VmEdited
    Right RespVmNotFound -> pure $ Right VmEditNotFound
    Right RespVmMustBeStopped -> pure $ Right VmEditMustBeStopped
    Right (RespError msg) -> pure $ Right $ VmEditError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

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
  | -- | Disk has overlay images depending on it
    DiskHasOverlays ![Int64]
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
  Right (RespDiskHasOverlays overlayIds) -> Right $ DiskHasOverlays overlayIds
  Right RespVmMustBeStopped -> Right VmMustBeStopped
  Right (RespFormatNotSupported msg) -> Right $ FormatNotSupported msg
  Right RespVmNotFound -> Right DiskVmNotFound
  Right (RespError msg) -> Right $ DiskError msg
  Right _ -> Left $ DecodeFailed "Unexpected response"

-- | Create a new disk image
diskCreate :: Connection -> Text -> DriveFormat -> Int64 -> IO (Either ConnectionError DiskResult)
diskCreate conn name format sizeMb =
  handleDiskResponse <$> sendRequest conn (ReqDiskCreate name format sizeMb)

-- | Create an overlay disk image backed by an existing disk
diskCreateOverlay :: Connection -> Text -> Int64 -> IO (Either ConnectionError DiskResult)
diskCreateOverlay conn name baseDiskId =
  handleDiskResponse <$> sendRequest conn (ReqDiskCreateOverlay name baseDiskId)

-- | Register an existing disk image file
diskRegister
  :: Connection
  -> Text
  -> Text
  -> DriveFormat
  -> Maybe Int64
  -> IO (Either ConnectionError DiskResult)
diskRegister conn name filePath format sizeMb =
  handleDiskResponse <$> sendRequest conn (ReqDiskRegister name filePath format sizeMb)

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

-- | Clone a disk image
diskClone :: Connection -> Text -> Int64 -> Maybe Text -> IO (Either ConnectionError DiskResult)
diskClone conn name baseDiskId optionalPath =
  handleDiskResponse <$> sendRequest conn (ReqDiskClone name baseDiskId optionalPath)

-- | Attach a disk to a VM
diskAttach
  :: Connection
  -> Int64
  -> Int64
  -> DriveInterface
  -> Maybe DriveMedia
  -> Bool
  -> Bool
  -> CacheType
  -> IO (Either ConnectionError DiskResult)
diskAttach conn vmId diskId interface media readOnly discard cache =
  handleDiskResponse <$> sendRequest conn (ReqDiskAttach vmId diskId interface media readOnly discard cache)

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
  Right (RespError msg) -> Right $ SnapshotError msg
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

--------------------------------------------------------------------------------
-- Shared Directory Operations
--------------------------------------------------------------------------------

-- | Result of a shared directory operation
data SharedDirResult
  = -- | Operation succeeded
    SharedDirOk
  | -- | Shared directory added with new ID
    SharedDirAdded !Int64
  | -- | List of shared directories
    SharedDirListResult ![SharedDirInfo]
  | -- | Shared directory not found
    SharedDirNotFound
  | -- | VM not found
    SharedDirVmNotFound
  | -- | VM must be stopped for this operation
    SharedDirVmMustBeStopped
  | -- | Error with message
    SharedDirError !Text
  deriving (Eq, Show)

-- | Helper for shared directory operation responses
handleSharedDirResponse :: Either ConnectionError Response -> Either ConnectionError SharedDirResult
handleSharedDirResponse result = case result of
  Left err -> Left err
  Right RespSharedDirOk -> Right SharedDirOk
  Right (RespSharedDirAdded dirId) -> Right $ SharedDirAdded dirId
  Right (RespSharedDirList dirs) -> Right $ SharedDirListResult dirs
  Right RespSharedDirNotFound -> Right SharedDirNotFound
  Right RespVmNotFound -> Right SharedDirVmNotFound
  Right RespVmMustBeStopped -> Right SharedDirVmMustBeStopped
  Right (RespError msg) -> Right $ SharedDirError msg
  Right _ -> Left $ DecodeFailed "Unexpected response"

-- | Add a shared directory to a VM
sharedDirAdd
  :: Connection
  -> Int64
  -> Text
  -> Text
  -> SharedDirCache
  -> Bool
  -> IO (Either ConnectionError SharedDirResult)
sharedDirAdd conn vmId path tag cache readOnly =
  handleSharedDirResponse <$> sendRequest conn (ReqSharedDirAdd vmId path tag cache readOnly)

-- | Remove a shared directory from a VM
sharedDirRemove :: Connection -> Int64 -> Int64 -> IO (Either ConnectionError SharedDirResult)
sharedDirRemove conn vmId sharedDirId =
  handleSharedDirResponse <$> sendRequest conn (ReqSharedDirRemove vmId sharedDirId)

-- | List shared directories for a VM
sharedDirList :: Connection -> Int64 -> IO (Either ConnectionError SharedDirResult)
sharedDirList conn vmId =
  handleSharedDirResponse <$> sendRequest conn (ReqSharedDirList vmId)

--------------------------------------------------------------------------------
-- Network Interface Operations
--------------------------------------------------------------------------------

-- | Result of a network interface operation
data NetIfResult
  = -- | Operation succeeded
    NetIfOk
  | -- | Network interface added with new ID
    NetIfAdded !Int64
  | -- | List of network interfaces
    NetIfListResult ![NetIfInfo]
  | -- | Network interface not found
    NetIfNotFound
  | -- | VM not found
    NetIfVmNotFound
  | -- | Error with message
    NetIfError !Text
  deriving (Eq, Show)

-- | Helper for network interface operation responses
handleNetIfResponse :: Either ConnectionError Response -> Either ConnectionError NetIfResult
handleNetIfResponse result = case result of
  Left err -> Left err
  Right RespNetIfOk -> Right NetIfOk
  Right (RespNetIfAdded netIfId) -> Right $ NetIfAdded netIfId
  Right (RespNetIfList netIfs) -> Right $ NetIfListResult netIfs
  Right RespNetIfNotFound -> Right NetIfNotFound
  Right RespVmNotFound -> Right NetIfVmNotFound
  Right (RespError msg) -> Right $ NetIfError msg
  Right _ -> Left $ DecodeFailed "Unexpected response"

-- | Add a network interface to a VM
netIfAdd
  :: Connection
  -> Int64
  -> NetInterfaceType
  -> Text
  -> Maybe Text
  -> Maybe Int64
  -> IO (Either ConnectionError NetIfResult)
netIfAdd conn vmId ifaceType hostDevice macAddress mNetworkId =
  handleNetIfResponse <$> sendRequest conn (ReqNetIfAdd vmId ifaceType hostDevice macAddress mNetworkId)

-- | Remove a network interface from a VM
netIfRemove :: Connection -> Int64 -> Int64 -> IO (Either ConnectionError NetIfResult)
netIfRemove conn vmId netIfId =
  handleNetIfResponse <$> sendRequest conn (ReqNetIfRemove vmId netIfId)

-- | List network interfaces for a VM
netIfList :: Connection -> Int64 -> IO (Either ConnectionError NetIfResult)
netIfList conn vmId =
  handleNetIfResponse <$> sendRequest conn (ReqNetIfList vmId)

--------------------------------------------------------------------------------
-- SSH Key Operations
--------------------------------------------------------------------------------

-- | Result of an SSH key operation
data SshKeyResult
  = -- | Operation succeeded
    SshKeyOk
  | -- | SSH key created with new ID
    SshKeyCreated !Int64
  | -- | List of SSH keys
    SshKeyListResult ![SshKeyInfo]
  | -- | SSH key not found
    SshKeyNotFound
  | -- | SSH key is in use by VMs
    SshKeyInUse ![Int64]
  | -- | VM not found
    SshKeyVmNotFound
  | -- | Error with message
    SshKeyError !Text
  deriving (Eq, Show)

-- | Helper for SSH key operation responses
handleSshKeyResponse :: Either ConnectionError Response -> Either ConnectionError SshKeyResult
handleSshKeyResponse result = case result of
  Left err -> Left err
  Right RespSshKeyOk -> Right SshKeyOk
  Right (RespSshKeyCreated keyId) -> Right $ SshKeyCreated keyId
  Right (RespSshKeyList keys) -> Right $ SshKeyListResult keys
  Right RespSshKeyNotFound -> Right SshKeyNotFound
  Right (RespSshKeyInUse vmIds) -> Right $ SshKeyInUse vmIds
  Right RespVmNotFound -> Right SshKeyVmNotFound
  Right (RespError msg) -> Right $ SshKeyError msg
  Right _ -> Left $ DecodeFailed "Unexpected response"

-- | Create a new SSH key
sshKeyCreate :: Connection -> Text -> Text -> IO (Either ConnectionError SshKeyResult)
sshKeyCreate conn name publicKey =
  handleSshKeyResponse <$> sendRequest conn (ReqSshKeyCreate name publicKey)

-- | Delete an SSH key
sshKeyDelete :: Connection -> Int64 -> IO (Either ConnectionError SshKeyResult)
sshKeyDelete conn keyId =
  handleSshKeyResponse <$> sendRequest conn (ReqSshKeyDelete keyId)

-- | List all SSH keys
sshKeyList :: Connection -> IO (Either ConnectionError SshKeyResult)
sshKeyList conn =
  handleSshKeyResponse <$> sendRequest conn ReqSshKeyList

-- | Attach an SSH key to a VM
sshKeyAttach :: Connection -> Int64 -> Int64 -> IO (Either ConnectionError SshKeyResult)
sshKeyAttach conn vmId keyId =
  handleSshKeyResponse <$> sendRequest conn (ReqSshKeyAttach vmId keyId)

-- | Detach an SSH key from a VM
sshKeyDetach :: Connection -> Int64 -> Int64 -> IO (Either ConnectionError SshKeyResult)
sshKeyDetach conn vmId keyId =
  handleSshKeyResponse <$> sendRequest conn (ReqSshKeyDetach vmId keyId)

-- | List SSH keys for a VM
sshKeyListForVm :: Connection -> Int64 -> IO (Either ConnectionError SshKeyResult)
sshKeyListForVm conn vmId =
  handleSshKeyResponse <$> sendRequest conn (ReqSshKeyListForVm vmId)

--------------------------------------------------------------------------------
-- Template Operations
--------------------------------------------------------------------------------

-- | Result of a template operation
data TemplateResult
  = -- | Template created with new ID
    TemplateCreated !Int64
  | -- | List of templates
    TemplateListResult ![TemplateVmInfo]
  | -- | Single template details
    TemplateDetailsResult !TemplateDetails
  | -- | Template deleted
    TemplateDeleted
  | -- | VM instantiated from template (new VM ID)
    TemplateInstantiated !Int64
  | -- | Template not found
    TemplateNotFound
  | -- | Error with message
    TemplateError !Text
  deriving (Eq, Show)

-- | Helper for template operation responses
handleTemplateResponse :: Either ConnectionError Response -> Either ConnectionError TemplateResult
handleTemplateResponse result = case result of
  Left err -> Left err
  Right (RespTemplateCreated tid) -> Right $ TemplateCreated tid
  Right (RespTemplateList templates) -> Right $ TemplateListResult templates
  Right (RespTemplateInfo details) -> Right $ TemplateDetailsResult details
  Right RespTemplateDeleted -> Right TemplateDeleted
  Right (RespTemplateInstantiated vmId) -> Right $ TemplateInstantiated vmId
  Right RespTemplateNotFound -> Right TemplateNotFound
  Right (RespError msg) -> Right $ TemplateError msg
  Right _ -> Left $ DecodeFailed "Unexpected response"

-- | Create a template from YAML
templateCreate :: Connection -> Text -> IO (Either ConnectionError TemplateResult)
templateCreate conn yaml =
  handleTemplateResponse <$> sendRequest conn (ReqTemplateCreate yaml)

-- | Delete a template
templateDelete :: Connection -> Int64 -> IO (Either ConnectionError TemplateResult)
templateDelete conn tid =
  handleTemplateResponse <$> sendRequest conn (ReqTemplateDelete tid)

-- | List all templates
templateList :: Connection -> IO (Either ConnectionError TemplateResult)
templateList conn =
  handleTemplateResponse <$> sendRequest conn ReqTemplateList

-- | Show template details
templateShow :: Connection -> Int64 -> IO (Either ConnectionError TemplateResult)
templateShow conn tid =
  handleTemplateResponse <$> sendRequest conn (ReqTemplateShow tid)

-- | Instantiate a template
templateInstantiate :: Connection -> Int64 -> Text -> IO (Either ConnectionError TemplateResult)
templateInstantiate conn tid newVmName =
  handleTemplateResponse <$> sendRequest conn (ReqTemplateInstantiate tid newVmName)

--------------------------------------------------------------------------------
-- Virtual Network Operations
--------------------------------------------------------------------------------

-- | Result of a virtual network operation
data NetworkResult
  = -- | Network created with new ID
    NetworkCreated !Int64
  | -- | Network deleted
    NetworkDeleted
  | -- | Network started
    NetworkStarted
  | -- | Network stopped
    NetworkStopped
  | -- | List of networks
    NetworkListResult ![NetworkInfo]
  | -- | Single network details
    NetworkDetails !NetworkInfo
  | -- | Network not found
    NetworkNotFound
  | -- | Network is already running
    NetworkAlreadyRunning
  | -- | Network is not running
    NetworkNotRunning
  | -- | Network is in use
    NetworkInUse
  | -- | Error with message
    NetworkError !Text
  deriving (Eq, Show)

-- | Helper for network operation responses
handleNetworkResponse :: Either ConnectionError Response -> Either ConnectionError NetworkResult
handleNetworkResponse result = case result of
  Left err -> Left err
  Right (RespNetworkCreated nwId) -> Right $ NetworkCreated nwId
  Right RespNetworkDeleted -> Right NetworkDeleted
  Right RespNetworkStarted -> Right NetworkStarted
  Right RespNetworkStopped -> Right NetworkStopped
  Right (RespNetworkList networks) -> Right $ NetworkListResult networks
  Right (RespNetworkDetails info) -> Right $ NetworkDetails info
  Right RespNetworkNotFound -> Right NetworkNotFound
  Right RespNetworkAlreadyRunning -> Right NetworkAlreadyRunning
  Right RespNetworkNotRunning -> Right NetworkNotRunning
  Right RespNetworkInUse -> Right NetworkInUse
  Right (RespNetworkError msg) -> Right $ NetworkError msg
  Right (RespError msg) -> Right $ NetworkError msg
  Right _ -> Left $ DecodeFailed "Unexpected response"

-- | Create a virtual network
networkCreate :: Connection -> Text -> Text -> IO (Either ConnectionError NetworkResult)
networkCreate conn name subnet =
  handleNetworkResponse <$> sendRequest conn (ReqNetworkCreate name subnet)

-- | Delete a virtual network
networkDelete :: Connection -> Int64 -> IO (Either ConnectionError NetworkResult)
networkDelete conn nwId =
  handleNetworkResponse <$> sendRequest conn (ReqNetworkDelete nwId)

-- | Start a virtual network
networkStart :: Connection -> Int64 -> IO (Either ConnectionError NetworkResult)
networkStart conn nwId =
  handleNetworkResponse <$> sendRequest conn (ReqNetworkStart nwId)

-- | Stop a virtual network
networkStop :: Connection -> Int64 -> Bool -> IO (Either ConnectionError NetworkResult)
networkStop conn nwId force =
  handleNetworkResponse <$> sendRequest conn (ReqNetworkStop nwId force)

-- | List all virtual networks
networkList :: Connection -> IO (Either ConnectionError NetworkResult)
networkList conn =
  handleNetworkResponse <$> sendRequest conn ReqNetworkList

-- | Show virtual network details
networkShow :: Connection -> Int64 -> IO (Either ConnectionError NetworkResult)
networkShow conn nwId =
  handleNetworkResponse <$> sendRequest conn (ReqNetworkShow nwId)
