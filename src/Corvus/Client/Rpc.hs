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
  , vmCloudInit

    -- * Guest execution
  , GuestExecResult (..)
  , vmExec

    -- * Disk operations
  , DiskResult (..)
  , diskCreate
  , diskCreateOverlay
  , diskRegister
  , diskRefresh
  , diskImportUrl
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
  , networkEdit

    -- * Apply operations
  , ApplyRpcResult (..)
  , applyConfig

    -- * Cloud-init config operations
  , CloudInitResult (..)
  , cloudInitSet
  , cloudInitGet
  , cloudInitDelete

    -- * Task history operations
  , taskList
  , taskShow
  , taskListChildren
  )
where

import Corvus.Client.Connection
import Corvus.Model (CacheType, DriveFormat, DriveInterface, DriveMedia, NetInterfaceType, SharedDirCache, TaskResult, TaskSubsystem, VmStatus)
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
showVm :: Connection -> Text -> IO (Either ConnectionError (Maybe VmDetails))
showVm conn vmRef = do
  result <- sendRequest conn (ReqShowVm (Ref vmRef))
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
vmCreate :: Connection -> Text -> Int -> Int -> Maybe Text -> Bool -> Bool -> Bool -> Bool -> IO (Either ConnectionError VmCreateResult)
vmCreate conn name cpuCount ramMb description headless guestAgent cloudInit autostart = do
  result <- sendRequest conn (ReqVmCreate name cpuCount ramMb description headless guestAgent cloudInit autostart)
  case result of
    Left err -> pure $ Left err
    Right (RespVmCreated vmId) -> pure $ Right $ VmCreated vmId
    Right (RespError msg) -> pure $ Right $ VmCreateError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Delete a VM
vmDelete :: Connection -> Text -> IO (Either ConnectionError VmDeleteResult)
vmDelete conn vmRef = do
  result <- sendRequest conn (ReqVmDelete (Ref vmRef))
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
vmStart :: Connection -> Text -> Bool -> IO (Either ConnectionError VmActionResult)
vmStart conn vmRef wait = handleVmActionResponse <$> sendRequest conn (ReqVmStart (Ref vmRef) wait)

-- | Stop a VM (running -> stopped)
vmStop :: Connection -> Text -> Bool -> IO (Either ConnectionError VmActionResult)
vmStop conn vmRef wait = handleVmActionResponse <$> sendRequest conn (ReqVmStop (Ref vmRef) wait)

-- | Pause a VM (running -> paused)
vmPause :: Connection -> Text -> IO (Either ConnectionError VmActionResult)
vmPause conn vmRef = handleVmActionResponse <$> sendRequest conn (ReqVmPause (Ref vmRef))

-- | Reset a VM (any -> stopped)
vmReset :: Connection -> Text -> IO (Either ConnectionError VmActionResult)
vmReset conn vmRef = handleVmActionResponse <$> sendRequest conn (ReqVmReset (Ref vmRef))

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
  -> Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> IO (Either ConnectionError VmEditResult)
vmEdit conn vmRef mCpus mRam mDesc mHeadless mGuestAgent mCloudInit mAutostart = do
  result <- sendRequest conn (ReqVmEdit (Ref vmRef) mCpus mRam mDesc mHeadless mGuestAgent mCloudInit mAutostart)
  case result of
    Left err -> pure $ Left err
    Right RespVmEdited -> pure $ Right VmEdited
    Right RespVmNotFound -> pure $ Right VmEditNotFound
    Right RespVmMustBeStopped -> pure $ Right VmEditMustBeStopped
    Right (RespError msg) -> pure $ Right $ VmEditError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Generate/regenerate cloud-init ISO for a VM
vmCloudInit :: Connection -> Text -> IO (Either ConnectionError VmEditResult)
vmCloudInit conn vmRef = do
  result <- sendRequest conn (ReqVmCloudInit (Ref vmRef))
  case result of
    Left err -> pure $ Left err
    Right RespVmEdited -> pure $ Right VmEdited
    Right RespVmNotFound -> pure $ Right VmEditNotFound
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
    DiskInUse ![(Int64, Text)]
  | -- | Disk has overlay images depending on it
    DiskHasOverlays ![(Int64, Text)]
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
diskCreate :: Connection -> Text -> DriveFormat -> Int64 -> Maybe Text -> IO (Either ConnectionError DiskResult)
diskCreate conn name format sizeMb mPath =
  handleDiskResponse <$> sendRequest conn (ReqDiskCreate name format sizeMb mPath)

-- | Create an overlay disk image backed by an existing disk
diskCreateOverlay :: Connection -> Text -> Text -> Maybe Text -> IO (Either ConnectionError DiskResult)
diskCreateOverlay conn name baseDiskRef optDirPath =
  handleDiskResponse <$> sendRequest conn (ReqDiskCreateOverlay name (Ref baseDiskRef) optDirPath)

-- | Register an existing disk image file
diskRegister
  :: Connection
  -> Text
  -> Text
  -> Maybe DriveFormat
  -> IO (Either ConnectionError DiskResult)
diskRegister conn name filePath mFormat =
  handleDiskResponse <$> sendRequest conn (ReqDiskRegister name filePath mFormat)

-- | Refresh a disk image's size from qemu-img info
diskRefresh :: Connection -> Text -> IO (Either ConnectionError DiskResult)
diskRefresh conn diskRef =
  handleDiskResponse <$> sendRequest conn (ReqDiskRefresh (Ref diskRef))

-- | Import a disk image from an HTTP/HTTPS URL
diskImportUrl :: Connection -> Text -> Text -> Maybe Text -> IO (Either ConnectionError DiskResult)
diskImportUrl conn name url mFormat =
  handleDiskResponse <$> sendRequest conn (ReqDiskImportUrl name url mFormat)

-- | Delete a disk image
diskDelete :: Connection -> Text -> IO (Either ConnectionError DiskResult)
diskDelete conn diskRef = handleDiskResponse <$> sendRequest conn (ReqDiskDelete (Ref diskRef))

-- | Resize a disk image
diskResize :: Connection -> Text -> Int64 -> IO (Either ConnectionError DiskResult)
diskResize conn diskRef newSizeMb =
  handleDiskResponse <$> sendRequest conn (ReqDiskResize (Ref diskRef) newSizeMb)

-- | List all disk images
diskList :: Connection -> IO (Either ConnectionError DiskResult)
diskList conn = handleDiskResponse <$> sendRequest conn ReqDiskList

-- | Show disk image details
diskShow :: Connection -> Text -> IO (Either ConnectionError DiskResult)
diskShow conn diskRef = handleDiskResponse <$> sendRequest conn (ReqDiskShow (Ref diskRef))

-- | Clone a disk image
diskClone :: Connection -> Text -> Text -> Maybe Text -> IO (Either ConnectionError DiskResult)
diskClone conn name baseDiskRef optionalPath =
  handleDiskResponse <$> sendRequest conn (ReqDiskClone name (Ref baseDiskRef) optionalPath)

-- | Attach a disk to a VM
diskAttach
  :: Connection
  -> Text
  -> Text
  -> DriveInterface
  -> Maybe DriveMedia
  -> Bool
  -> Bool
  -> CacheType
  -> IO (Either ConnectionError DiskResult)
diskAttach conn vmRef diskRef interface media readOnly discard cache =
  handleDiskResponse <$> sendRequest conn (ReqDiskAttach (Ref vmRef) (Ref diskRef) interface media readOnly discard cache)

-- | Detach a disk from a VM
diskDetach :: Connection -> Text -> Text -> IO (Either ConnectionError DiskResult)
diskDetach conn vmRef driveRef =
  handleDiskResponse <$> sendRequest conn (ReqDiskDetach (Ref vmRef) (Ref driveRef))

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
snapshotCreate :: Connection -> Text -> Text -> IO (Either ConnectionError SnapshotResult)
snapshotCreate conn diskRef name =
  handleSnapshotResponse <$> sendRequest conn (ReqSnapshotCreate (Ref diskRef) name)

-- | Delete a snapshot
snapshotDelete :: Connection -> Text -> Text -> IO (Either ConnectionError SnapshotResult)
snapshotDelete conn diskRef snapshotRef =
  handleSnapshotResponse <$> sendRequest conn (ReqSnapshotDelete (Ref diskRef) (Ref snapshotRef))

-- | Rollback to a snapshot
snapshotRollback :: Connection -> Text -> Text -> IO (Either ConnectionError SnapshotResult)
snapshotRollback conn diskRef snapshotRef =
  handleSnapshotResponse <$> sendRequest conn (ReqSnapshotRollback (Ref diskRef) (Ref snapshotRef))

-- | Merge a snapshot
snapshotMerge :: Connection -> Text -> Text -> IO (Either ConnectionError SnapshotResult)
snapshotMerge conn diskRef snapshotRef =
  handleSnapshotResponse <$> sendRequest conn (ReqSnapshotMerge (Ref diskRef) (Ref snapshotRef))

-- | List snapshots for a disk
snapshotList :: Connection -> Text -> IO (Either ConnectionError SnapshotResult)
snapshotList conn diskRef =
  handleSnapshotResponse <$> sendRequest conn (ReqSnapshotList (Ref diskRef))

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
  -> Text
  -> Text
  -> Text
  -> SharedDirCache
  -> Bool
  -> IO (Either ConnectionError SharedDirResult)
sharedDirAdd conn vmRef path tag cache readOnly =
  handleSharedDirResponse <$> sendRequest conn (ReqSharedDirAdd (Ref vmRef) path tag cache readOnly)

-- | Remove a shared directory from a VM
sharedDirRemove :: Connection -> Text -> Text -> IO (Either ConnectionError SharedDirResult)
sharedDirRemove conn vmRef sharedDirRef =
  handleSharedDirResponse <$> sendRequest conn (ReqSharedDirRemove (Ref vmRef) (Ref sharedDirRef))

-- | List shared directories for a VM
sharedDirList :: Connection -> Text -> IO (Either ConnectionError SharedDirResult)
sharedDirList conn vmRef =
  handleSharedDirResponse <$> sendRequest conn (ReqSharedDirList (Ref vmRef))

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
  -> Text
  -> NetInterfaceType
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> IO (Either ConnectionError NetIfResult)
netIfAdd conn vmRef ifaceType hostDevice macAddress mNetworkRef =
  handleNetIfResponse <$> sendRequest conn (ReqNetIfAdd (Ref vmRef) ifaceType hostDevice macAddress (Ref <$> mNetworkRef))

-- | Remove a network interface from a VM
netIfRemove :: Connection -> Text -> Int64 -> IO (Either ConnectionError NetIfResult)
netIfRemove conn vmRef netIfId =
  handleNetIfResponse <$> sendRequest conn (ReqNetIfRemove (Ref vmRef) netIfId)

-- | List network interfaces for a VM
netIfList :: Connection -> Text -> IO (Either ConnectionError NetIfResult)
netIfList conn vmRef =
  handleNetIfResponse <$> sendRequest conn (ReqNetIfList (Ref vmRef))

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
    SshKeyInUse ![(Int64, Text)]
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
sshKeyDelete :: Connection -> Text -> IO (Either ConnectionError SshKeyResult)
sshKeyDelete conn keyRef =
  handleSshKeyResponse <$> sendRequest conn (ReqSshKeyDelete (Ref keyRef))

-- | List all SSH keys
sshKeyList :: Connection -> IO (Either ConnectionError SshKeyResult)
sshKeyList conn =
  handleSshKeyResponse <$> sendRequest conn ReqSshKeyList

-- | Attach an SSH key to a VM
sshKeyAttach :: Connection -> Text -> Text -> IO (Either ConnectionError SshKeyResult)
sshKeyAttach conn vmRef keyRef =
  handleSshKeyResponse <$> sendRequest conn (ReqSshKeyAttach (Ref vmRef) (Ref keyRef))

-- | Detach an SSH key from a VM
sshKeyDetach :: Connection -> Text -> Text -> IO (Either ConnectionError SshKeyResult)
sshKeyDetach conn vmRef keyRef =
  handleSshKeyResponse <$> sendRequest conn (ReqSshKeyDetach (Ref vmRef) (Ref keyRef))

-- | List SSH keys for a VM
sshKeyListForVm :: Connection -> Text -> IO (Either ConnectionError SshKeyResult)
sshKeyListForVm conn vmRef =
  handleSshKeyResponse <$> sendRequest conn (ReqSshKeyListForVm (Ref vmRef))

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
templateDelete :: Connection -> Text -> IO (Either ConnectionError TemplateResult)
templateDelete conn templateRef =
  handleTemplateResponse <$> sendRequest conn (ReqTemplateDelete (Ref templateRef))

-- | List all templates
templateList :: Connection -> IO (Either ConnectionError TemplateResult)
templateList conn =
  handleTemplateResponse <$> sendRequest conn ReqTemplateList

-- | Show template details
templateShow :: Connection -> Text -> IO (Either ConnectionError TemplateResult)
templateShow conn templateRef =
  handleTemplateResponse <$> sendRequest conn (ReqTemplateShow (Ref templateRef))

-- | Instantiate a template
templateInstantiate :: Connection -> Text -> Text -> IO (Either ConnectionError TemplateResult)
templateInstantiate conn templateRef newVmName =
  handleTemplateResponse <$> sendRequest conn (ReqTemplateInstantiate (Ref templateRef) newVmName)

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
  | -- | Network edited
    NetworkEdited
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
  Right RespNetworkEdited -> Right NetworkEdited
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
networkCreate :: Connection -> Text -> Text -> Bool -> Bool -> Bool -> IO (Either ConnectionError NetworkResult)
networkCreate conn name subnet dhcp nat autostart =
  handleNetworkResponse <$> sendRequest conn (ReqNetworkCreate name subnet dhcp nat autostart)

-- | Delete a virtual network
networkDelete :: Connection -> Text -> IO (Either ConnectionError NetworkResult)
networkDelete conn networkRef =
  handleNetworkResponse <$> sendRequest conn (ReqNetworkDelete (Ref networkRef))

-- | Start a virtual network
networkStart :: Connection -> Text -> IO (Either ConnectionError NetworkResult)
networkStart conn networkRef =
  handleNetworkResponse <$> sendRequest conn (ReqNetworkStart (Ref networkRef))

-- | Stop a virtual network
networkStop :: Connection -> Text -> Bool -> IO (Either ConnectionError NetworkResult)
networkStop conn networkRef force =
  handleNetworkResponse <$> sendRequest conn (ReqNetworkStop (Ref networkRef) force)

-- | List all virtual networks
networkList :: Connection -> IO (Either ConnectionError NetworkResult)
networkList conn =
  handleNetworkResponse <$> sendRequest conn ReqNetworkList

-- | Show virtual network details
networkShow :: Connection -> Text -> IO (Either ConnectionError NetworkResult)
networkShow conn networkRef =
  handleNetworkResponse <$> sendRequest conn (ReqNetworkShow (Ref networkRef))

-- | Edit virtual network properties
networkEdit :: Connection -> Text -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO (Either ConnectionError NetworkResult)
networkEdit conn networkRef mSubnet mDhcp mNat mAutostart =
  handleNetworkResponse <$> sendRequest conn (ReqNetworkEdit (Ref networkRef) mSubnet mDhcp mNat mAutostart)

--------------------------------------------------------------------------------
-- Guest Execution
--------------------------------------------------------------------------------

-- | Result of guest command execution
data GuestExecResult
  = -- | Command succeeded (exitcode, stdout, stderr)
    GuestExecOk !Int !Text !Text
  | -- | VM not found
    GuestExecVmNotFound
  | -- | Guest agent not enabled
    GuestExecNotEnabled
  | -- | VM is not running
    GuestExecInvalidState !VmStatus !Text
  | -- | Guest agent error
    GuestExecAgentError !Text
  deriving (Eq, Show)

-- | Execute a command inside a VM via guest agent
vmExec :: Connection -> Text -> Text -> IO (Either ConnectionError GuestExecResult)
vmExec conn vmRef command = do
  result <- sendRequest conn (ReqGuestExec (Ref vmRef) command)
  case result of
    Left err -> pure $ Left err
    Right (RespGuestExecResult exitcode stdout stderr) -> pure $ Right $ GuestExecOk exitcode stdout stderr
    Right RespVmNotFound -> pure $ Right GuestExecVmNotFound
    Right RespGuestAgentNotEnabled -> pure $ Right GuestExecNotEnabled
    Right (RespInvalidTransition status msg) -> pure $ Right $ GuestExecInvalidState status msg
    Right (RespGuestAgentError msg) -> pure $ Right $ GuestExecAgentError msg
    Right (RespError msg) -> pure $ Left $ ServerError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

--------------------------------------------------------------------------------
-- Apply Operations
--------------------------------------------------------------------------------

-- | Result of an apply operation
data ApplyRpcResult
  = -- | Apply succeeded with summary
    ApplyOk !ApplyResult
  | -- | Apply failed
    ApplyFailed !Text
  | -- | Apply started asynchronously (task ID)
    ApplyAsync !Int64
  deriving (Eq, Show)

-- | Apply an environment configuration from YAML
applyConfig :: Connection -> Text -> Bool -> Bool -> IO (Either ConnectionError ApplyRpcResult)
applyConfig conn yaml skipExisting wait = do
  result <- sendRequest conn (ReqApply yaml skipExisting wait)
  case result of
    Left err -> pure $ Left err
    Right (RespApplyResult ar) -> pure $ Right $ ApplyOk ar
    Right (RespApplyStarted tid) -> pure $ Right $ ApplyAsync tid
    Right (RespError msg) -> pure $ Right $ ApplyFailed msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

--------------------------------------------------------------------------------
-- Task History Operations
--------------------------------------------------------------------------------

-- | List task history entries
taskList :: Connection -> Int -> Maybe TaskSubsystem -> Maybe TaskResult -> Bool -> IO (Either ConnectionError [TaskInfo])
taskList conn limit mSub mResult includeSubtasks = do
  result <- sendRequest conn (ReqTaskList limit mSub mResult includeSubtasks)
  case result of
    Left err -> pure $ Left err
    Right (RespTaskList tasks) -> pure $ Right tasks
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Show a single task history entry
taskShow :: Connection -> Int64 -> IO (Either ConnectionError (Maybe TaskInfo))
taskShow conn taskId = do
  result <- sendRequest conn (ReqTaskShow taskId)
  case result of
    Left err -> pure $ Left err
    Right (RespTaskInfo info) -> pure $ Right $ Just info
    Right RespTaskNotFound -> pure $ Right Nothing
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | List subtasks for a parent task
taskListChildren :: Connection -> Int64 -> IO (Either ConnectionError [TaskInfo])
taskListChildren conn parentId = do
  result <- sendRequest conn (ReqTaskListChildren parentId)
  case result of
    Left err -> pure $ Left err
    Right (RespTaskList tasks) -> pure $ Right tasks
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

--------------------------------------------------------------------------------
-- Cloud-Init Config Operations
--------------------------------------------------------------------------------

-- | Result of a cloud-init config operation
data CloudInitResult
  = CloudInitOk
  | CloudInitConfig !(Maybe CloudInitInfo)
  | CloudInitNotFound
  | CloudInitError !Text
  deriving (Show)

-- | Set cloud-init config for a VM
cloudInitSet :: Connection -> Text -> Maybe Text -> Maybe Text -> Bool -> IO (Either ConnectionError CloudInitResult)
cloudInitSet conn vmRef mUserData mNetworkConfig injectSshKeys = do
  result <- sendRequest conn (ReqCloudInitSet (Ref vmRef) mUserData mNetworkConfig injectSshKeys)
  case result of
    Left err -> pure $ Left err
    Right RespCloudInitOk -> pure $ Right CloudInitOk
    Right RespVmNotFound -> pure $ Right CloudInitNotFound
    Right (RespError msg) -> pure $ Right $ CloudInitError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Get cloud-init config for a VM
cloudInitGet :: Connection -> Text -> IO (Either ConnectionError CloudInitResult)
cloudInitGet conn vmRef = do
  result <- sendRequest conn (ReqCloudInitGet (Ref vmRef))
  case result of
    Left err -> pure $ Left err
    Right (RespCloudInitConfig mConfig) -> pure $ Right $ CloudInitConfig mConfig
    Right RespVmNotFound -> pure $ Right CloudInitNotFound
    Right (RespError msg) -> pure $ Right $ CloudInitError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Delete cloud-init config for a VM (revert to defaults)
cloudInitDelete :: Connection -> Text -> IO (Either ConnectionError CloudInitResult)
cloudInitDelete conn vmRef = do
  result <- sendRequest conn (ReqCloudInitDelete (Ref vmRef))
  case result of
    Left err -> pure $ Left err
    Right RespCloudInitOk -> pure $ Right CloudInitOk
    Right RespVmNotFound -> pure $ Right CloudInitNotFound
    Right (RespError msg) -> pure $ Right $ CloudInitError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"
