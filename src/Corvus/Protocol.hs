{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Corvus.Protocol
  ( -- * Message wrapper
    Request (..),
    Response (..),

    -- * Protocol version
    protocolVersion,

    -- * Message encoding/decoding
    encodeMessage,
    decodeMessage,

    -- * Response data
    StatusInfo (..),
    VmInfo (..),
    VmDetails (..),
    DriveInfo (..),
    NetIfInfo (..),
    DiskImageInfo (..),
    SnapshotInfo (..),
    SharedDirInfo (..),
    SshKeyInfo (..),
    TemplateVmInfo (..),
    TemplateDetails (..),
    TemplateDriveInfo (..),
    TemplateNetIfInfo (..),
    TemplateSshKeyInfo (..),
  )
where

import Corvus.Model (CacheType, DriveFormat, DriveInterface, DriveMedia, NetInterfaceType, SharedDirCache, TemplateCloneStrategy, VmStatus)
import Data.Binary (Binary, decodeOrFail, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word8)
import GHC.Generics (Generic)

-- | Current protocol version. Increment when the wire format changes.
protocolVersion :: Word8
protocolVersion = 1

-- | Client requests
data Request
  = ReqPing
  | ReqStatus
  | ReqShutdown
  | ReqListVms
  | -- | VM ID
    ReqShowVm !Int64
  | -- | Create VM (name, cpuCount, ramMb, description)
    ReqVmCreate !Text !Int !Int !(Maybe Text)
  | -- | Delete VM (vmId)
    ReqVmDelete !Int64
  | -- | Start VM (stopped/paused -> running)
    ReqVmStart !Int64
  | -- | Stop VM (running -> stopped)
    ReqVmStop !Int64
  | -- | Pause VM (running -> paused)
    ReqVmPause !Int64
  | -- | Reset VM (any -> stopped)
    ReqVmReset !Int64
  | -- | Disk image operations
    -- | Create disk image (name, format, sizeMb)
    ReqDiskCreate !Text !DriveFormat !Int64
  | -- | Register existing disk image (name, filePath, format, sizeMb)
    ReqDiskRegister !Text !Text !DriveFormat !(Maybe Int64)
  | -- | Create overlay disk image (overlayName, baseDiskImageId)
    ReqDiskCreateOverlay !Text !Int64
  | -- | Delete disk image (diskImageId)
    ReqDiskDelete !Int64
  | -- | Resize disk image (diskImageId, newSizeMb)
    ReqDiskResize !Int64 !Int64
  | -- | List all disk images
    ReqDiskList
  | -- | Show disk image details (diskImageId)
    ReqDiskShow !Int64
  | -- | Clone disk image (name, baseDiskImageId, optionalPath)
    ReqDiskClone !Text !Int64 !(Maybe Text)
  | -- | Snapshot operations (qcow2 only)
    -- | Create snapshot (diskImageId, snapshotName)
    ReqSnapshotCreate !Int64 !Text
  | -- | Delete snapshot (diskImageId, snapshotId)
    ReqSnapshotDelete !Int64 !Int64
  | -- | Rollback to snapshot (diskImageId, snapshotId)
    ReqSnapshotRollback !Int64 !Int64
  | -- | Merge snapshot (diskImageId, snapshotId)
    ReqSnapshotMerge !Int64 !Int64
  | -- | List snapshots (diskImageId)
    ReqSnapshotList !Int64
  | -- | Attach/detach operations
    -- | Attach disk to VM (vmId, diskImageId, interface, media, readOnly, discard, cache)
    ReqDiskAttach !Int64 !Int64 !DriveInterface !(Maybe DriveMedia) !Bool !Bool !CacheType
  | -- | Detach disk from VM (vmId, driveId)
    ReqDiskDetach !Int64 !Int64
  | -- | Shared directory operations
    -- | Add shared directory to VM (vmId, hostPath, tag, cache, readOnly)
    ReqSharedDirAdd !Int64 !Text !Text !SharedDirCache !Bool
  | -- | Remove shared directory from VM (vmId, sharedDirId)
    ReqSharedDirRemove !Int64 !Int64
  | -- | List shared directories for VM (vmId)
    ReqSharedDirList !Int64
  | -- | Network interface operations
    -- | Add network interface to VM (vmId, interfaceType, hostDevice, macAddress)
    ReqNetIfAdd !Int64 !NetInterfaceType !Text !Text
  | -- | Remove network interface from VM (vmId, netIfId)
    ReqNetIfRemove !Int64 !Int64
  | -- | List network interfaces for VM (vmId)
    ReqNetIfList !Int64
  | -- | SSH key operations
    -- | Create SSH key (name, publicKey)
    ReqSshKeyCreate !Text !Text
  | -- | Delete SSH key (keyId)
    ReqSshKeyDelete !Int64
  | -- | List all SSH keys
    ReqSshKeyList
  | -- | Attach SSH key to VM (vmId, keyId)
    ReqSshKeyAttach !Int64 !Int64
  | -- | Detach SSH key from VM (vmId, keyId)
    ReqSshKeyDetach !Int64 !Int64
  | -- | List SSH keys for VM (vmId)
    ReqSshKeyListForVm !Int64
  | -- | Template operations
    -- | Create template from YAML (yamlContent)
    ReqTemplateCreate !Text
  | -- | Delete template (templateId)
    ReqTemplateDelete !Int64
  | -- | List all templates
    ReqTemplateList
  | -- | Show template details (templateId)
    ReqTemplateShow !Int64
  | -- | Instantiate template (templateId, newVmName)
    ReqTemplateInstantiate !Int64 !Text
  deriving (Eq, Show, Generic, Binary)

-- | Status information returned by the server
data StatusInfo = StatusInfo
  { -- | Uptime in seconds
    siUptime :: !Int,
    -- | Number of active connections
    siConnections :: !Int,
    -- | Daemon version
    siVersion :: !Text
  }
  deriving (Eq, Show, Generic, Binary)

-- | VM summary for list view
data VmInfo = VmInfo
  { viId :: !Int64,
    viName :: !Text,
    viStatus :: !VmStatus,
    viCpuCount :: !Int,
    viRamMb :: !Int
  }
  deriving (Eq, Show, Generic, Binary)

-- | Drive info for details view
data DriveInfo = DriveInfo
  { diId :: !Int64,
    diDiskImageId :: !Int64,
    diInterface :: !DriveInterface,
    diFilePath :: !Text,
    diFormat :: !DriveFormat,
    diMedia :: !(Maybe DriveMedia),
    diReadOnly :: !Bool,
    diCacheType :: !CacheType,
    diDiscard :: !Bool
  }
  deriving (Eq, Show, Generic, Binary)

-- | Network interface info for details view
data NetIfInfo = NetIfInfo
  { niId :: !Int64,
    niType :: !NetInterfaceType,
    niHostDevice :: !Text,
    niMacAddress :: !Text
  }
  deriving (Eq, Show, Generic, Binary)

-- | Full VM details
data VmDetails = VmDetails
  { vdId :: !Int64,
    vdName :: !Text,
    vdCreatedAt :: !UTCTime,
    vdStatus :: !VmStatus,
    vdCpuCount :: !Int,
    vdRamMb :: !Int,
    vdDescription :: !(Maybe Text),
    vdDrives :: ![DriveInfo],
    vdNetIfs :: ![NetIfInfo],
    -- | Path to HMP monitor socket
    vdMonitorSocket :: !Text,
    -- | Path to SPICE socket
    vdSpiceSocket :: !Text
  }
  deriving (Eq, Show, Generic, Binary)

-- | Disk image info for list/show view
data DiskImageInfo = DiskImageInfo
  { diiId :: !Int64,
    diiName :: !Text,
    diiFilePath :: !Text,
    diiFormat :: !DriveFormat,
    diiSizeMb :: !(Maybe Int),
    diiCreatedAt :: !UTCTime,
    -- | VM IDs this disk is attached to
    diiAttachedTo :: ![Int64],
    -- | Backing image ID (if this is an overlay)
    diiBackingImageId :: !(Maybe Int64),
    -- | Backing image name (if this is an overlay)
    diiBackingImageName :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Snapshot info
data SnapshotInfo = SnapshotInfo
  { sniId :: !Int64,
    sniName :: !Text,
    sniCreatedAt :: !UTCTime,
    sniSizeMb :: !(Maybe Int)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Shared directory info
data SharedDirInfo = SharedDirInfo
  { sdiId :: !Int64,
    sdiPath :: !Text,
    sdiTag :: !Text,
    sdiCache :: !SharedDirCache,
    sdiReadOnly :: !Bool,
    -- | virtiofsd PID if running
    sdiPid :: !(Maybe Int)
  }
  deriving (Eq, Show, Generic, Binary)

-- | SSH key info
data SshKeyInfo = SshKeyInfo
  { skiId :: !Int64,
    skiName :: !Text,
    skiPublicKey :: !Text,
    skiCreatedAt :: !UTCTime,
    -- | VMs this key is attached to
    skiAttachedVms :: ![Int64]
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template VM summary for list view
data TemplateVmInfo = TemplateVmInfo
  { tviId :: !Int64,
    tviName :: !Text,
    tviCpuCount :: !Int,
    tviRamMb :: !Int,
    tviDescription :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template drive info for details view
data TemplateDriveInfo = TemplateDriveInfo
  { tvdiDiskImageId :: !Int64,
    tvdiDiskImageName :: !Text,
    tvdiInterface :: !DriveInterface,
    tvdiMedia :: !(Maybe DriveMedia),
    tvdiReadOnly :: !Bool,
    tvdiCacheType :: !CacheType,
    tvdiDiscard :: !Bool,
    tvdiCloneStrategy :: !TemplateCloneStrategy,
    tvdiNewSizeMb :: !(Maybe Int)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template network interface info
data TemplateNetIfInfo = TemplateNetIfInfo
  { tvniType :: !NetInterfaceType,
    tvniHostDevice :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template SSH key info
data TemplateSshKeyInfo = TemplateSshKeyInfo
  { tvskiId :: !Int64,
    tvskiName :: !Text
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template VM full details
data TemplateDetails = TemplateDetails
  { tvdId :: !Int64,
    tvdName :: !Text,
    tvdCpuCount :: !Int,
    tvdRamMb :: !Int,
    tvdDescription :: !(Maybe Text),
    tvdCreatedAt :: !UTCTime,
    tvdDrives :: ![TemplateDriveInfo],
    tvdNetIfs :: ![TemplateNetIfInfo],
    tvdSshKeys :: ![TemplateSshKeyInfo]
  }
  deriving (Eq, Show, Generic, Binary)

-- | Server responses
data Response
  = RespPong
  | RespStatus !StatusInfo
  | RespShutdownAck !Bool
  | RespError !Text
  | RespVmList ![VmInfo]
  | RespVmDetails !VmDetails
  | RespVmNotFound
  | -- | VM created successfully (new VM ID)
    RespVmCreated !Int64
  | -- | VM deleted successfully
    RespVmDeleted
  | -- | VM is running and cannot be deleted
    RespVmRunning
  | -- | New status after successful transition
    RespVmStateChanged !VmStatus
  | -- | Current status and error message
    RespInvalidTransition !VmStatus !Text
  | -- | Disk image responses
    -- | List of disk images
    RespDiskList ![DiskImageInfo]
  | -- | Single disk image info
    RespDiskInfo !DiskImageInfo
  | -- | Disk created successfully (new disk ID)
    RespDiskCreated !Int64
  | -- | Disk image not found
    RespDiskNotFound
  | -- | Disk operation successful
    RespDiskOk
  | -- | Snapshot responses
    -- | List of snapshots
    RespSnapshotList ![SnapshotInfo]
  | -- | Snapshot created successfully (new snapshot ID)
    RespSnapshotCreated !Int64
  | -- | Snapshot not found
    RespSnapshotNotFound
  | -- | Snapshot operation successful
    RespSnapshotOk
  | -- | Drive attached successfully (new drive ID)
    RespDiskAttached !Int64
  | -- | Drive not found
    RespDriveNotFound
  | -- | Operation not supported for this format
    RespFormatNotSupported !Text
  | -- | VM must be stopped for this operation
    RespVmMustBeStopped
  | -- | Disk is still attached to VMs
    RespDiskInUse ![Int64]
  | -- | Disk is used as backing image for overlays
    RespDiskHasOverlays ![Int64]
  | -- | Shared directory responses
    -- | List of shared directories
    RespSharedDirList ![SharedDirInfo]
  | -- | Shared directory added (new ID)
    RespSharedDirAdded !Int64
  | -- | Shared directory operation successful
    RespSharedDirOk
  | -- | Shared directory not found
    RespSharedDirNotFound
  | -- | Network interface responses
    -- | List of network interfaces
    RespNetIfList ![NetIfInfo]
  | -- | Network interface added (new ID)
    RespNetIfAdded !Int64
  | -- | Network interface operation successful
    RespNetIfOk
  | -- | Network interface not found
    RespNetIfNotFound
  | -- | SSH key responses
    -- | List of SSH keys
    RespSshKeyList ![SshKeyInfo]
  | -- | SSH key created (new ID)
    RespSshKeyCreated !Int64
  | -- | SSH key operation successful
    RespSshKeyOk
  | -- | SSH key not found
    RespSshKeyNotFound
  | -- | SSH key is in use by VMs
    RespSshKeyInUse ![Int64]
  | -- | Template responses
    -- | List of templates
    RespTemplateList ![TemplateVmInfo]
  | -- | Single template info
    RespTemplateInfo !TemplateDetails
  | -- | Template created successfully (new template ID)
    RespTemplateCreated !Int64
  | -- | Template not found
    RespTemplateNotFound
  | -- | Template deleted successfully
    RespTemplateDeleted
  | -- | Template instantiated successfully (new VM ID)
    RespTemplateInstantiated !Int64
  deriving (Eq, Show, Generic, Binary)

-- | Encode a message with protocol version and length prefix.
-- Wire format: [1 byte version][8 bytes length (big-endian)][payload]
encodeMessage :: (Binary a) => a -> ByteString
encodeMessage msg =
  let payload = encode msg
      len = fromIntegral (BL.length payload) :: Int64
   in encode protocolVersion <> encode len <> payload

-- | Decode a versioned, length-prefixed message.
-- Returns Left on version mismatch or decoding failure.
decodeMessage :: (Binary a) => ByteString -> Either String a
decodeMessage bs =
  case decodeOrFail bs of
    Left (_, _, err) -> Left $ "version decode error: " <> err
    Right (rest1, _, ver) ->
      if (ver :: Word8) /= protocolVersion
        then Left $ "protocol version mismatch: expected " <> show protocolVersion <> ", got " <> show ver
        else case decodeOrFail rest1 of
          Left (_, _, err) -> Left $ "length decode error: " <> err
          Right (rest2, _, len) ->
            let payload = BL.take (fromIntegral (len :: Int64)) rest2
             in case decodeOrFail payload of
                  Left (_, _, err) -> Left err
                  Right (_, _, msg) -> Right msg
