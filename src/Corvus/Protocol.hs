{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Corvus.Protocol
  ( -- * Message wrapper
    Request (..),
    Response (..),

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
  )
where

import Corvus.Model (CacheType, DriveFormat, DriveInterface, DriveMedia, NetInterfaceType, VmStatus)
import Data.Binary (Binary, decodeOrFail, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Client requests
data Request
  = ReqPing
  | ReqStatus
  | ReqShutdown
  | ReqListVms
  | -- | VM ID
    ReqShowVm !Int64
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
  | -- | Delete disk image (diskImageId)
    ReqDiskDelete !Int64
  | -- | Resize disk image (diskImageId, newSizeMb)
    ReqDiskResize !Int64 !Int64
  | -- | List all disk images
    ReqDiskList
  | -- | Show disk image details (diskImageId)
    ReqDiskShow !Int64
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
    -- | Attach disk to VM (vmId, diskImageId, interface, media)
    ReqDiskAttach !Int64 !Int64 !DriveInterface !(Maybe DriveMedia)
  | -- | Detach disk from VM (vmId, driveId)
    ReqDiskDetach !Int64 !Int64
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
    diiAttachedTo :: ![Int64]
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

-- | Server responses
data Response
  = RespPong
  | RespStatus !StatusInfo
  | RespShutdownAck !Bool
  | RespError !Text
  | RespVmList ![VmInfo]
  | RespVmDetails !VmDetails
  | RespVmNotFound
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
  deriving (Eq, Show, Generic, Binary)

-- | Encode a message with a length prefix (8 bytes, big-endian)
encodeMessage :: (Binary a) => a -> ByteString
encodeMessage msg =
  let payload = encode msg
      len = fromIntegral (BL.length payload) :: Int64
   in encode len <> payload

-- | Decode a length-prefixed message
decodeMessage :: (Binary a) => ByteString -> Either String a
decodeMessage bs =
  case decodeOrFail bs of
    Left (_, _, err) -> Left err
    Right (rest, _, len) ->
      let payload = BL.take (fromIntegral (len :: Int64)) rest
       in case decodeOrFail payload of
            Left (_, _, err) -> Left err
            Right (_, _, msg) -> Right msg
