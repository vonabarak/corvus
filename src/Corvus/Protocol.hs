{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Corvus.Protocol
  ( -- * Message wrapper
    Request (..)
  , Response (..)

    -- * Protocol version
  , protocolVersion

    -- * Message encoding/decoding
  , encodeMessage
  , decodeMessage

    -- * Response data
  , StatusInfo (..)
  , VmInfo (..)
  , VmDetails (..)
  , DriveInfo (..)
  , NetIfInfo (..)
  , DiskImageInfo (..)
  , SnapshotInfo (..)
  , SharedDirInfo (..)
  , SshKeyInfo (..)
  , TemplateVmInfo (..)
  , TemplateDetails (..)
  , TemplateDriveInfo (..)
  , TemplateNetIfInfo (..)
  , TemplateSshKeyInfo (..)

    -- * Network info
  , NetworkInfo (..)

    -- * Apply config
  , ApplyCreated (..)
  , ApplyResult (..)
  , TaskInfo (..)

    -- * Entity reference
  , Ref (..)
  )
where

import Corvus.Model (CacheType, DriveFormat, DriveInterface, DriveMedia, NetInterfaceType, SharedDirCache, TaskResult, TaskSubsystem, TemplateCloneStrategy, VmStatus)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Binary (Binary, decodeOrFail, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word8)
import GHC.Generics (Generic)

-- | A reference to an entity by name or numeric ID.
-- If the text parses as an integer, it is treated as an ID lookup.
-- Otherwise it is treated as a name lookup via the entity's unique constraint.
newtype Ref = Ref {unRef :: Text}
  deriving (Eq, Show, Generic, Binary)

-- | Current protocol version. Increment when the wire format changes.
protocolVersion :: Word8
protocolVersion = 15

-- | Client requests
data Request
  = ReqPing
  | ReqStatus
  | ReqShutdown
  | ReqListVms
  | -- | Show VM (vmRef)
    ReqShowVm !Ref
  | -- | Create VM (name, cpuCount, ramMb, description, headless, guestAgent, cloudInit)
    ReqVmCreate !Text !Int !Int !(Maybe Text) !Bool !Bool !Bool
  | -- | Delete VM (vmRef)
    ReqVmDelete !Ref
  | -- | Start VM (stopped/paused -> running). Bool = wait for completion.
    ReqVmStart !Ref !Bool
  | -- | Stop VM (running -> stopped). Bool = wait for completion.
    ReqVmStop !Ref !Bool
  | -- | Pause VM (running -> paused)
    ReqVmPause !Ref
  | -- | Reset VM (any -> stopped)
    ReqVmReset !Ref
  | -- | Disk image operations
    -- | Create disk image (name, format, sizeMb, optionalPath)
    ReqDiskCreate !Text !DriveFormat !Int64 !(Maybe Text)
  | -- | Register existing disk image (name, filePath, format)
    ReqDiskRegister !Text !Text !(Maybe DriveFormat)
  | -- | Create overlay disk image (overlayName, baseDiskRef, optionalPath)
    ReqDiskCreateOverlay !Text !Ref !(Maybe Text)
  | -- | Refresh disk image size from qemu-img info (diskRef)
    ReqDiskRefresh !Ref
  | -- | Delete disk image (diskRef)
    ReqDiskDelete !Ref
  | -- | Resize disk image (diskRef, newSizeMb)
    ReqDiskResize !Ref !Int64
  | -- | List all disk images
    ReqDiskList
  | -- | Show disk image details (diskRef)
    ReqDiskShow !Ref
  | -- | Clone disk image (name, baseDiskRef, optionalPath)
    ReqDiskClone !Text !Ref !(Maybe Text)
  | -- | Snapshot operations (qcow2 only)
    -- | Create snapshot (diskRef, snapshotName)
    ReqSnapshotCreate !Ref !Text
  | -- | Delete snapshot (diskRef, snapshotRef)
    ReqSnapshotDelete !Ref !Ref
  | -- | Rollback to snapshot (diskRef, snapshotRef)
    ReqSnapshotRollback !Ref !Ref
  | -- | Merge snapshot (diskRef, snapshotRef)
    ReqSnapshotMerge !Ref !Ref
  | -- | List snapshots (diskRef)
    ReqSnapshotList !Ref
  | -- | Attach/detach operations
    -- | Attach disk to VM (vmRef, diskRef, interface, media, readOnly, discard, cache)
    ReqDiskAttach !Ref !Ref !DriveInterface !(Maybe DriveMedia) !Bool !Bool !CacheType
  | -- | Detach disk from VM (vmRef, diskRef)
    ReqDiskDetach !Ref !Ref
  | -- | Shared directory operations
    -- | Add shared directory to VM (vmRef, hostPath, tag, cache, readOnly)
    ReqSharedDirAdd !Ref !Text !Text !SharedDirCache !Bool
  | -- | Remove shared directory from VM (vmRef, sharedDirRef)
    ReqSharedDirRemove !Ref !Ref
  | -- | List shared directories for VM (vmRef)
    ReqSharedDirList !Ref
  | -- | Network interface operations
    -- | Add network interface to VM (vmRef, interfaceType, hostDevice, macAddress, networkRef)
    ReqNetIfAdd !Ref !NetInterfaceType !Text !(Maybe Text) !(Maybe Ref)
  | -- | Remove network interface from VM (vmRef, netIfId)
    ReqNetIfRemove !Ref !Int64
  | -- | List network interfaces for VM (vmRef)
    ReqNetIfList !Ref
  | -- | SSH key operations
    -- | Create SSH key (name, publicKey)
    ReqSshKeyCreate !Text !Text
  | -- | Delete SSH key (keyRef)
    ReqSshKeyDelete !Ref
  | -- | List all SSH keys
    ReqSshKeyList
  | -- | Attach SSH key to VM (vmRef, keyRef)
    ReqSshKeyAttach !Ref !Ref
  | -- | Detach SSH key from VM (vmRef, keyRef)
    ReqSshKeyDetach !Ref !Ref
  | -- | List SSH keys for VM (vmRef)
    ReqSshKeyListForVm !Ref
  | -- | Template operations
    -- | Create template from YAML (yamlContent)
    ReqTemplateCreate !Text
  | -- | Delete template (templateRef)
    ReqTemplateDelete !Ref
  | -- | List all templates
    ReqTemplateList
  | -- | Show template details (templateRef)
    ReqTemplateShow !Ref
  | -- | Instantiate template (templateRef, newVmName)
    ReqTemplateInstantiate !Ref !Text
  | -- | Edit VM properties (vmRef, cpuCount, ramMb, description, headless, guestAgent, cloudInit)
    -- Each Maybe field is updated only if Just.
    ReqVmEdit !Ref !(Maybe Int) !(Maybe Int) !(Maybe Text) !(Maybe Bool) !(Maybe Bool) !(Maybe Bool)
  | -- | Generate/regenerate cloud-init ISO for a VM (vmRef)
    ReqVmCloudInit !Ref
  | -- | Virtual network operations
    -- | Create network (name, subnet, dhcp)
    ReqNetworkCreate !Text !Text !Bool
  | -- | Delete network (networkRef)
    ReqNetworkDelete !Ref
  | -- | Start network (networkRef)
    ReqNetworkStart !Ref
  | -- | Stop network (networkRef, force)
    ReqNetworkStop !Ref !Bool
  | -- | List all networks
    ReqNetworkList
  | -- | Show network details (networkRef)
    ReqNetworkShow !Ref
  | -- | Guest command execution (vmRef, command)
    ReqGuestExec !Ref !Text
  | -- | Import disk image from URL (name, url, optionalFormat)
    ReqDiskImportUrl !Text !Text !(Maybe Text)
  | -- | Apply environment from YAML config (yamlContent, skipExisting)
    ReqApply !Text !Bool
  | -- | List task history (limit, optionalSubsystem, optionalResult)
    ReqTaskList !Int !(Maybe TaskSubsystem) !(Maybe TaskResult)
  | -- | Show single task details (taskId)
    ReqTaskShow !Int64
  deriving (Eq, Show, Generic, Binary)

-- | Status information returned by the server
data StatusInfo = StatusInfo
  { siUptime :: !Int
  -- ^ Uptime in seconds
  , siConnections :: !Int
  -- ^ Number of active connections
  , siVersion :: !Text
  -- ^ Daemon version
  , siNamespacePid :: !(Maybe Int)
  -- ^ PID of the network namespace manager
  }
  deriving (Eq, Show, Generic, Binary)

-- | VM summary for list view
data VmInfo = VmInfo
  { viId :: !Int64
  , viName :: !Text
  , viStatus :: !VmStatus
  , viCpuCount :: !Int
  , viRamMb :: !Int
  , viHeadless :: !Bool
  , viGuestAgent :: !Bool
  , viCloudInit :: !Bool
  , viHealthcheck :: !(Maybe UTCTime)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Drive info for details view
data DriveInfo = DriveInfo
  { diId :: !Int64
  , diDiskImageId :: !Int64
  , diDiskImageName :: !Text
  , diInterface :: !DriveInterface
  , diFilePath :: !Text
  , diFormat :: !DriveFormat
  , diMedia :: !(Maybe DriveMedia)
  , diReadOnly :: !Bool
  , diCacheType :: !CacheType
  , diDiscard :: !Bool
  }
  deriving (Eq, Show, Generic, Binary)

-- | Network interface info for details view
data NetIfInfo = NetIfInfo
  { niId :: !Int64
  , niType :: !NetInterfaceType
  , niHostDevice :: !Text
  , niMacAddress :: !Text
  , niNetworkId :: !(Maybe Int64)
  , niNetworkName :: !(Maybe Text)
  , niGuestIpAddresses :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Full VM details
data VmDetails = VmDetails
  { vdId :: !Int64
  , vdName :: !Text
  , vdCreatedAt :: !UTCTime
  , vdStatus :: !VmStatus
  , vdCpuCount :: !Int
  , vdRamMb :: !Int
  , vdDescription :: !(Maybe Text)
  , vdDrives :: ![DriveInfo]
  , vdNetIfs :: ![NetIfInfo]
  , vdHeadless :: !Bool
  , vdMonitorSocket :: !Text
  -- ^ Path to HMP monitor socket
  , vdSpiceSocket :: !Text
  -- ^ Path to SPICE socket
  , vdSerialSocket :: !Text
  -- ^ Path to serial console socket
  , vdGuestAgentSocket :: !Text
  -- ^ Path to QEMU Guest Agent socket
  , vdGuestAgent :: !Bool
  -- ^ Whether guest agent is enabled for this VM
  , vdCloudInit :: !Bool
  -- ^ Whether cloud-init is enabled for this VM
  , vdHealthcheck :: !(Maybe UTCTime)
  -- ^ Last successful guest agent ping time
  }
  deriving (Eq, Show, Generic, Binary)

-- | Disk image info for list/show view
data DiskImageInfo = DiskImageInfo
  { diiId :: !Int64
  , diiName :: !Text
  , diiFilePath :: !Text
  , diiFormat :: !DriveFormat
  , diiSizeMb :: !(Maybe Int)
  , diiCreatedAt :: !UTCTime
  , diiAttachedTo :: ![(Int64, Text)]
  -- ^ VM (ID, name) pairs this disk is attached to
  , diiBackingImageId :: !(Maybe Int64)
  -- ^ Backing image ID (if this is an overlay)
  , diiBackingImageName :: !(Maybe Text)
  -- ^ Backing image name (if this is an overlay)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Snapshot info
data SnapshotInfo = SnapshotInfo
  { sniId :: !Int64
  , sniName :: !Text
  , sniCreatedAt :: !UTCTime
  , sniSizeMb :: !(Maybe Int)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Shared directory info
data SharedDirInfo = SharedDirInfo
  { sdiId :: !Int64
  , sdiPath :: !Text
  , sdiTag :: !Text
  , sdiCache :: !SharedDirCache
  , sdiReadOnly :: !Bool
  , sdiPid :: !(Maybe Int)
  -- ^ virtiofsd PID if running
  }
  deriving (Eq, Show, Generic, Binary)

-- | SSH key info
data SshKeyInfo = SshKeyInfo
  { skiId :: !Int64
  , skiName :: !Text
  , skiPublicKey :: !Text
  , skiCreatedAt :: !UTCTime
  , skiAttachedVms :: ![(Int64, Text)]
  -- ^ VM (ID, name) pairs this key is attached to
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template VM summary for list view
data TemplateVmInfo = TemplateVmInfo
  { tviId :: !Int64
  , tviName :: !Text
  , tviCpuCount :: !Int
  , tviRamMb :: !Int
  , tviDescription :: !(Maybe Text)
  , tviHeadless :: !Bool
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template drive info for details view
data TemplateDriveInfo = TemplateDriveInfo
  { tvdiDiskImageId :: !Int64
  , tvdiDiskImageName :: !Text
  , tvdiInterface :: !DriveInterface
  , tvdiMedia :: !(Maybe DriveMedia)
  , tvdiReadOnly :: !Bool
  , tvdiCacheType :: !CacheType
  , tvdiDiscard :: !Bool
  , tvdiCloneStrategy :: !TemplateCloneStrategy
  , tvdiNewSizeMb :: !(Maybe Int)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template network interface info
data TemplateNetIfInfo = TemplateNetIfInfo
  { tvniType :: !NetInterfaceType
  , tvniHostDevice :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template SSH key info
data TemplateSshKeyInfo = TemplateSshKeyInfo
  { tvskiId :: !Int64
  , tvskiName :: !Text
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template VM full details
data TemplateDetails = TemplateDetails
  { tvdId :: !Int64
  , tvdName :: !Text
  , tvdCpuCount :: !Int
  , tvdRamMb :: !Int
  , tvdDescription :: !(Maybe Text)
  , tvdHeadless :: !Bool
  , tvdCloudInit :: !Bool
  , tvdCreatedAt :: !UTCTime
  , tvdDrives :: ![TemplateDriveInfo]
  , tvdNetIfs :: ![TemplateNetIfInfo]
  , tvdSshKeys :: ![TemplateSshKeyInfo]
  }
  deriving (Eq, Show, Generic, Binary)

-- | Virtual network info
data NetworkInfo = NetworkInfo
  { nwiId :: !Int64
  , nwiName :: !Text
  , nwiSubnet :: !Text
  , nwiDhcp :: !Bool
  , nwiRunning :: !Bool
  , nwiDnsmasqPid :: !(Maybe Int)
  , nwiCreatedAt :: !UTCTime
  }
  deriving (Eq, Show, Generic, Binary)

-- | A resource created during an apply operation
data ApplyCreated = ApplyCreated
  { acName :: !Text
  , acId :: !Int64
  }
  deriving (Eq, Show, Generic, Binary)

-- | Summary of resources created by an apply operation
data ApplyResult = ApplyResult
  { arSshKeys :: ![ApplyCreated]
  , arDisks :: ![ApplyCreated]
  , arNetworks :: ![ApplyCreated]
  , arVms :: ![ApplyCreated]
  }
  deriving (Eq, Show, Generic, Binary)

-- | Information about a task history entry
data TaskInfo = TaskInfo
  { tiId :: !Int64
  , tiStartedAt :: !UTCTime
  , tiFinishedAt :: !(Maybe UTCTime)
  , tiSubsystem :: !TaskSubsystem
  , tiEntityId :: !(Maybe Int)
  , tiEntityName :: !(Maybe Text)
  , tiCommand :: !Text
  , tiResult :: !TaskResult
  , tiMessage :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, Binary)

--------------------------------------------------------------------------------
-- ToJSON instances for machine-readable output
--------------------------------------------------------------------------------

instance ToJSON StatusInfo where
  toJSON s =
    object
      [ "uptime" .= siUptime s
      , "connections" .= siConnections s
      , "version" .= siVersion s
      , "namespacePid" .= siNamespacePid s
      ]

instance ToJSON VmInfo where
  toJSON v =
    object
      [ "id" .= viId v
      , "name" .= viName v
      , "status" .= viStatus v
      , "cpuCount" .= viCpuCount v
      , "ramMb" .= viRamMb v
      , "headless" .= viHeadless v
      , "guestAgent" .= viGuestAgent v
      , "cloudInit" .= viCloudInit v
      , "healthcheck" .= viHealthcheck v
      ]

instance ToJSON DriveInfo where
  toJSON d =
    object
      [ "id" .= diId d
      , "diskImageId" .= diDiskImageId d
      , "diskImageName" .= diDiskImageName d
      , "interface" .= diInterface d
      , "filePath" .= diFilePath d
      , "format" .= diFormat d
      , "media" .= diMedia d
      , "readOnly" .= diReadOnly d
      , "cacheType" .= diCacheType d
      , "discard" .= diDiscard d
      ]

instance ToJSON NetIfInfo where
  toJSON n =
    object
      [ "id" .= niId n
      , "type" .= niType n
      , "hostDevice" .= niHostDevice n
      , "macAddress" .= niMacAddress n
      , "networkId" .= niNetworkId n
      , "networkName" .= niNetworkName n
      , "guestIpAddresses" .= niGuestIpAddresses n
      ]

instance ToJSON VmDetails where
  toJSON v =
    object
      [ "id" .= vdId v
      , "name" .= vdName v
      , "createdAt" .= vdCreatedAt v
      , "status" .= vdStatus v
      , "cpuCount" .= vdCpuCount v
      , "ramMb" .= vdRamMb v
      , "description" .= vdDescription v
      , "drives" .= vdDrives v
      , "networkInterfaces" .= vdNetIfs v
      , "headless" .= vdHeadless v
      , "monitorSocket" .= vdMonitorSocket v
      , "spiceSocket" .= vdSpiceSocket v
      , "serialSocket" .= vdSerialSocket v
      , "guestAgentSocket" .= vdGuestAgentSocket v
      , "guestAgent" .= vdGuestAgent v
      , "cloudInit" .= vdCloudInit v
      , "healthcheck" .= vdHealthcheck v
      ]

instance ToJSON DiskImageInfo where
  toJSON d =
    object
      [ "id" .= diiId d
      , "name" .= diiName d
      , "filePath" .= diiFilePath d
      , "format" .= diiFormat d
      , "sizeMb" .= diiSizeMb d
      , "createdAt" .= diiCreatedAt d
      , "attachedTo" .= diiAttachedTo d
      , "backingImageId" .= diiBackingImageId d
      , "backingImageName" .= diiBackingImageName d
      ]

instance ToJSON SnapshotInfo where
  toJSON s =
    object
      [ "id" .= sniId s
      , "name" .= sniName s
      , "createdAt" .= sniCreatedAt s
      , "sizeMb" .= sniSizeMb s
      ]

instance ToJSON SharedDirInfo where
  toJSON s =
    object
      [ "id" .= sdiId s
      , "path" .= sdiPath s
      , "tag" .= sdiTag s
      , "cache" .= sdiCache s
      , "readOnly" .= sdiReadOnly s
      , "pid" .= sdiPid s
      ]

instance ToJSON SshKeyInfo where
  toJSON k =
    object
      [ "id" .= skiId k
      , "name" .= skiName k
      , "publicKey" .= skiPublicKey k
      , "createdAt" .= skiCreatedAt k
      , "attachedVms" .= skiAttachedVms k
      ]

instance ToJSON TemplateVmInfo where
  toJSON t =
    object
      [ "id" .= tviId t
      , "name" .= tviName t
      , "cpuCount" .= tviCpuCount t
      , "ramMb" .= tviRamMb t
      , "description" .= tviDescription t
      , "headless" .= tviHeadless t
      ]

instance ToJSON TemplateDriveInfo where
  toJSON d =
    object
      [ "diskImageId" .= tvdiDiskImageId d
      , "diskImageName" .= tvdiDiskImageName d
      , "interface" .= tvdiInterface d
      , "media" .= tvdiMedia d
      , "readOnly" .= tvdiReadOnly d
      , "cacheType" .= tvdiCacheType d
      , "discard" .= tvdiDiscard d
      , "cloneStrategy" .= tvdiCloneStrategy d
      , "newSizeMb" .= tvdiNewSizeMb d
      ]

instance ToJSON TemplateNetIfInfo where
  toJSON n =
    object
      [ "type" .= tvniType n
      , "hostDevice" .= tvniHostDevice n
      ]

instance ToJSON TemplateSshKeyInfo where
  toJSON k =
    object
      [ "id" .= tvskiId k
      , "name" .= tvskiName k
      ]

instance ToJSON NetworkInfo where
  toJSON n =
    object
      [ "id" .= nwiId n
      , "name" .= nwiName n
      , "subnet" .= nwiSubnet n
      , "dhcp" .= nwiDhcp n
      , "running" .= nwiRunning n
      , "dnsmasqPid" .= nwiDnsmasqPid n
      , "createdAt" .= nwiCreatedAt n
      ]

instance ToJSON TemplateDetails where
  toJSON t =
    object
      [ "id" .= tvdId t
      , "name" .= tvdName t
      , "cpuCount" .= tvdCpuCount t
      , "ramMb" .= tvdRamMb t
      , "description" .= tvdDescription t
      , "headless" .= tvdHeadless t
      , "cloudInit" .= tvdCloudInit t
      , "createdAt" .= tvdCreatedAt t
      , "drives" .= tvdDrives t
      , "networkInterfaces" .= tvdNetIfs t
      , "sshKeys" .= tvdSshKeys t
      ]

instance ToJSON ApplyCreated where
  toJSON a =
    object
      [ "name" .= acName a
      , "id" .= acId a
      ]

instance ToJSON ApplyResult where
  toJSON r =
    object
      [ "sshKeys" .= arSshKeys r
      , "disks" .= arDisks r
      , "networks" .= arNetworks r
      , "vms" .= arVms r
      ]

instance ToJSON TaskInfo where
  toJSON t =
    object
      [ "id" .= tiId t
      , "startedAt" .= tiStartedAt t
      , "finishedAt" .= tiFinishedAt t
      , "subsystem" .= tiSubsystem t
      , "entityId" .= tiEntityId t
      , "entityName" .= tiEntityName t
      , "command" .= tiCommand t
      , "result" .= tiResult t
      , "message" .= tiMessage t
      ]

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
  | -- | Disk is still attached to VMs (ID, name pairs)
    RespDiskInUse ![(Int64, Text)]
  | -- | Disk is used as backing image for overlays (ID, name pairs)
    RespDiskHasOverlays ![(Int64, Text)]
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
  | -- | SSH key is in use by VMs (ID, name pairs)
    RespSshKeyInUse ![(Int64, Text)]
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
  | -- | VM edited successfully
    RespVmEdited
  | -- | Virtual network responses
    -- | Network created successfully (new network ID)
    RespNetworkCreated !Int64
  | -- | Network deleted successfully
    RespNetworkDeleted
  | -- | Network started successfully
    RespNetworkStarted
  | -- | Network stopped successfully
    RespNetworkStopped
  | -- | List of networks
    RespNetworkList ![NetworkInfo]
  | -- | Single network info
    RespNetworkDetails !NetworkInfo
  | -- | Network not found
    RespNetworkNotFound
  | -- | Network is already running
    RespNetworkAlreadyRunning
  | -- | Network is not running
    RespNetworkNotRunning
  | -- | Network is in use (referenced by interfaces or running VMs)
    RespNetworkInUse
  | -- | Network error
    RespNetworkError !Text
  | -- | Guest command execution result (exitcode, stdout, stderr)
    RespGuestExecResult !Int !Text !Text
  | -- | Guest agent not enabled on this VM
    RespGuestAgentNotEnabled
  | -- | Guest agent communication error
    RespGuestAgentError !Text
  | -- | Apply config result with summary of created resources
    RespApplyResult !ApplyResult
  | -- | Task history list
    RespTaskList ![TaskInfo]
  | -- | Single task info
    RespTaskInfo !TaskInfo
  | -- | Task not found
    RespTaskNotFound
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
