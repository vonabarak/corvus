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

    -- * Daemon status
  , StatusInfo (..)

    -- * Per-subsystem response data (re-exported for convenience)
  , module Corvus.Protocol.Vm
  , module Corvus.Protocol.Disk
  , module Corvus.Protocol.SharedDir
  , module Corvus.Protocol.SshKey
  , module Corvus.Protocol.Template
  , module Corvus.Protocol.Network
  , module Corvus.Protocol.CloudInit
  , module Corvus.Protocol.Apply
  , module Corvus.Protocol.Task

    -- * Entity reference
  , Ref (..)
  )
where

import Corvus.Model (CacheType, DriveFormat, DriveInterface, DriveMedia, NetInterfaceType, SharedDirCache, TaskResult, TaskSubsystem, TemplateCloneStrategy, VmStatus)
import Corvus.Protocol.Apply
import Corvus.Protocol.CloudInit
import Corvus.Protocol.Disk
import Corvus.Protocol.Network
import Corvus.Protocol.SharedDir
import Corvus.Protocol.SshKey
import Corvus.Protocol.Task
import Corvus.Protocol.Template
import Corvus.Protocol.Vm
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Binary (Binary, decodeOrFail, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Generics (Generic)

-- | A reference to an entity by name or numeric ID.
-- If the text parses as an integer, it is treated as an ID lookup.
-- Otherwise it is treated as a name lookup via the entity's unique constraint.
newtype Ref = Ref {unRef :: Text}
  deriving (Eq, Show, Generic, Binary)

-- | Current protocol version. Increment when the wire format changes.
protocolVersion :: Word8
protocolVersion = 28

-- | Client requests
data Request
  = ReqPing
  | ReqStatus
  | ReqShutdown
  | ReqListVms
  | -- | Show VM (vmRef)
    ReqShowVm !Ref
  | -- | Create VM (name, cpuCount, ramMb, description, headless, guestAgent, cloudInit, autostart)
    ReqVmCreate !Text !Int !Int !(Maybe Text) !Bool !Bool !Bool !Bool
  | -- | Delete VM (vmRef, deleteDisks)
    ReqVmDelete !Ref !Bool
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
  | -- | Register existing disk image (name, filePath, format, optionalBackingRef)
    ReqDiskRegister !Text !Text !(Maybe DriveFormat) !(Maybe Ref)
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
  | -- | Rebase overlay to new backing or flatten (diskRef, newBackingRef, unsafe)
    ReqDiskRebase !Ref !(Maybe Ref) !Bool
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
  | -- | Edit VM properties (vmRef, cpuCount, ramMb, description, headless, guestAgent, cloudInit, autostart)
    -- Each Maybe field is updated only if Just.
    ReqVmEdit !Ref !(Maybe Int) !(Maybe Int) !(Maybe Text) !(Maybe Bool) !(Maybe Bool) !(Maybe Bool) !(Maybe Bool)
  | -- | Generate/regenerate cloud-init ISO for a VM (vmRef)
    ReqVmCloudInit !Ref
  | -- | Virtual network operations
    -- | Create network (name, subnet, dhcp, nat, autostart)
    ReqNetworkCreate !Text !Text !Bool !Bool !Bool
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
  | -- | Edit network properties (networkRef, subnet, dhcp, nat, autostart)
    -- Each Maybe field is updated only if Just.
    ReqNetworkEdit !Ref !(Maybe Text) !(Maybe Bool) !(Maybe Bool) !(Maybe Bool)
  | -- | Guest command execution (vmRef, command)
    ReqGuestExec !Ref !Text
  | -- | Import disk image from URL (name, url, optionalFormat)
    ReqDiskImportUrl !Text !Text !(Maybe Text)
  | -- | Apply environment from YAML config (yamlContent, skipExisting, wait)
    ReqApply !Text !Bool !Bool
  | -- | List task history (limit, optionalSubsystem, optionalResult, includeSubtasks)
    ReqTaskList !Int !(Maybe TaskSubsystem) !(Maybe TaskResult) !Bool
  | -- | Show single task details (taskId)
    ReqTaskShow !Int64
  | -- | List subtasks for a parent task (parentTaskId)
    ReqTaskListChildren !Int64
  | -- | Set/update cloud-init config for a VM (vmRef, userData, networkConfig, injectSshKeys)
    ReqCloudInitSet !Ref !(Maybe Text) !(Maybe Text) !Bool
  | -- | Get cloud-init config for a VM (vmRef)
    ReqCloudInitGet !Ref
  | -- | Delete custom cloud-init config for a VM (vmRef)
    ReqCloudInitDelete !Ref
  | -- | Attach to serial console (vmRef). After RespSerialConsoleOk,
    -- the connection switches to raw byte streaming.
    ReqSerialConsole !Ref
  | -- | Flush (clear) the serial console ring buffer for a VM (vmRef)
    ReqSerialConsoleFlush !Ref
  | -- | Import disk image from source (local path or URL) with copy to destination
    -- (name, source, optionalDestPath, optionalFormat, wait)
    ReqDiskImport !Text !Text !(Maybe Text) !(Maybe Text) !Bool
  | -- | Update (replace) an existing template atomically with new YAML (templateRef, yaml)
    ReqTemplateUpdate !Ref !Text
  deriving (Eq, Show, Generic, Binary)

-- | Status information returned by the server.
--
-- Daemon-level metadata — uptime, version, namespace PID — not tied to
-- any one subsystem, so it stays in the umbrella module.
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

instance ToJSON StatusInfo where
  toJSON s =
    object
      [ "uptime" .= siUptime s
      , "connections" .= siConnections s
      , "version" .= siVersion s
      , "namespacePid" .= siNamespacePid s
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
  | -- | Network edited successfully
    RespNetworkEdited
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
  | -- | Apply started asynchronously (parent task ID)
    RespApplyStarted !Int64
  | -- | Task history list
    RespTaskList ![TaskInfo]
  | -- | Single task info
    RespTaskInfo !TaskInfo
  | -- | Task not found
    RespTaskNotFound
  | -- | Cloud-init config (Nothing = using defaults)
    RespCloudInitConfig !(Maybe CloudInitInfo)
  | -- | Cloud-init config operation successful
    RespCloudInitOk
  | -- | Serial console attached; connection switches to raw byte streaming
    RespSerialConsoleOk
  | -- | Serial console buffer flushed
    RespSerialConsoleFlushed
  | -- | Daemon startup completed
    RespStartupComplete
  | -- | Daemon shutdown completed
    RespShutdownComplete
  | -- | Generic success for internal operations
    RespOk
  | -- | Disk import started asynchronously (task ID)
    RespDiskImportStarted !Int64
  | -- | Template updated successfully (new template ID)
    RespTemplateUpdated !Int64
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
