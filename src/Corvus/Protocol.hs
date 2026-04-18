{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Request / Response sum types for the Corvus RPC protocol.
--
-- Constructors use record syntax so both the binary wire format and the
-- JSON projection (for the Python bridge) can be derived from a single
-- type. 'Data.Binary' only cares about field *order*; 'Aeson' only cares
-- about field *names*; both coexist.
--
-- 'DuplicateRecordFields' lets the same field name appear in multiple
-- constructors (e.g. @ref@, @name@) and in both the 'Request' and
-- 'Response' types. Existing callers pattern-match positionally and
-- continue to work unchanged.
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
  , unRef
  )
where

import Corvus.Model (CacheType, DriveFormat, DriveInterface, DriveMedia, NetInterfaceType, SharedDirCache, TaskResult, TaskSubsystem, VmStatus)
import Corvus.Protocol.Aeson (innerOptions, requestOptions, responseOptions)
import Corvus.Protocol.Apply
import Corvus.Protocol.CloudInit
import Corvus.Protocol.Disk
import Corvus.Protocol.Network
import Corvus.Protocol.SharedDir
import Corvus.Protocol.SshKey
import Corvus.Protocol.Task
import Corvus.Protocol.Template
import Corvus.Protocol.Vm
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
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
newtype Ref = Ref Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (Binary)

-- | Unwrap a 'Ref' to its underlying 'Text'. Defined explicitly because
-- 'NoFieldSelectors' in this module suppresses auto-generated selectors.
unRef :: Ref -> Text
unRef (Ref t) = t

instance ToJSON Ref where
  toJSON (Ref t) = toJSON t

instance FromJSON Ref where
  parseJSON v = Ref <$> parseJSON v

-- | Current protocol version. Increment when the wire format changes.
protocolVersion :: Word8
protocolVersion = 30

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | Client requests.
--
-- Each non-nullary constructor uses record syntax. Field order matches the
-- previous positional declarations so 'Data.Binary' wire encoding stays
-- unchanged. Field *names* exist purely for JSON serialisation and for
-- code generation of the Python client; existing callers pattern-match
-- positionally and continue to work unmodified.
data Request
  = ReqPing
  | ReqStatus
  | ReqShutdown
  | ReqVmList
  | -- | Show VM
    ReqVmShow {ref :: !Ref}
  | -- | Create VM
    ReqVmCreate
      { name :: !Text
      , cpuCount :: !Int
      , ramMb :: !Int
      , description :: !(Maybe Text)
      , headless :: !Bool
      , guestAgent :: !Bool
      , cloudInit :: !Bool
      , autostart :: !Bool
      }
  | -- | Delete VM
    ReqVmDelete {ref :: !Ref, deleteDisks :: !Bool}
  | -- | Start VM (stopped/paused → running). @wait@ blocks until complete.
    ReqVmStart {ref :: !Ref, wait :: !Bool}
  | -- | Stop VM (running → stopped). @wait@ blocks until complete.
    ReqVmStop {ref :: !Ref, wait :: !Bool}
  | -- | Pause VM (running → paused)
    ReqVmPause {ref :: !Ref}
  | -- | Reset VM (any → stopped)
    ReqVmReset {ref :: !Ref}
  | -- | Create disk image
    ReqDiskCreate
      { name :: !Text
      , format :: !DriveFormat
      , sizeMb :: !Int64
      , path :: !(Maybe Text)
      }
  | -- | Register an existing on-disk image file
    ReqDiskRegister
      { name :: !Text
      , filePath :: !Text
      , formatHint :: !(Maybe DriveFormat)
      , backing :: !(Maybe Ref)
      }
  | -- | Create overlay disk image
    ReqDiskCreateOverlay
      { name :: !Text
      , base :: !Ref
      , path :: !(Maybe Text)
      }
  | -- | Refresh disk image size from qemu-img info
    ReqDiskRefresh {ref :: !Ref}
  | -- | Delete disk image
    ReqDiskDelete {ref :: !Ref}
  | -- | Resize disk image
    ReqDiskResize {ref :: !Ref, newSizeMb :: !Int64}
  | -- | List all disk images
    ReqDiskList
  | -- | Show disk image details
    ReqDiskShow {ref :: !Ref}
  | -- | Clone disk image
    ReqDiskClone
      { name :: !Text
      , base :: !Ref
      , path :: !(Maybe Text)
      }
  | -- | Rebase overlay to new backing or flatten
    ReqDiskRebase
      { ref :: !Ref
      , backing :: !(Maybe Ref)
      , unsafe :: !Bool
      }
  | -- | Create snapshot (qcow2 only)
    ReqSnapshotCreate {ref :: !Ref, name :: !Text}
  | -- | Delete snapshot
    ReqSnapshotDelete {ref :: !Ref, snapshotRef :: !Ref}
  | -- | Rollback to snapshot
    ReqSnapshotRollback {ref :: !Ref, snapshotRef :: !Ref}
  | -- | Merge snapshot
    ReqSnapshotMerge {ref :: !Ref, snapshotRef :: !Ref}
  | -- | List snapshots
    ReqSnapshotList {ref :: !Ref}
  | -- | Attach disk to VM
    ReqDiskAttach
      { vmRef :: !Ref
      , diskRef :: !Ref
      , interface :: !DriveInterface
      , media :: !(Maybe DriveMedia)
      , readOnly :: !Bool
      , discard :: !Bool
      , diskCache :: !CacheType
      }
  | -- | Detach disk from VM
    ReqDiskDetach {vmRef :: !Ref, diskRef :: !Ref}
  | -- | Add shared directory to VM
    ReqSharedDirAdd
      { ref :: !Ref
      , hostPath :: !Text
      , tag :: !Text
      , dirCache :: !SharedDirCache
      , readOnly :: !Bool
      }
  | -- | Remove shared directory from VM
    ReqSharedDirRemove {ref :: !Ref, sharedDirRef :: !Ref}
  | -- | List shared directories for VM
    ReqSharedDirList {ref :: !Ref}
  | -- | Add network interface to VM
    ReqNetIfAdd
      { ref :: !Ref
      , interfaceType :: !NetInterfaceType
      , hostDevice :: !Text
      , macAddress :: !(Maybe Text)
      , network :: !(Maybe Ref)
      }
  | -- | Remove network interface from VM
    ReqNetIfRemove {ref :: !Ref, netIfId :: !Int64}
  | -- | List network interfaces for VM
    ReqNetIfList {ref :: !Ref}
  | -- | Create SSH key
    ReqSshKeyCreate {name :: !Text, publicKey :: !Text}
  | -- | Delete SSH key
    ReqSshKeyDelete {ref :: !Ref}
  | -- | List all SSH keys
    ReqSshKeyList
  | -- | Attach SSH key to VM
    ReqSshKeyAttach {vmRef :: !Ref, keyRef :: !Ref}
  | -- | Detach SSH key from VM
    ReqSshKeyDetach {vmRef :: !Ref, keyRef :: !Ref}
  | -- | List SSH keys for VM
    ReqSshKeyListForVm {ref :: !Ref}
  | -- | Create template from YAML
    ReqTemplateCreate {yaml :: !Text}
  | -- | Delete template
    ReqTemplateDelete {ref :: !Ref}
  | -- | List all templates
    ReqTemplateList
  | -- | Show template details
    ReqTemplateShow {ref :: !Ref}
  | -- | Instantiate template
    ReqTemplateInstantiate {ref :: !Ref, name :: !Text}
  | -- | Edit VM properties (each Maybe is applied only if Just).
    ReqVmEdit
      { ref :: !Ref
      , newCpuCount :: !(Maybe Int)
      , newRamMb :: !(Maybe Int)
      , newDescription :: !(Maybe Text)
      , newHeadless :: !(Maybe Bool)
      , newGuestAgent :: !(Maybe Bool)
      , newCloudInit :: !(Maybe Bool)
      , newAutostart :: !(Maybe Bool)
      }
  | -- | Regenerate cloud-init ISO for a VM
    ReqVmCloudInit {ref :: !Ref}
  | -- | Create virtual network
    ReqNetworkCreate
      { name :: !Text
      , subnet :: !Text
      , dhcp :: !Bool
      , nat :: !Bool
      , autostart :: !Bool
      }
  | -- | Delete network
    ReqNetworkDelete {ref :: !Ref}
  | -- | Start network
    ReqNetworkStart {ref :: !Ref}
  | -- | Stop network
    ReqNetworkStop {ref :: !Ref, force :: !Bool}
  | -- | List all networks
    ReqNetworkList
  | -- | Show network details
    ReqNetworkShow {ref :: !Ref}
  | -- | Edit network (each Maybe applied only if Just)
    ReqNetworkEdit
      { ref :: !Ref
      , newSubnet :: !(Maybe Text)
      , newDhcp :: !(Maybe Bool)
      , newNat :: !(Maybe Bool)
      , newAutostart :: !(Maybe Bool)
      }
  | -- | Execute command in guest via qemu-guest-agent
    ReqGuestExec {ref :: !Ref, command :: !Text}
  | -- | Import disk image from URL
    ReqDiskImportUrl
      { name :: !Text
      , url :: !Text
      , formatText :: !(Maybe Text)
      }
  | -- | Apply environment from YAML config
    ReqApply
      { yaml :: !Text
      , skipExisting :: !Bool
      , wait :: !Bool
      }
  | -- | List task history
    ReqTaskList
      { limit :: !Int
      , subsystem :: !(Maybe TaskSubsystem)
      , result :: !(Maybe TaskResult)
      , includeSubtasks :: !Bool
      }
  | -- | Show single task details
    ReqTaskShow {taskId :: !Int64}
  | -- | List subtasks of a parent task
    ReqTaskListChildren {parentId :: !Int64}
  | -- | Set/update cloud-init config for a VM
    ReqCloudInitSet
      { ref :: !Ref
      , userData :: !(Maybe Text)
      , networkConfig :: !(Maybe Text)
      , injectSshKeys :: !Bool
      }
  | -- | Get cloud-init config for a VM
    ReqCloudInitGet {ref :: !Ref}
  | -- | Delete custom cloud-init config for a VM
    ReqCloudInitDelete {ref :: !Ref}
  | -- | Attach to serial console (protocol upgrade after RespSerialConsoleOk).
    ReqSerialConsole {ref :: !Ref}
  | -- | Flush the serial console ring buffer for a VM
    ReqSerialConsoleFlush {ref :: !Ref}
  | -- | Import disk image from source (local path or URL) with copy.
    ReqDiskImport
      { name :: !Text
      , source :: !Text
      , path :: !(Maybe Text)
      , formatText :: !(Maybe Text)
      , wait :: !Bool
      }
  | -- | Replace an existing template atomically with new YAML
    ReqTemplateUpdate {ref :: !Ref, yaml :: !Text}
  deriving (Eq, Show, Generic, Binary)

instance ToJSON Request where
  toJSON = genericToJSON requestOptions

instance FromJSON Request where
  parseJSON = genericParseJSON requestOptions

-- ---------------------------------------------------------------------------
-- Status
-- ---------------------------------------------------------------------------

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
  -- ^ Daemon version (package version + short git commit hash)
  , siProtocolVersion :: !Word8
  -- ^ Binary RPC protocol version
  , siNamespacePid :: !(Maybe Int)
  -- ^ PID of the network namespace manager
  }
  deriving (Eq, Show, Generic, Binary)

instance ToJSON StatusInfo where
  toJSON = genericToJSON innerOptions

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Server responses.
--
-- Record syntax throughout (except nullary constructors). Field order
-- preserves the original positional declarations so 'Data.Binary'
-- continues to produce byte-identical output.
data Response
  = RespPong
  | RespStatus {info :: !StatusInfo}
  | RespShutdownAck {ack :: !Bool}
  | RespError {message :: !Text}
  | RespVmList {vms :: ![VmInfo]}
  | RespVmDetails {details :: !VmDetails}
  | RespVmNotFound
  | -- | VM created successfully
    RespVmCreated {id :: !Int64}
  | -- | VM deleted successfully
    RespVmDeleted
  | -- | VM is running and cannot be deleted
    RespVmRunning
  | -- | New status after successful transition
    RespVmStateChanged {status :: !VmStatus}
  | -- | Current status and reason for rejection
    RespInvalidTransition {status :: !VmStatus, reason :: !Text}
  | -- | List of disk images
    RespDiskList {disks :: ![DiskImageInfo]}
  | -- | Single disk image info
    RespDiskInfo {disk :: !DiskImageInfo}
  | -- | Disk created successfully
    RespDiskCreated {id :: !Int64}
  | -- | Disk image not found
    RespDiskNotFound
  | -- | Disk operation successful
    RespDiskOk
  | -- | List of snapshots
    RespSnapshotList {snapshots :: ![SnapshotInfo]}
  | -- | Snapshot created successfully
    RespSnapshotCreated {id :: !Int64}
  | -- | Snapshot not found
    RespSnapshotNotFound
  | -- | Snapshot operation successful
    RespSnapshotOk
  | -- | Drive attached successfully
    RespDiskAttached {id :: !Int64}
  | -- | Drive not found
    RespDriveNotFound
  | -- | Operation not supported for this format
    RespFormatNotSupported {message :: !Text}
  | -- | VM must be stopped for this operation
    RespVmMustBeStopped
  | -- | Disk is still attached to VMs
    RespDiskInUse {attachedVms :: ![(Int64, Text)]}
  | -- | Disk is used as backing image for overlays
    RespDiskHasOverlays {overlays :: ![(Int64, Text)]}
  | -- | List of shared directories
    RespSharedDirList {sharedDirs :: ![SharedDirInfo]}
  | -- | Shared directory added
    RespSharedDirAdded {id :: !Int64}
  | -- | Shared directory operation successful
    RespSharedDirOk
  | -- | Shared directory not found
    RespSharedDirNotFound
  | -- | List of network interfaces
    RespNetIfList {netIfs :: ![NetIfInfo]}
  | -- | Network interface added
    RespNetIfAdded {id :: !Int64}
  | -- | Network interface operation successful
    RespNetIfOk
  | -- | Network interface not found
    RespNetIfNotFound
  | -- | List of SSH keys
    RespSshKeyList {sshKeys :: ![SshKeyInfo]}
  | -- | SSH key created
    RespSshKeyCreated {id :: !Int64}
  | -- | SSH key operation successful
    RespSshKeyOk
  | -- | SSH key not found
    RespSshKeyNotFound
  | -- | SSH key is in use by VMs
    RespSshKeyInUse {usedByVms :: ![(Int64, Text)]}
  | -- | List of templates
    RespTemplateList {templates :: ![TemplateVmInfo]}
  | -- | Single template info
    RespTemplateInfo {template :: !TemplateDetails}
  | -- | Template created successfully
    RespTemplateCreated {id :: !Int64}
  | -- | Template not found
    RespTemplateNotFound
  | -- | Template deleted successfully
    RespTemplateDeleted
  | -- | Template instantiated successfully (new VM id)
    RespTemplateInstantiated {id :: !Int64}
  | -- | VM edited successfully
    RespVmEdited
  | -- | Network created successfully
    RespNetworkCreated {id :: !Int64}
  | -- | Network deleted successfully
    RespNetworkDeleted
  | -- | Network started successfully
    RespNetworkStarted
  | -- | Network stopped successfully
    RespNetworkStopped
  | -- | List of networks
    RespNetworkList {networks :: ![NetworkInfo]}
  | -- | Single network info
    RespNetworkDetails {network :: !NetworkInfo}
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
    RespNetworkError {message :: !Text}
  | -- | Guest command execution result
    RespGuestExecResult
      { exitCode :: !Int
      , stdout :: !Text
      , stderr :: !Text
      }
  | -- | Guest agent not enabled on this VM
    RespGuestAgentNotEnabled
  | -- | Guest agent communication error
    RespGuestAgentError {message :: !Text}
  | -- | Apply config result with summary of created resources
    RespApplyResult {result :: !ApplyResult}
  | -- | Apply started asynchronously (parent task id)
    RespApplyStarted {taskId :: !Int64}
  | -- | Task history list
    RespTaskList {tasks :: ![TaskInfo]}
  | -- | Single task info
    RespTaskInfo {task :: !TaskInfo}
  | -- | Task not found
    RespTaskNotFound
  | -- | Cloud-init config (Nothing = using defaults)
    RespCloudInitConfig {config :: !(Maybe CloudInitInfo)}
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
  | -- | Disk import started asynchronously
    RespDiskImportStarted {taskId :: !Int64}
  | -- | Template updated successfully
    RespTemplateUpdated {id :: !Int64}
  deriving (Eq, Show, Generic, Binary)

instance ToJSON Response where
  toJSON = genericToJSON responseOptions

-- Note: 'FromJSON Response' is not derived because most inner record
-- types currently only have 'ToJSON' (they flow server→client only).
-- When / if round-trip Response tests are added, derive 'FromJSON' on
-- those inner types first.

-- ---------------------------------------------------------------------------
-- Wire encoding
-- ---------------------------------------------------------------------------

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
