{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Daemon-internal Response sum type plus the leaf info records.
--
-- The 'Response' sum is still useful inside the daemon: handlers
-- return it from @Action@ instances, and the Cap'n Proto cap glue in
-- @Corvus.Rpc.*@ pattern-matches on it to decide what to push onto
-- the wire (struct return / typed exception). It is no longer
-- serialised — the legacy 'Data.Binary' wire is gone.
--
-- 'Ref' likewise survives as the daemon-internal "name or id" handle
-- consumed by @Corvus.Handlers.Resolve@. Clients now send a
-- 'Corvus.Wire.Common.EntityRef' union; the Cap'n Proto glue in
-- 'Corvus.Rpc.Common.capnpRefToRef' translates that into a 'Ref'.
module Corvus.Protocol
  ( -- * Server-side response sum
    Response (..)

    -- * Daemon status
  , StatusInfo (..)

    -- * Per-subsystem response data (re-exported for convenience)
  , module Corvus.Protocol.Vm
  , module Corvus.Protocol.Disk
  , module Corvus.Protocol.SharedDir
  , module Corvus.Protocol.SshKey
  , module Corvus.Protocol.Template
  , module Corvus.Protocol.Network
  , module Corvus.Protocol.Node
  , module Corvus.Protocol.CloudInit
  , module Corvus.Protocol.Apply
  , module Corvus.Protocol.Build
  , module Corvus.Protocol.Task

    -- * Entity reference
  , Ref (..)
  , unRef
  )
where

import Corvus.Model (VmStatus)
import Corvus.Protocol.Apply
import Corvus.Protocol.Build
import Corvus.Protocol.CloudInit
import Corvus.Protocol.Disk
import Corvus.Protocol.JsonOptions (innerOptions)
import Corvus.Protocol.Network
import Corvus.Protocol.Node
import Corvus.Protocol.SharedDir
import Corvus.Protocol.SshKey
import Corvus.Protocol.Task
import Corvus.Protocol.Template
import Corvus.Protocol.Vm
import Data.Aeson (FromJSON (..), ToJSON (..), genericToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A reference to an entity by name or numeric ID.
-- If the text parses as an integer, it is treated as an ID lookup.
-- Otherwise it is treated as a name lookup via the entity's unique constraint.
newtype Ref = Ref Text
  deriving stock (Eq, Show, Generic)

-- | Unwrap a 'Ref' to its underlying 'Text'. Defined explicitly because
-- 'NoFieldSelectors' in this module suppresses auto-generated selectors.
unRef :: Ref -> Text
unRef (Ref t) = t

instance ToJSON Ref where
  toJSON (Ref t) = toJSON t

instance FromJSON Ref where
  parseJSON v = Ref <$> parseJSON v

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
  , siProtocolVersion :: !Int
  -- ^ RPC protocol version (Cap'n Proto schema generation number)
  }
  deriving (Eq, Show, Generic)

instance ToJSON StatusInfo where
  toJSON = genericToJSON innerOptions

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Daemon-internal response sum.
--
-- Each constructor's record syntax exists for clarity at construction
-- sites — fields are not serialised. The Cap'n Proto cap layer
-- (@Corvus.Rpc.*@) pattern-matches on these to decide which struct
-- field to populate or which exception to throw.
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
  | -- | A peer node was attached to a multi-node network
    RespNetworkPeerAttached
  | -- | A peer node was detached from a multi-node network
    RespNetworkPeerDetached
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
  | -- | Serial console attached (post-upgrade)
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
  | -- | Disk copy / move kicked off asynchronously; @taskId@
    -- references the long-running task row.
    RespDiskTransferStarted {taskId :: !Int64}
  | -- | Template updated successfully
    RespTemplateUpdated {id :: !Int64}
  | -- | SPICE connection parameters with an ephemeral password.
    RespVmViewGrant
      { host :: !Text
      , port :: !Int
      , password :: !Text
      , ttlSeconds :: !Int
      }
  | -- | VM is not in a state where SPICE is available (e.g. stopped).
    RespVmNotRunning
  | -- | VM has no SPICE console (headless configuration).
    RespVmHeadless
  | -- | HMP monitor attached (post-upgrade).
    RespHmpMonitorOk
  | -- | HMP monitor buffer flushed.
    RespHmpMonitorFlushed
  | -- | Build pipeline result (one entry per build in the request).
    RespBuildResult {buildResult :: !BuildResult}
  | -- | Build started asynchronously (parent task id, mirrors RespApplyStarted).
    RespBuildStarted {taskId :: !Int64}
  | -- | Build accepted with @--wait@; events stream over a separate
    -- 'BuildEventSink' cap (Phase 6).
    RespBuildStreamStarted
  | -- | List of nodes
    RespNodeList {nodes :: ![NodeInfo]}
  | -- | Single node details
    RespNodeDetails {node :: !NodeDetails}
  | -- | Node created successfully
    RespNodeCreated {id :: !Int64}
  | -- | Node deleted successfully
    RespNodeDeleted
  | -- | Node edited successfully
    RespNodeEdited
  | -- | Node not found
    RespNodeNotFound
  | -- | Node is still referenced by VMs / networks / disks
    RespNodeInUse {message :: !Text}
  deriving (Eq, Show, Generic)
