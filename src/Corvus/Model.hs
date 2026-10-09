{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Corvus.Model
  ( -- * Database schema
    migrateAll

    -- * Entities
  , Node (..)
  , Vm (..)
  , Drive (..)
  , NetworkInterface (..)
  , DiskImageTag (..)
  , DiskImageUploadIdentity (..)
  , DiskImageUploadIdentityId
  , DiskImageImportIdentity (..)
  , DiskImageImportIdentityId
  , DiskImageBuildIdentity (..)
  , DiskImageBuildIdentityId
  , DiskImageTagId
  , DiskImage (..)
  , DiskImageNode (..)
  , Snapshot (..)

    -- * Entity IDs
  , NodeId
  , VmId
  , DriveId
  , NetworkInterfaceId
  , DiskImageId
  , DiskImageNodeId
  , SnapshotId

    -- * Entity Fields (for queries)
  , EntityField (..)

    -- * Enums
  , NodeAdminState (..)
  , VmStatus (..)
  , DriveInterface (..)
  , DriveFormat (..)
  , DriveMedia (..)
  , CacheType (..)
  , NetInterfaceType (..)
  , SharedDirCache (..)
  , AudioBackend (..)
  , AudioDeviceModel (..)
  , NetworkDeviceModel (..)
  , GraphicsAdapter (..)
  , TemplateCloneStrategy (..)
  , Network (..)
  , NetworkId
  , NetworkPeer (..)
  , NetworkPeerId
  , SharedDir (..)
  , SharedDirId
  , AudioDevice (..)
  , AudioDeviceId
  , SshKey (..)
  , SshKeyId
  , VmSshKey (..)
  , VmSshKeyId
  , TemplateVm (..)
  , TemplateVmId
  , TemplateDrive (..)
  , TemplateDriveId
  , TemplateNetworkInterface (..)
  , TemplateNetworkInterfaceId
  , TemplateSshKey (..)
  , TemplateSshKeyId
  , TemplateSharedDir (..)
  , TemplateSharedDirId
  , TemplateAudioDevice (..)
  , TemplateAudioDeviceId
  , Task (..)
  , TaskId
  , CloudInit (..)
  , CloudInitId
  , TemplateCloudInit (..)
  , TemplateCloudInitId

    -- * Task enums
  , TaskSubsystem (..)
  , TaskResult (..)

    -- * Unique constraints
  , Unique (..)

    -- * Enum conversion type class
  , EnumText (..)

    -- * Re-exports for convenience
  , Entity (..)
  , Key
  , toSqlKey
  , fromSqlKey
  )
where

import Corvus.Model.EnumText
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql (PersistFieldSql (..), SqlType (..), fromSqlKey, toSqlKey)
import Database.Persist.TH
import GHC.Generics (Generic)

data VmStatus
  = VmStopped
  | VmStarting
  | VmRunning
  | VmStopping
  | VmPaused
  | VmSaved
  | VmError
  | VmSaving
  | VmLoading
  | VmMigrating
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText VmStatus where
  enumTypeName = "VmStatus"
  enumMapping =
    [ (VmStopped, "stopped")
    , (VmStarting, "starting")
    , (VmRunning, "running")
    , (VmStopping, "stopping")
    , (VmPaused, "paused")
    , (VmSaved, "saved")
    , (VmError, "error")
    , (VmSaving, "saving")
    , (VmLoading, "loading")
    , (VmMigrating, "migrating")
    ]

instance FromJSON VmStatus where
  parseJSON = parseEnumJSON

instance ToJSON VmStatus where
  toJSON = toEnumJSON

instance PersistField VmStatus where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql VmStatus where
  sqlType _ = SqlString

--------------------------------------------------------------------------------
-- DriveInterface
--------------------------------------------------------------------------------

data DriveInterface
  = InterfaceVirtio
  | InterfaceIde
  | InterfaceScsi
  | InterfaceSata
  | InterfaceNvme
  | InterfacePflash
  | InterfaceFloppy
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText DriveInterface where
  enumTypeName = "DriveInterface"
  enumMapping =
    [ (InterfaceVirtio, "virtio")
    , (InterfaceIde, "ide")
    , (InterfaceScsi, "scsi")
    , (InterfaceSata, "sata")
    , (InterfaceNvme, "nvme")
    , (InterfacePflash, "pflash")
    , (InterfaceFloppy, "floppy")
    ]

instance FromJSON DriveInterface where
  parseJSON = parseEnumJSON

instance ToJSON DriveInterface where
  toJSON = toEnumJSON

instance PersistField DriveInterface where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql DriveInterface where
  sqlType _ = SqlString

--------------------------------------------------------------------------------
-- DriveFormat
--------------------------------------------------------------------------------

data DriveFormat
  = FormatQcow2
  | FormatRaw
  | FormatVmdk
  | FormatVdi
  | FormatVpc
  | FormatVhdx
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText DriveFormat where
  enumTypeName = "DriveFormat"
  enumMapping =
    [ (FormatQcow2, "qcow2")
    , (FormatRaw, "raw")
    , (FormatVmdk, "vmdk")
    , (FormatVdi, "vdi")
    , (FormatVpc, "vpc")
    , (FormatVhdx, "vhdx")
    ]

instance FromJSON DriveFormat where
  parseJSON = parseEnumJSON

instance ToJSON DriveFormat where
  toJSON = toEnumJSON

instance PersistField DriveFormat where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql DriveFormat where
  sqlType _ = SqlString

--------------------------------------------------------------------------------
-- CacheType
--------------------------------------------------------------------------------

data CacheType
  = CacheNone
  | CacheWriteback
  | CacheWritethrough
  | CacheDirectsync
  | CacheUnsafe
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText CacheType where
  enumTypeName = "CacheType"
  enumMapping =
    [ (CacheNone, "none")
    , (CacheWriteback, "writeback")
    , (CacheWritethrough, "writethrough")
    , (CacheDirectsync, "directsync")
    , (CacheUnsafe, "unsafe")
    ]

instance FromJSON CacheType where
  parseJSON = parseEnumJSON

instance ToJSON CacheType where
  toJSON = toEnumJSON

instance PersistField CacheType where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql CacheType where
  sqlType _ = SqlString

--------------------------------------------------------------------------------
-- DriveMedia
--------------------------------------------------------------------------------

data DriveMedia
  = MediaDisk
  | MediaCdrom
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText DriveMedia where
  enumTypeName = "DriveMedia"
  enumMapping =
    [ (MediaDisk, "disk")
    , (MediaCdrom, "cdrom")
    ]

instance FromJSON DriveMedia where
  parseJSON = parseEnumJSON

instance ToJSON DriveMedia where
  toJSON = toEnumJSON

instance PersistField DriveMedia where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql DriveMedia where
  sqlType _ = SqlString

--------------------------------------------------------------------------------
-- NetInterfaceType
--------------------------------------------------------------------------------

data NetInterfaceType
  = NetUser
  | NetTap
  | NetBridge
  | NetMacvtap
  | NetVde
  | NetManaged
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText NetInterfaceType where
  enumTypeName = "NetInterfaceType"
  enumMapping =
    [ (NetUser, "user")
    , (NetTap, "tap")
    , (NetBridge, "bridge")
    , (NetMacvtap, "macvtap")
    , (NetVde, "vde")
    , (NetManaged, "managed")
    ]

instance FromJSON NetInterfaceType where
  parseJSON = parseEnumJSON

instance ToJSON NetInterfaceType where
  toJSON = toEnumJSON

instance PersistField NetInterfaceType where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql NetInterfaceType where
  sqlType _ = SqlString

--------------------------------------------------------------------------------
-- SharedDirCache
--------------------------------------------------------------------------------

data SharedDirCache
  = CacheAlways
  | CacheAuto
  | CacheNever
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText SharedDirCache where
  enumTypeName = "SharedDirCache"
  enumMapping =
    [ (CacheAlways, "always")
    , (CacheAuto, "auto")
    , (CacheNever, "never")
    ]

instance FromJSON SharedDirCache where
  parseJSON = parseEnumJSON

instance ToJSON SharedDirCache where
  toJSON = toEnumJSON

instance PersistField SharedDirCache where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql SharedDirCache where
  sqlType _ = SqlString

data AudioBackend = AudioPulse | AudioPipewire | AudioSpice
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText AudioBackend where
  enumTypeName = "AudioBackend"
  enumMapping =
    [ (AudioPulse, "pulse")
    , (AudioPipewire, "pipewire")
    , (AudioSpice, "spice")
    ]

instance FromJSON AudioBackend where
  parseJSON = parseEnumJSON

instance ToJSON AudioBackend where
  toJSON = toEnumJSON

instance PersistField AudioBackend where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql AudioBackend where
  sqlType _ = SqlString

data AudioDeviceModel = AudioVirtioSound | AudioIntelHda | AudioIch9IntelHda | AudioAc97
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText AudioDeviceModel where
  enumTypeName = "AudioDeviceModel"
  enumMapping = [(AudioVirtioSound, "virtio-sound"), (AudioIntelHda, "intel-hda"), (AudioIch9IntelHda, "ich9-intel-hda"), (AudioAc97, "AC97")]

instance FromJSON AudioDeviceModel where
  parseJSON = parseEnumJSON
instance ToJSON AudioDeviceModel where
  toJSON = toEnumJSON
instance PersistField AudioDeviceModel where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue
instance PersistFieldSql AudioDeviceModel where
  sqlType _ = SqlString

data NetworkDeviceModel = NetworkVirtioNetPci | NetworkVirtioNetPciNonTransitional | NetworkVirtioNetPciTransitional | NetworkE1000
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText NetworkDeviceModel where
  enumTypeName = "NetworkDeviceModel"
  enumMapping = [(NetworkVirtioNetPci, "virtio-net-pci"), (NetworkVirtioNetPciNonTransitional, "virtio-net-pci-non-transitional"), (NetworkVirtioNetPciTransitional, "virtio-net-pci-transitional"), (NetworkE1000, "e1000")]

instance FromJSON NetworkDeviceModel where
  parseJSON = parseEnumJSON
instance ToJSON NetworkDeviceModel where
  toJSON = toEnumJSON
instance PersistField NetworkDeviceModel where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue
instance PersistFieldSql NetworkDeviceModel where
  sqlType _ = SqlString

data GraphicsAdapter
  = GraphicsVirtioVga
  | GraphicsQxlVga
  | GraphicsVga
  | GraphicsVirtioGpuPci
  | GraphicsVirtioVgaGl
  | GraphicsVirtioGpuGlPci
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText GraphicsAdapter where
  enumTypeName = "GraphicsAdapter"
  enumMapping =
    [ (GraphicsVirtioVga, "virtio-vga")
    , (GraphicsQxlVga, "qxl-vga")
    , (GraphicsVga, "vga")
    , (GraphicsVirtioGpuPci, "virtio-gpu-pci")
    , (GraphicsVirtioVgaGl, "virtio-vga-gl")
    , (GraphicsVirtioGpuGlPci, "virtio-gpu-gl-pci")
    ]

instance FromJSON GraphicsAdapter where
  parseJSON = parseEnumJSON

instance ToJSON GraphicsAdapter where
  toJSON = toEnumJSON

instance PersistField GraphicsAdapter where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql GraphicsAdapter where
  sqlType _ = SqlString

-- TemplateCloneStrategy

data TemplateCloneStrategy
  = StrategyClone
  | StrategyOverlay
  | StrategyDirect
  | StrategyCreate
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText TemplateCloneStrategy where
  enumTypeName = "TemplateCloneStrategy"
  enumMapping =
    [ (StrategyClone, "clone")
    , (StrategyOverlay, "overlay")
    , (StrategyDirect, "direct")
    , (StrategyCreate, "create")
    ]

instance FromJSON TemplateCloneStrategy where
  parseJSON = parseEnumJSON

instance ToJSON TemplateCloneStrategy where
  toJSON = toEnumJSON

instance PersistField TemplateCloneStrategy where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql TemplateCloneStrategy where
  sqlType _ = SqlString

-- TaskSubsystem

data TaskSubsystem
  = SubVm
  | SubDisk
  | SubNetwork
  | SubSshKey
  | SubTemplate
  | SubSharedDir
  | SubSnapshot
  | SubSystem
  | SubApply
  | SubBuild
  | SubNode
  | SubMigration
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText TaskSubsystem where
  enumTypeName = "TaskSubsystem"
  enumMapping =
    [ (SubVm, "vm")
    , (SubDisk, "disk")
    , (SubNetwork, "network")
    , (SubSshKey, "ssh-key")
    , (SubTemplate, "template")
    , (SubSharedDir, "shared-dir")
    , (SubSnapshot, "snapshot")
    , (SubSystem, "system")
    , (SubApply, "apply")
    , (SubBuild, "build")
    , (SubNode, "node")
    , (SubMigration, "migration")
    ]

instance FromJSON TaskSubsystem where
  parseJSON = parseEnumJSON

instance ToJSON TaskSubsystem where
  toJSON = toEnumJSON

instance PersistField TaskSubsystem where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql TaskSubsystem where
  sqlType _ = SqlString

-- TaskResult

data TaskResult
  = TaskRunning
  | TaskSuccess
  | TaskError
  | TaskNotStarted
  | TaskCancelled
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText TaskResult where
  enumTypeName = "TaskResult"
  enumMapping =
    [ (TaskRunning, "running")
    , (TaskSuccess, "success")
    , (TaskError, "error")
    , (TaskNotStarted, "not_started")
    , (TaskCancelled, "cancelled")
    ]

instance FromJSON TaskResult where
  parseJSON = parseEnumJSON

instance ToJSON TaskResult where
  toJSON = toEnumJSON

instance PersistField TaskResult where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql TaskResult where
  sqlType _ = SqlString

--------------------------------------------------------------------------------
-- NodeAdminState
--------------------------------------------------------------------------------

-- | Operator-controlled lifecycle state of a 'Node' row, independent
-- of whether its agents are currently reachable.
--
--   * 'NodeOnline' — eligible for the scheduler's pick-a-node pass.
--   * 'NodeDraining' — existing VMs keep running but no new VMs land here.
--   * 'NodeMaintenance' — operator hint that the node is intentionally
--     down; suppresses "agent unreachable" alerting when added.
data NodeAdminState
  = NodeOnline
  | NodeDraining
  | NodeMaintenance
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText NodeAdminState where
  enumTypeName = "NodeAdminState"
  enumMapping =
    [ (NodeOnline, "online")
    , (NodeDraining, "draining")
    , (NodeMaintenance, "maintenance")
    ]

instance FromJSON NodeAdminState where
  parseJSON = parseEnumJSON

instance ToJSON NodeAdminState where
  toJSON = toEnumJSON

instance PersistField NodeAdminState where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql NodeAdminState where
  sqlType _ = SqlString

-- Entity definitions

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Node
    name Text
    host Text
    nodeAgentPort Int
    netAgentPort Int
    basePath Text
    description Text Maybe default=NULL
    adminState NodeAdminState
    createdAt UTCTime
    -- Populated/refreshed by the agent push (Phase 5).
    cpuCount Int Maybe default=NULL
    ramTotal Int64 Maybe default=NULL
    ramFree Int64 Maybe default=NULL
    storageBytesTotal Int64 Maybe default=NULL
    storageBytesFree Int64 Maybe default=NULL
    loadAvg1 Double Maybe default=NULL
    loadAvg5 Double Maybe default=NULL
    loadAvg15 Double Maybe default=NULL
    kernelRelease Text Maybe default=NULL
    agentVersion Text Maybe default=NULL
    nodeAgentHealthcheck UTCTime Maybe default=NULL
    netAgentHealthcheck UTCTime Maybe default=NULL
    -- When true, the daemon skips the corvus-netd reconnect loop
    -- for this node and rejects netd-dependent NIC types
    -- (managed/tap/bridge/macvtap) and managed-network creation.
    -- Only user-mode and vde NICs are allowed.
    netdDisabled Bool default=false
    UniqueNodeName name
    UniqueNodeAddress host nodeAgentPort
    deriving Show Eq Generic

Vm
    name Text
    nodeId NodeId
    createdAt UTCTime
    status VmStatus
    -- Monotonically increasing daemon-owned fence for start/reset.
    -- It invalidates asynchronous work admitted by an older lifecycle
    -- operation without storing durable composite-operation context.
    lifecycleRevision Int64 default=0
    -- Identifies the runtime admitted by the current cold start. Cleared
    -- only after the matching reset/termination has been confirmed.
    runtimeGeneration Int64 Maybe default=NULL
    cpuCount Int
    ram Int64
    description Text Maybe
    headless Bool default=false
    guestAgent Bool default=false
    tpm Bool default=false
    cloudInit Bool default=false
    healthcheck UTCTime Maybe default=NULL
    autostart Bool default=false
    spicePort Int Maybe default=NULL
    vsockCid Int Maybe default=NULL
    vsock Bool default=true
    balloon Bool default=true
    rng Bool default=true
    errorMessage Text Maybe default=NULL
    lastErrorAt UTCTime Maybe default=NULL
    rebootQuirk Bool default=false
    cpuModel Text default='host'
    graphicsAdapter GraphicsAdapter default='virtio-vga'
    UniqueVmNamePerNode nodeId name
    deriving Show Eq Generic

DiskImageTag
    name Text
    tag Text
    diskImageId DiskImageId
    UniqueDiskImageTag name tag
    deriving Show Eq Generic

DiskImage
    name Text
    format DriveFormat
    size Int64 Maybe
    createdAt UTCTime
    backingImageId DiskImageId Maybe
    ephemeral Bool default=false
    deriving Show Eq Generic

-- Verified source identity; absent for older or non-checksummed imports.
DiskImageImportIdentity
    diskImageId DiskImageId
    algorithm Text
    digest Text
    target Text
    importUrl Text Maybe
    UniqueDiskImageImportIdentity diskImageId
    deriving Show Eq Generic

-- Verified SHA-256 of uploaded file bytes; absent for older uploads.
-- The source path is diagnostic and does not affect matching.
DiskImageUploadIdentity
    diskImageId DiskImageId
    digest Text
    sourcePath Text Maybe
    UniqueDiskImageUploadIdentity diskImageId
    deriving Show Eq Generic

-- Effective recipe and resolved source versions used to build this artifact.
-- Sources live in the JSON manifest, without foreign keys, so deleting an
-- old source version does not prevent retaining a flattened artifact.
DiskImageBuildIdentity
    diskImageId DiskImageId
    fingerprint Text
    inputs Text
    UniqueDiskImageBuildIdentity diskImageId
    deriving Show Eq Generic

-- Per-node paths for an image version. Attachment requires a placement
-- on the VM's node; paths may differ between nodes.
DiskImageNode
    diskImageId DiskImageId
    nodeId NodeId
    filePath Text
    UniqueDiskImageOnNode diskImageId nodeId
    UniqueDiskImagePathOnNode nodeId filePath
    UniqueDiskImagePathPerNode nodeId filePath
    deriving Show Eq Generic

Snapshot
    diskImageId DiskImageId
    name Text
    createdAt UTCTime
    size Int64 Maybe
    -- Whether this snapshot was taken against a running VM via QMP
    -- (`True`) versus offline via `qemu-img snapshot -c` (`False`).
    -- Pure operator diagnostic; the qcow2 entry is bit-identical
    -- either way. Defaults to `False` so existing rows from before
    -- the live-snapshot feature don't claim to be live.
    live Bool default=false
    -- Whether QGA `guest-fsfreeze-freeze` was active at the moment
    -- the snapshot was stamped. `True` implies the operator can
    -- trust filesystem-level consistency; `False` is
    -- hard-reset-equivalent for any unflushed in-guest writes.
    quiesced Bool default=false
    -- Whether this snapshot row carries QEMU vmstate (RAM + device
    -- model + CPU state) inside its qcow2. `True` only on the
    -- single "carrier" disk of a full-machine snapshot; the
    -- sibling rows that share the same `name` carry block snapshots
    -- alone. Restore of a carrier row routes through QMP
    -- `snapshot-load` (which resumes the VM in the saved running
    -- state) rather than offline `qemu-img snapshot -a`.
    hasVmstate Bool default=false
    UniqueSnapshot diskImageId name
    deriving Show Eq Generic

Drive
    vmId VmId
    diskImageId DiskImageId Maybe
    interface DriveInterface
    media DriveMedia Maybe
    readOnly Bool default=false
    cacheType CacheType
    discard Bool default=false
    -- !force: diskImageId is nullable; NULLs are treated as distinct so a VM
    -- may have several CD-ROM drives with no media (ejected tray) at once.
    UniqueDrive vmId diskImageId !force
    deriving Show Eq Generic

Network
    name Text
    nodeId NodeId
    subnet Text default=''
    dhcp Bool default=false
    nat Bool default=false
    running Bool default=false
    dnsmasqPid Int Maybe
    createdAt UTCTime
    autostart Bool default=false
    -- VXLAN VNI for multi-node overlays. NULL while the network has
    -- no peer nodes (single-node behavior); allocated on first
    -- attach-node and reused for the network's lifetime.
    vni Int Maybe default=NULL
    -- DNS servers advertised via DHCP option 6, comma-joined
    -- (e.g. "1.1.1.1,8.8.8.8"). Empty string means no DNS option
    -- is emitted by dnsmasq, preserving the original behavior.
    dnsServers Text default=''
    -- DNS suffix dnsmasq is authoritative for (e.g. "corvus").
    -- Empty string means "derive from the network's name at apply
    -- time" so the default is implicit and survives a network
    -- rename. Set explicitly via `crv network create --domain` or
    -- YAML `domain:`.
    domain Text default=''
    -- Whether the agent installs a systemd-resolved drop-in on
    -- the owner host pointing `*.<domain>` at the bridge IP. True
    -- by default; the operator opts out with `--no-host-dns` /
    -- `hostDns: false` when they manage host DNS themselves.
    hostDns Bool default=true
    UniqueNetworkPerNode nodeId name
    deriving Show Eq Generic

NetworkPeer
    networkId NetworkId
    nodeId NodeId
    UniqueNetworkPeer networkId nodeId
    deriving Show Eq Generic

NetworkInterface
    vmId VmId
    interfaceType NetInterfaceType
    model NetworkDeviceModel default='virtio-net-pci'
    hostDevice Text
    macAddress Text
    networkId NetworkId Maybe
    guestIpAddresses Text Maybe default=NULL
    -- v4 address allocated by the daemon's IPAM and reserved on
    -- dnsmasq via --dhcp-host. Populated when the NIC is attached to
    -- a managed network; NULL for unmanaged interfaces. Distinct
    -- from 'guestIpAddresses' which reflects what the guest agent
    -- observes; this one is the daemon's intent.
    ipAddress Text Maybe default=NULL
    deriving Show Eq Generic

SharedDir
    vmId VmId
    path Text
    tag Text
    cache SharedDirCache
    readOnly Bool default=false
    UniqueSharedDirTag vmId tag
    deriving Show Eq Generic

AudioDevice
    vmId VmId
    backend AudioBackend
    model AudioDeviceModel default='virtio-sound'
    options Text default=''
    deriving Show Eq Generic

SshKey
    name Text
    publicKey Text
    createdAt UTCTime
    UniqueSshKeyName name
    deriving Show Eq Generic

VmSshKey
    vmId VmId
    sshKeyId SshKeyId
    UniqueVmSshKey vmId sshKeyId
    deriving Show Eq Generic

TemplateVm
    name Text
    cpuCount Int
    ram Int64
    description Text Maybe
    headless Bool default=false
    cloudInit Bool default=false
    guestAgent Bool default=false
    tpm Bool default=false
    autostart Bool default=false
    rebootQuirk Bool default=false
    createdAt UTCTime
    graphicsAdapter GraphicsAdapter default='virtio-vga'
    vsock Bool default=true
    balloon Bool default=true
    rng Bool default=true
    UniqueTemplateVmName name
    deriving Show Eq Generic

TemplateDrive
    templateId TemplateVmId
    diskImageId DiskImageId Maybe
    diskName Text Maybe
    diskTag Text Maybe
    interface DriveInterface
    media DriveMedia Maybe
    readOnly Bool default=false
    cacheType CacheType
    discard Bool default=false
    cloneStrategy TemplateCloneStrategy
    size Int64 Maybe
    format DriveFormat Maybe
    ephemeral Bool Maybe
    deriving Show Eq Generic

TemplateNetworkInterface
    templateId TemplateVmId
    interfaceType NetInterfaceType
    model NetworkDeviceModel default='virtio-net-pci'
    hostDevice Text Maybe
    -- Managed-network NICs reference the network by name; the
    -- instantiation path resolves this to a NetworkId on the new
    -- VM's NetworkInterface row. Stored by name (not id) so the
    -- template doesn't carry a foreign key into the live Network
    -- table — networks can be created/destroyed independently.
    networkName Text Maybe
    deriving Show Eq Generic

TemplateSshKey
    templateId TemplateVmId
    sshKeyId SshKeyId
    UniqueTemplateSshKey templateId sshKeyId
    deriving Show Eq Generic

TemplateSharedDir
    templateId TemplateVmId
    path Text
    tag Text
    cache SharedDirCache
    readOnly Bool default=false
    UniqueTemplateSharedDirTag templateId tag
    deriving Show Eq Generic

TemplateAudioDevice
    templateId TemplateVmId
    backend AudioBackend
    model AudioDeviceModel default='virtio-sound'
    options Text default=''
    deriving Show Eq Generic

Task
    parent TaskId Maybe
    startedAt UTCTime
    finishedAt UTCTime Maybe
    subsystem TaskSubsystem
    entityId Int Maybe
    entityName Text Maybe
    command Text
    result TaskResult
    message Text Maybe
    clientName Text
    deriving Show Eq Generic

CloudInit
    vmId VmId
    userData Text Maybe
    networkConfig Text Maybe
    injectSshKeys Bool default=true
    UniqueCloudInitVm vmId
    deriving Show Eq Generic

TemplateCloudInit
    templateId TemplateVmId
    userData Text Maybe
    networkConfig Text Maybe
    injectSshKeys Bool default=true
    UniqueTemplateCloudInitVm templateId
    deriving Show Eq Generic

|]
