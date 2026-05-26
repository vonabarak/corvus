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
  , TemplateCloneStrategy (..)

    -- * Network entity
  , Network (..)
  , NetworkId
  , NetworkPeer (..)
  , NetworkPeerId

    -- * Shared directory entity
  , SharedDir (..)
  , SharedDirId

    -- * SSH key entities
  , SshKey (..)
  , SshKeyId
  , VmSshKey (..)
  , VmSshKeyId

    -- * Template entities
  , TemplateVm (..)
  , TemplateVmId
  , TemplateDrive (..)
  , TemplateDriveId
  , TemplateNetworkInterface (..)
  , TemplateNetworkInterfaceId
  , TemplateSshKey (..)
  , TemplateSshKeyId

    -- * Task entity
  , Task (..)
  , TaskId

    -- * Cloud-init config entities
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

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import qualified Data.Aeson.Types as AT
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql (PersistFieldSql (..), SqlType (..), fromSqlKey, toSqlKey)
import Database.Persist.TH
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- EnumText type class - shared interface for all text-serializable enums
--------------------------------------------------------------------------------

-- | Type class for enums that serialize to/from Text
class (Eq a) => EnumText a where
  -- | The mapping between enum values and their text representations
  enumMapping :: [(a, Text)]

  -- | Type name for error messages
  enumTypeName :: Text

  -- | Convert enum to text (derived from mapping)
  enumToText :: a -> Text
  enumToText val =
    fromMaybe ("<unknown " <> enumTypeName @a <> ">") (lookup val enumMapping)

  -- | Convert text to enum (derived from mapping)
  enumFromText :: Text -> Either Text a
  enumFromText t =
    case find (\(_, txt) -> T.toLower txt == T.toLower t) enumMapping of
      Just (val, _) -> Right val
      Nothing -> Left $ "Invalid " <> enumTypeName @a <> ": " <> t

-- | Standard parser for enums using EnumText
parseEnumJSON :: (EnumText a) => Value -> AT.Parser a
parseEnumJSON (String t) =
  case enumFromText t of
    Right val -> pure val
    Left err -> fail (T.unpack err)
parseEnumJSON _ = fail "Expected String"

-- | Standard serializer for enums using EnumText
toEnumJSON :: (EnumText a) => a -> Value
toEnumJSON = String . enumToText

-- | Helper to create PersistField instance
enumToPersistValue :: (EnumText a) => a -> PersistValue
enumToPersistValue = PersistText . enumToText

-- | Helper to create PersistField instance
enumFromPersistValue :: forall a. (EnumText a) => PersistValue -> Either Text a
enumFromPersistValue (PersistText t) = enumFromText t
enumFromPersistValue x = Left $ "Expected Text for " <> enumTypeName @a <> ", got: " <> T.pack (show x)

--------------------------------------------------------------------------------
-- VmStatus
--------------------------------------------------------------------------------

data VmStatus
  = VmStopped
  | VmStarting
  | VmRunning
  | VmStopping
  | VmPaused
  | VmError
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance EnumText VmStatus where
  enumTypeName = "VmStatus"
  enumMapping =
    [ (VmStopped, "stopped")
    , (VmStarting, "starting")
    , (VmRunning, "running")
    , (VmStopping, "stopping")
    , (VmPaused, "paused")
    , (VmError, "error")
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

--------------------------------------------------------------------------------
-- TemplateCloneStrategy
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- TaskSubsystem
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- TaskResult
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Entity definitions
--------------------------------------------------------------------------------

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
    ramMbTotal Int Maybe default=NULL
    ramMbFree Int Maybe default=NULL
    storageBytesTotal Int Maybe default=NULL
    storageBytesFree Int Maybe default=NULL
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
    cpuCount Int
    ramMb Int
    description Text Maybe
    headless Bool default=false
    guestAgent Bool default=false
    cloudInit Bool default=false
    healthcheck UTCTime Maybe default=NULL
    autostart Bool default=false
    spicePort Int Maybe default=NULL
    vsockCid Int Maybe default=NULL
    errorMessage Text Maybe default=NULL
    lastErrorAt UTCTime Maybe default=NULL
    migrating Bool default=false
    rebootQuirk Bool default=false
    UniqueVmNamePerNode nodeId name
    deriving Show Eq Generic

DiskImage
    name Text
    format DriveFormat
    sizeMb Int Maybe
    createdAt UTCTime
    backingImageId DiskImageId Maybe
    ephemeral Bool default=false
    UniqueDiskImageName name
    deriving Show Eq Generic

-- Per-node placement of a logical 'DiskImage'. A logical image
-- may live on one or many nodes; each row carries the on-disk
-- path on that specific node (paths can differ — e.g.
-- host-installed daemon vs containerised daemon vs test-node
-- overlay). Same-node check on attach: @disk_image_node@ row
-- must exist for @(image, vm.node)@.
DiskImageNode
    diskImageId DiskImageId
    nodeId NodeId
    filePath Text
    UniqueDiskImageOnNode diskImageId nodeId
    UniqueDiskImagePathPerNode nodeId filePath
    deriving Show Eq Generic

Snapshot
    diskImageId DiskImageId
    name Text
    createdAt UTCTime
    sizeMb Int Maybe
    UniqueSnapshot diskImageId name
    deriving Show Eq Generic

Drive
    vmId VmId
    diskImageId DiskImageId
    interface DriveInterface
    media DriveMedia Maybe
    readOnly Bool default=false
    cacheType CacheType
    discard Bool default=false
    UniqueDrive vmId diskImageId
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
    ramMb Int
    description Text Maybe
    headless Bool default=false
    cloudInit Bool default=false
    guestAgent Bool default=false
    autostart Bool default=false
    rebootQuirk Bool default=false
    createdAt UTCTime
    UniqueTemplateVmName name
    deriving Show Eq Generic

TemplateDrive
    templateId TemplateVmId
    diskImageId DiskImageId Maybe
    diskName Text Maybe
    interface DriveInterface
    media DriveMedia Maybe
    readOnly Bool default=false
    cacheType CacheType
    discard Bool default=false
    cloneStrategy TemplateCloneStrategy
    sizeMb Int Maybe
    format DriveFormat Maybe
    ephemeral Bool Maybe
    deriving Show Eq Generic

TemplateNetworkInterface
    templateId TemplateVmId
    interfaceType NetInterfaceType
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
