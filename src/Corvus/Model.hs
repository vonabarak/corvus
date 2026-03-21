{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Corvus.Model
  ( -- * Database schema
    migrateAll,

    -- * Entities
    Vm (..),
    Drive (..),
    NetworkInterface (..),
    DiskImage (..),
    Snapshot (..),

    -- * Entity IDs
    VmId,
    DriveId,
    NetworkInterfaceId,
    DiskImageId,
    SnapshotId,

    -- * Entity Fields (for queries)
    EntityField (..),

    -- * Enums
    VmStatus (..),
    DriveInterface (..),
    DriveFormat (..),
    DriveMedia (..),
    CacheType (..),
    NetInterfaceType (..),
    SharedDirCache (..),
    TemplateCloneStrategy (..),

    -- * Shared directory entity
    SharedDir (..),
    SharedDirId,

    -- * SSH key entities
    SshKey (..),
    SshKeyId,
    VmSshKey (..),
    VmSshKeyId,

    -- * Template entities
    TemplateVm (..),
    TemplateVmId,
    TemplateDrive (..),
    TemplateDriveId,
    TemplateNetworkInterface (..),
    TemplateNetworkInterfaceId,
    TemplateSshKey (..),
    TemplateSshKeyId,

    -- * Unique constraints
    Unique (..),

    -- * Enum conversion type class
    EnumText (..),

    -- * Re-exports for convenience
    Entity (..),
    Key,
    toSqlKey,
    fromSqlKey,
  )
where

import Data.Binary (Binary)
import qualified Data.Binary as Bin
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import qualified Data.Aeson.Types as AT
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.Persist
import Database.Persist.Sql (PersistFieldSql (..), SqlType (..), fromSqlKey, toSqlKey)
import Database.Persist.TH
import GHC.Generics (Generic)

-- Orphan instance for UTCTime serialization
instance Binary UTCTime where
  put t = Bin.put (realToFrac (utcTimeToPOSIXSeconds t) :: Double)
  get = posixSecondsToUTCTime . realToFrac <$> (Bin.get :: Bin.Get Double)

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
  | VmRunning
  | VmPaused
  | VmError
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Binary VmStatus

instance EnumText VmStatus where
  enumTypeName = "VmStatus"
  enumMapping =
    [ (VmStopped, "stopped"),
      (VmRunning, "running"),
      (VmPaused, "paused"),
      (VmError, "error")
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
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Binary DriveInterface

instance EnumText DriveInterface where
  enumTypeName = "DriveInterface"
  enumMapping =
    [ (InterfaceVirtio, "virtio"),
      (InterfaceIde, "ide"),
      (InterfaceScsi, "scsi"),
      (InterfaceSata, "sata"),
      (InterfaceNvme, "nvme"),
      (InterfacePflash, "pflash")
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
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Binary DriveFormat

instance EnumText DriveFormat where
  enumTypeName = "DriveFormat"
  enumMapping =
    [ (FormatQcow2, "qcow2"),
      (FormatRaw, "raw"),
      (FormatVmdk, "vmdk"),
      (FormatVdi, "vdi")
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

instance Binary CacheType

instance EnumText CacheType where
  enumTypeName = "CacheType"
  enumMapping =
    [ (CacheNone, "none"),
      (CacheWriteback, "writeback"),
      (CacheWritethrough, "writethrough"),
      (CacheDirectsync, "directsync"),
      (CacheUnsafe, "unsafe")
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

instance Binary DriveMedia

instance EnumText DriveMedia where
  enumTypeName = "DriveMedia"
  enumMapping =
    [ (MediaDisk, "disk"),
      (MediaCdrom, "cdrom")
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
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Binary NetInterfaceType

instance EnumText NetInterfaceType where
  enumTypeName = "NetInterfaceType"
  enumMapping =
    [ (NetUser, "user"),
      (NetTap, "tap"),
      (NetBridge, "bridge"),
      (NetMacvtap, "macvtap"),
      (NetVde, "vde")
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

instance Binary SharedDirCache

instance EnumText SharedDirCache where
  enumTypeName = "SharedDirCache"
  enumMapping =
    [ (CacheAlways, "always"),
      (CacheAuto, "auto"),
      (CacheNever, "never")
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
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Binary TemplateCloneStrategy

instance EnumText TemplateCloneStrategy where
  enumTypeName = "TemplateCloneStrategy"
  enumMapping =
    [ (StrategyClone, "clone"),
      (StrategyOverlay, "overlay"),
      (StrategyDirect, "direct")
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
-- Entity definitions
--------------------------------------------------------------------------------

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Vm
    name Text
    createdAt UTCTime
    status VmStatus
    cpuCount Int
    ramMb Int
    description Text Maybe
    pid Int Maybe
    UniqueName name
    deriving Show Eq Generic

DiskImage
    name Text
    filePath Text
    format DriveFormat
    sizeMb Int Maybe
    createdAt UTCTime
    backingImageId DiskImageId Maybe
    UniqueDiskImageName name
    UniqueImagePath filePath
    deriving Show Eq Generic

Snapshot
    diskImageId DiskImageId
    name Text
    createdAt UTCTime
    sizeMb Int Maybe
    deriving Show Eq Generic

Drive
    vmId VmId
    diskImageId DiskImageId
    interface DriveInterface
    media DriveMedia Maybe
    readOnly Bool default=false
    cacheType CacheType
    discard Bool default=false
    deriving Show Eq Generic

NetworkInterface
    vmId VmId
    interfaceType NetInterfaceType
    hostDevice Text
    macAddress Text
    deriving Show Eq Generic

SharedDir
    vmId VmId
    path Text
    tag Text
    cache SharedDirCache
    readOnly Bool default=false
    pid Int Maybe
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
    createdAt UTCTime
    UniqueTemplateVmName name
    deriving Show Eq Generic

TemplateDrive
    templateId TemplateVmId
    diskImageId DiskImageId
    interface DriveInterface
    media DriveMedia Maybe
    readOnly Bool default=false
    cacheType CacheType
    discard Bool default=false
    cloneStrategy TemplateCloneStrategy
    newSizeMb Int Maybe
    deriving Show Eq Generic

TemplateNetworkInterface
    templateId TemplateVmId
    interfaceType NetInterfaceType
    hostDevice Text Maybe
    deriving Show Eq Generic

TemplateSshKey
    templateId TemplateVmId
    sshKeyId SshKeyId
    UniqueTemplateSshKey templateId sshKeyId
    deriving Show Eq Generic
|]

-- Binary instances for entities (for network serialization)
instance Binary Vm

instance Binary DiskImage

instance Binary Snapshot

instance Binary Drive

instance Binary NetworkInterface

instance Binary SharedDir

instance Binary SshKey

instance Binary VmSshKey

instance Binary TemplateVm

instance Binary TemplateDrive

instance Binary TemplateNetworkInterface

instance Binary TemplateSshKey

-- Binary instances for keys
instance Binary (Key Vm) where
  put = Bin.put . fromSqlKey
  get = toSqlKey <$> Bin.get

instance Binary (Key DiskImage) where
  put = Bin.put . fromSqlKey
  get = toSqlKey <$> Bin.get

instance Binary (Key Snapshot) where
  put = Bin.put . fromSqlKey
  get = toSqlKey <$> Bin.get

instance Binary (Key Drive) where
  put = Bin.put . fromSqlKey
  get = toSqlKey <$> Bin.get

instance Binary (Key NetworkInterface) where
  put = Bin.put . fromSqlKey
  get = toSqlKey <$> Bin.get

instance Binary (Key SharedDir) where
  put = Bin.put . fromSqlKey
  get = toSqlKey <$> Bin.get

instance Binary (Key SshKey) where
  put = Bin.put . fromSqlKey
  get = toSqlKey <$> Bin.get

instance Binary (Key VmSshKey) where
  put = Bin.put . fromSqlKey
  get = toSqlKey <$> Bin.get

instance Binary (Key TemplateVm) where
  put = Bin.put . fromSqlKey
  get = toSqlKey <$> Bin.get

instance Binary (Key TemplateDrive) where
  put = Bin.put . fromSqlKey
  get = toSqlKey <$> Bin.get

instance Binary (Key TemplateNetworkInterface) where
  put = Bin.put . fromSqlKey
  get = toSqlKey <$> Bin.get

instance Binary (Key TemplateSshKey) where
  put = Bin.put . fromSqlKey
  get = toSqlKey <$> Bin.get
