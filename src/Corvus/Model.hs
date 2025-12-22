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

    -- * Entity IDs
    VmId,
    DriveId,
    NetworkInterfaceId,

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

    -- * Shared directory entity
    SharedDir (..),
    SharedDirId,

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
import Data.List (find)
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
    case lookup val enumMapping of
      Just t -> t
      Nothing -> error $ "enumToText: incomplete mapping for " <> T.unpack (enumTypeName @a)

  -- | Convert text to enum (derived from mapping)
  enumFromText :: Text -> Either Text a
  enumFromText t =
    case find (\(_, txt) -> T.toLower txt == T.toLower t) enumMapping of
      Just (val, _) -> Right val
      Nothing -> Left $ "Invalid " <> enumTypeName @a <> ": " <> t

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

instance PersistField SharedDirCache where
  toPersistValue = enumToPersistValue
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql SharedDirCache where
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

Drive
    vmId VmId
    interface DriveInterface
    filePath Text
    format DriveFormat
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
|]

-- Binary instances for entities (for network serialization)
instance Binary Vm

instance Binary Drive

instance Binary NetworkInterface

instance Binary SharedDir

-- Binary instances for keys
instance Binary (Key Vm) where
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
