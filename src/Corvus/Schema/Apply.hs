{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | YAML input schema for @crv apply@ declarative environment configs.
--
-- The top-level 'ApplyConfig' aggregates SSH keys, disks, networks, VMs,
-- and templates; per-VM drives, network interfaces, and shared directories
-- are modelled by the nested 'ApplyDrive', 'ApplyNetIf', 'ApplySharedDir'
-- types. Templates inside an apply config reuse "Corvus.Schema.Template".
module Corvus.Schema.Apply
  ( ApplyConfig (..)
  , ApplySshKey (..)
  , ApplyDisk (..)
  , ApplyNetwork (..)
  , ApplyVm (..)
  , ApplyDrive (..)
  , ApplyNetIf (..)
  , ApplySharedDir (..)
  , IfExists (..)
  )
where

import Corvus.Model
import Corvus.Schema.CloudInit (CloudInitConfigYaml)
import Corvus.Schema.Template (TemplateYaml)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (FromJSON (..), withObject, withText, (.!=), (.:), (.:?))

-- | Policy applied when a target (an apply resource, or a build's
-- artifact disk) already exists at the time the operation would
-- create it.
--
--   * 'IfExistsError' (default) — fail loudly. Forces the operator
--     to choose explicitly between skip and overwrite.
--   * 'IfExistsSkip' — treat existing targets as success and move
--     on. Lets a partially-failed pipeline be re-run without
--     redoing already-completed work.
--   * 'IfExistsOverwrite' — delete the existing target first
--     (build-only; apply rejects this at validation time, since
--     deleting a registered template/disk/network/VM is invasive
--     and would clobber unrelated state).
data IfExists
  = IfExistsError
  | IfExistsSkip
  | IfExistsOverwrite
  deriving (Eq, Show)

instance FromJSON IfExists where
  parseJSON = withText "ifExists" $ \case
    "error" -> pure IfExistsError
    "skip" -> pure IfExistsSkip
    "overwrite" -> pure IfExistsOverwrite
    other ->
      fail $
        "unknown ifExists value '"
          <> T.unpack other
          <> "' (expected: error, skip, overwrite)"

data ApplyConfig = ApplyConfig
  { acSshKeys :: [ApplySshKey]
  , acDisks :: [ApplyDisk]
  , acNetworks :: [ApplyNetwork]
  , acVms :: [ApplyVm]
  , acTemplates :: [TemplateYaml]
  , acIfExists :: IfExists
  -- ^ YAML equivalent of the @--skip-existing@ CLI flag, plus the
  -- 'IfExistsError' default. The CLI flag, when present, forces
  -- 'IfExistsSkip' regardless of the YAML; absent, the YAML wins.
  -- Apply does not accept 'IfExistsOverwrite' — rejected at
  -- 'validateConfig' time.
  }
  deriving (Show)

instance FromJSON ApplyConfig where
  parseJSON = withObject "ApplyConfig" $ \o ->
    ApplyConfig
      <$> o .:? "sshKeys" .!= []
      <*> o .:? "disks" .!= []
      <*> o .:? "networks" .!= []
      <*> o .:? "vms" .!= []
      <*> o .:? "templates" .!= []
      <*> o .:? "ifExists" .!= IfExistsError

data ApplySshKey = ApplySshKey
  { askName :: Text
  , askPublicKey :: Text
  }
  deriving (Show)

instance FromJSON ApplySshKey where
  parseJSON = withObject "ApplySshKey" $ \o ->
    ApplySshKey
      <$> o .: "name"
      <*> o .: "publicKey"

-- | Disk definition in the apply YAML config.
--
-- The @path@ field controls where the disk image file is placed:
--
--   * Not specified: the image is placed in the base images directory
--   * Starts with @\/@: interpreted as an absolute path
--   * Otherwise: relative to the base images directory
--   * Ends with @\/@: treated as a directory (auto-generates filename from disk name + format extension)
--   * Does not end with @\/@: treated as the full file path
--
-- Examples:
--
-- @
-- path: "ws25/"           # -> $BASE/ws25/my-disk.qcow2
-- path: "my-disk.raw"     # -> $BASE/my-disk.raw
-- path: "/data/vms/"      # -> /data/vms/my-disk.qcow2
-- path: "/data/disk.raw"  # -> /data/disk.raw
-- @
data ApplyDisk = ApplyDisk
  { adName :: Text
  , adFormat :: Maybe DriveFormat
  , adSizeMb :: Maybe Int
  , adImport :: Maybe Text
  , adOverlay :: Maybe Text
  , adClone :: Maybe Text
  , adPath :: Maybe Text
  , adRegister :: Maybe Text
  , adBacking :: Maybe Text
  , adMd5 :: Maybe Text
  }
  deriving (Show)

instance FromJSON ApplyDisk where
  parseJSON = withObject "ApplyDisk" $ \o ->
    ApplyDisk
      <$> o .: "name"
      <*> o .:? "format"
      <*> o .:? "sizeMb"
      <*> o .:? "import"
      <*> o .:? "overlay"
      <*> o .:? "clone"
      <*> o .:? "path"
      <*> o .:? "register"
      <*> o .:? "backing"
      <*> o .:? "md5"

data ApplyNetwork = ApplyNetwork
  { anName :: Text
  , anNode :: Text
  , anSubnet :: Text
  , anDhcp :: Bool
  , anNat :: Bool
  , anAutostart :: Bool
  }
  deriving (Show)

instance FromJSON ApplyNetwork where
  parseJSON = withObject "ApplyNetwork" $ \o ->
    ApplyNetwork
      <$> o .: "name"
      <*> o .:? "node" .!= ""
      <*> o .:? "subnet" .!= ""
      <*> o .:? "dhcp" .!= False
      <*> o .:? "nat" .!= False
      <*> o .:? "autostart" .!= False

data ApplyVm = ApplyVm
  { avName :: Text
  , avNode :: Text
  , avCpuCount :: Int
  , avRamMb :: Int
  , avDescription :: Maybe Text
  , avHeadless :: Bool
  , avGuestAgent :: Bool
  , avCloudInit :: Maybe Bool
  , avCloudInitConfig :: Maybe CloudInitConfigYaml
  , avDrives :: [ApplyDrive]
  , avNetworkInterfaces :: [ApplyNetIf]
  , avSharedDirs :: [ApplySharedDir]
  , avSshKeys :: [Text]
  , avAutostart :: Bool
  }
  deriving (Show)

instance FromJSON ApplyVm where
  parseJSON = withObject "ApplyVm" $ \o ->
    ApplyVm
      <$> o .: "name"
      <*> o .:? "node" .!= ""
      <*> o .: "cpuCount"
      <*> o .: "ramMb"
      <*> o .:? "description"
      <*> o .:? "headless" .!= False
      <*> o .:? "guestAgent" .!= False
      <*> o .:? "cloudInit"
      <*> o .:? "cloudInitConfig"
      <*> o .:? "drives" .!= []
      <*> o .:? "networkInterfaces" .!= []
      <*> o .:? "sharedDirs" .!= []
      <*> o .:? "sshKeys" .!= []
      <*> o .:? "autostart" .!= False

data ApplyDrive = ApplyDrive
  { adrDisk :: Text
  , adrInterface :: DriveInterface
  , adrMedia :: Maybe DriveMedia
  , adrReadOnly :: Bool
  , adrCacheType :: CacheType
  , adrDiscard :: Bool
  }
  deriving (Show)

instance FromJSON ApplyDrive where
  parseJSON = withObject "ApplyDrive" $ \o ->
    ApplyDrive
      <$> o .: "disk"
      <*> o .: "interface"
      <*> o .:? "media"
      <*> o .:? "readOnly" .!= False
      <*> o .:? "cacheType" .!= CacheWriteback
      <*> o .:? "discard" .!= False

data ApplyNetIf = ApplyNetIf
  { aniType :: NetInterfaceType
  , aniHostDevice :: Maybe Text
  , aniNetwork :: Maybe Text
  , aniMac :: Maybe Text
  }
  deriving (Show)

instance FromJSON ApplyNetIf where
  parseJSON = withObject "ApplyNetIf" $ \o -> do
    mType <- o .:? "type"
    network <- o .:? "network"
    hostDevice <- o .:? "hostDevice"
    mac <- o .:? "mac"
    ifType <- case (mType, network) of
      (Nothing, Just _) -> pure NetManaged
      (Just t, Just _)
        | t /= NetManaged -> fail "network interface with 'network' must have type 'managed' or omit 'type'"
        | otherwise -> pure NetManaged
      (Just t, Nothing) -> pure t
      (Nothing, Nothing) -> pure NetUser
    pure $ ApplyNetIf ifType hostDevice network mac

data ApplySharedDir = ApplySharedDir
  { asdPath :: Text
  , asdTag :: Text
  , asdCache :: SharedDirCache
  , asdReadOnly :: Bool
  }
  deriving (Show)

instance FromJSON ApplySharedDir where
  parseJSON = withObject "ApplySharedDir" $ \o ->
    ApplySharedDir
      <$> o .: "path"
      <*> o .: "tag"
      <*> o .:? "cache" .!= CacheAuto
      <*> o .:? "readOnly" .!= False
