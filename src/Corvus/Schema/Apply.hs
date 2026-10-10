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
  , ChecksumAlgorithm (..)
  , ChecksumTarget (..)
  , ChecksumSpec (..)
  , ApplyNetwork (..)
  , ApplyVm (..)
  , ApplyDrive (..)
  , ApplyNetIf (..)
  , ApplySharedDir (..)
  , DiskIfExists (..)
  , IfExists (..)
  )
where

import Corvus.Model
import Corvus.Schema.CloudInit (CloudInitConfigYaml)
import Corvus.Schema.Template (TemplateAudioDeviceYaml, TemplateYaml)
import Corvus.Size (optionalSizeField, sizeField)
import Data.Int (Int64)
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
--   * 'IfExistsOverwrite' — replace the target; disks publish a new version.
data IfExists
  = IfExistsError
  | IfExistsSkip
  | IfExistsOverwrite
  deriving stock (Eq, Show)

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
  -- ^ Default policy. CLI --skip-existing changes error to skip.
  }
  deriving stock (Show)

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
  deriving stock (Show)

instance FromJSON ApplySshKey where
  parseJSON = withObject "ApplySshKey" $ \o ->
    ApplySshKey
      <$> o .: "name"
      <*> o .: "publicKey"

data ChecksumAlgorithm
  = ChecksumMd5
  | ChecksumSha1
  | ChecksumSha256
  | ChecksumSha512
  | ChecksumBlake2b
  deriving stock (Eq, Show)

instance FromJSON ChecksumAlgorithm where
  parseJSON = withText "ChecksumAlgorithm" $ \t -> case T.toLower t of
    "md5" -> pure ChecksumMd5
    "sha1" -> pure ChecksumSha1
    "sha256" -> pure ChecksumSha256
    "sha512" -> pure ChecksumSha512
    "blake2b" -> pure ChecksumBlake2b
    other ->
      fail $
        "unknown checksum algorithm '"
          <> T.unpack other
          <> "' (expected: md5, sha1, sha256, sha512, blake2b)"

data ChecksumTarget
  = ChecksumDownload
  | ChecksumFinal
  deriving stock (Eq, Show)

instance FromJSON ChecksumTarget where
  parseJSON = withText "ChecksumTarget" $ \t -> case T.toLower t of
    "download" -> pure ChecksumDownload
    "final" -> pure ChecksumFinal
    other ->
      fail $
        "unknown checksum target '"
          <> T.unpack other
          <> "' (expected: download, final)"

data ChecksumSpec = ChecksumSpec
  { csAlgorithm :: ChecksumAlgorithm
  , csValue :: Text
  , csTarget :: ChecksumTarget
  }
  deriving stock (Eq, Show)

instance FromJSON ChecksumSpec where
  parseJSON = withObject "ChecksumSpec" $ \o ->
    ChecksumSpec
      <$> o .: "algorithm"
      <*> o .: "value"
      <*> o .:? "target" .!= ChecksumDownload

-- | Update is restricted to checksummed HTTP(S) disk imports.
data DiskIfExists = DiskIfExistsPolicy IfExists | DiskIfExistsUpdate
  deriving stock (Eq, Show)

instance FromJSON DiskIfExists where
  parseJSON = withText "disk ifExists" $ \case
    "update" -> pure DiskIfExistsUpdate
    "error" -> pure $ DiskIfExistsPolicy IfExistsError
    "skip" -> pure $ DiskIfExistsPolicy IfExistsSkip
    "overwrite" -> pure $ DiskIfExistsPolicy IfExistsOverwrite
    _ -> fail "disk ifExists must be error, skip, overwrite, or update"

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
  , adSize :: Maybe Int64
  , adImport :: Maybe Text
  , adOverlay :: Maybe Text
  , adClone :: Maybe Text
  , adPath :: Maybe Text
  , adRegister :: Maybe Text
  , adBacking :: Maybe Text
  , adIfExists :: Maybe DiskIfExists
  , adChecksum :: Maybe ChecksumSpec
  , adEphemeral :: Bool
  , adNode :: Text
  }
  deriving stock (Show)

instance FromJSON ApplyDisk where
  parseJSON = withObject "ApplyDisk" $ \o ->
    ApplyDisk
      <$> o .: "name"
      <*> o .:? "format"
      <*> optionalSizeField o "size"
      <*> o .:? "import"
      <*> o .:? "overlay"
      <*> o .:? "clone"
      <*> o .:? "path"
      <*> o .:? "register"
      <*> o .:? "backing"
      <*> o .:? "ifExists"
      <*> o .:? "checksum"
      <*> o .:? "ephemeral" .!= False
      <*> o .:? "node" .!= ""

data ApplyNetwork = ApplyNetwork
  { anName :: Text
  , anNode :: Text
  , anSubnet :: Text
  , anDhcp :: Bool
  , anNat :: Bool
  , anAutostart :: Bool
  , anDnsServers :: [Text]
  , anDomain :: Text
  , anHostDns :: Bool
  }
  deriving stock (Show)

instance FromJSON ApplyNetwork where
  parseJSON = withObject "ApplyNetwork" $ \o ->
    ApplyNetwork
      <$> o .: "name"
      <*> o .:? "node" .!= ""
      <*> o .:? "subnet" .!= ""
      <*> o .:? "dhcp" .!= False
      <*> o .:? "nat" .!= False
      <*> o .:? "autostart" .!= False
      <*> o .:? "dnsServers" .!= []
      <*> o .:? "domain" .!= ""
      <*> o .:? "hostDns" .!= True

data ApplyVm = ApplyVm
  { avName :: Text
  , avNode :: Text
  , avCpuCount :: Int
  , avRam :: Int64
  , avDescription :: Maybe Text
  , avHeadless :: Bool
  , avGuestAgent :: Bool
  , avTpm :: Bool
  , avCloudInit :: Maybe Bool
  , avCloudInitConfig :: Maybe CloudInitConfigYaml
  , avDrives :: [ApplyDrive]
  , avNetworkInterfaces :: [ApplyNetIf]
  , avSharedDirs :: [ApplySharedDir]
  , avAudioDevices :: [TemplateAudioDeviceYaml]
  , avSshKeys :: [Text]
  , avAutostart :: Bool
  , avRebootQuirk :: Bool
  , avCpuModel :: Text
  , avGraphicsAdapter :: GraphicsAdapter
  , avVsock :: Bool
  , avBalloon :: Bool
  , avRng :: Bool
  -- ^ QEMU @-cpu@ model. Default @"host"@ exposes the host CPU
  -- (best perf, not safe for cross-host migration); set to a
  -- stable model (e.g. @"qemu64"@) per VM for migratable
  -- workloads.
  }
  deriving stock (Show)

instance FromJSON ApplyVm where
  parseJSON = withObject "ApplyVm" $ \o ->
    ApplyVm
      <$> o .: "name"
      <*> o .:? "node" .!= ""
      <*> o .: "cpuCount"
      <*> sizeField o "ram"
      <*> o .:? "description"
      <*> o .:? "headless" .!= False
      <*> o .:? "guestAgent" .!= False
      <*> o .:? "tpm" .!= False
      <*> o .:? "cloudInit"
      <*> o .:? "cloudInitConfig"
      <*> o .:? "drives" .!= []
      <*> o .:? "networkInterfaces" .!= []
      <*> o .:? "sharedDirs" .!= []
      <*> o .:? "audioDevices" .!= []
      <*> o .:? "sshKeys" .!= []
      <*> o .:? "autostart" .!= False
      <*> o .:? "rebootQuirk" .!= False
      <*> o .:? "cpuModel" .!= "host"
      <*> o .:? "graphicsAdapter" .!= GraphicsVirtioVga
      <*> o .:? "vsock" .!= True
      <*> o .:? "balloon" .!= True
      <*> o .:? "rng" .!= True

data ApplyDrive = ApplyDrive
  { adrDisk :: Text
  , adrInterface :: DriveInterface
  , adrMedia :: Maybe DriveMedia
  , adrReadOnly :: Bool
  , adrCacheType :: CacheType
  , adrDiscard :: Bool
  }
  deriving stock (Show)

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
  , aniModel :: NetworkDeviceModel
  , aniHostDevice :: Maybe Text
  , aniNetwork :: Maybe Text
  , aniMac :: Maybe Text
  }
  deriving stock (Show)

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
    model <- o .:? "model" .!= NetworkVirtioNetPci
    pure $ ApplyNetIf ifType model hostDevice network mac

data ApplySharedDir = ApplySharedDir
  { asdPath :: Text
  , asdTag :: Text
  , asdCache :: SharedDirCache
  , asdReadOnly :: Bool
  }
  deriving stock (Show)

instance FromJSON ApplySharedDir where
  parseJSON = withObject "ApplySharedDir" $ \o ->
    ApplySharedDir
      <$> o .: "path"
      <*> o .: "tag"
      <*> o .:? "cache" .!= CacheAuto
      <*> o .:? "readOnly" .!= False
