{-# LANGUAGE RecordWildCards #-}

-- | Cap'n Proto conversion for VM template info / details.
module Corvus.Wire.Template
  ( toCapnpTemplateVmInfo
  , fromCapnpTemplateVmInfo
  , toCapnpTemplateDriveInfo
  , fromCapnpTemplateDriveInfo
  , toCapnpTemplateNetIfInfo
  , fromCapnpTemplateNetIfInfo
  , toCapnpTemplateSshKeyInfo
  , fromCapnpTemplateSshKeyInfo
  , toCapnpTemplateSharedDirInfo
  , fromCapnpTemplateSharedDirInfo
  , toCapnpTemplateAudioDeviceInfo
  , fromCapnpTemplateAudioDeviceInfo
  , toCapnpTemplateDetails
  , fromCapnpTemplateDetails
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Template as CGT
import qualified Corvus.Protocol.CloudInit as PCI
import qualified Corvus.Protocol.Template as P
import Corvus.Wire.CloudInit (fromCapnpCloudInitInfo, toCapnpCloudInitInfo)
import Corvus.Wire.Common (fromCapnpNamedRefOpt, toCapnpNamedRefOpt)
import Corvus.Wire.Enums
  ( fromCapnpAudioBackend
  , fromCapnpAudioDeviceModel
  , fromCapnpCacheType
  , fromCapnpDriveFormat
  , fromCapnpDriveInterface
  , fromCapnpDriveMedia
  , fromCapnpGraphicsAdapter
  , fromCapnpNetInterfaceType
  , fromCapnpNetworkDeviceModel
  , fromCapnpSharedDirCache
  , fromCapnpTemplateCloneStrategy
  , toCapnpAudioBackend
  , toCapnpAudioDeviceModel
  , toCapnpCacheType
  , toCapnpDriveFormat
  , toCapnpDriveInterface
  , toCapnpDriveMedia
  , toCapnpGraphicsAdapter
  , toCapnpNetInterfaceType
  , toCapnpNetworkDeviceModel
  , toCapnpSharedDirCache
  , toCapnpTemplateCloneStrategy
  )
import Corvus.Wire.Errors (WireError)
import Corvus.Wire.Time (nanosToUtcTime, utcTimeToNanos)
import Data.Maybe (fromMaybe, isJust)

toCapnpTemplateAudioDeviceInfo :: P.TemplateAudioDeviceInfo -> C.Parsed CGT.TemplateAudioDeviceInfo
toCapnpTemplateAudioDeviceInfo P.TemplateAudioDeviceInfo {..} =
  CGT.TemplateAudioDeviceInfo
    { CGT.id = tvadiId
    , CGT.backend = toCapnpAudioBackend tvadiBackend
    , CGT.model = toCapnpAudioDeviceModel tvadiModel
    , CGT.options = tvadiOptions
    }

fromCapnpTemplateAudioDeviceInfo :: C.Parsed CGT.TemplateAudioDeviceInfo -> Either WireError P.TemplateAudioDeviceInfo
fromCapnpTemplateAudioDeviceInfo CGT.TemplateAudioDeviceInfo {..} = do
  backend' <- fromCapnpAudioBackend backend
  model' <- fromCapnpAudioDeviceModel model
  pure P.TemplateAudioDeviceInfo {P.tvadiId = id, P.tvadiBackend = backend', P.tvadiModel = model', P.tvadiOptions = options}

emptyCloudInitInfo :: PCI.CloudInitInfo
emptyCloudInitInfo =
  PCI.CloudInitInfo
    { PCI.ciiUserData = Nothing
    , PCI.ciiNetworkConfig = Nothing
    , PCI.ciiInjectSshKeys = False
    }

-- ---------------------------------------------------------------------
-- TemplateVmInfo
-- ---------------------------------------------------------------------

toCapnpTemplateVmInfo :: P.TemplateVmInfo -> C.Parsed CGT.TemplateVmInfo
toCapnpTemplateVmInfo P.TemplateVmInfo {..} =
  CGT.TemplateVmInfo
    { CGT.id = tviId
    , CGT.name = tviName
    , CGT.cpuCount = fromIntegral tviCpuCount
    , CGT.ramMb = fromIntegral tviRamMb
    , CGT.description = fromMaybe mempty tviDescription
    , CGT.headless = tviHeadless
    , CGT.guestAgent = tviGuestAgent
    , CGT.tpm = tviTpm
    , CGT.autostart = tviAutostart
    , CGT.rebootQuirk = tviRebootQuirk
    , CGT.graphicsAdapter = toCapnpGraphicsAdapter tviGraphicsAdapter
    , CGT.vsock = tviVsock
    , CGT.balloon = tviBalloon
    , CGT.rng = tviRng
    }

fromCapnpTemplateVmInfo :: C.Parsed CGT.TemplateVmInfo -> Either WireError P.TemplateVmInfo
fromCapnpTemplateVmInfo CGT.TemplateVmInfo {..} = do
  graphicsAdapter' <- fromCapnpGraphicsAdapter graphicsAdapter
  pure
    P.TemplateVmInfo
      { P.tviId = id
      , P.tviName = name
      , P.tviCpuCount = fromIntegral cpuCount
      , P.tviRamMb = fromIntegral ramMb
      , P.tviDescription = if description == mempty then Nothing else Just description
      , P.tviHeadless = headless
      , P.tviGuestAgent = guestAgent
      , P.tviTpm = tpm
      , P.tviAutostart = autostart
      , P.tviRebootQuirk = rebootQuirk
      , P.tviGraphicsAdapter = graphicsAdapter'
      , P.tviVsock = vsock
      , P.tviBalloon = balloon
      , P.tviRng = rng
      }

-- ---------------------------------------------------------------------
-- TemplateDriveInfo
-- ---------------------------------------------------------------------

toCapnpTemplateDriveInfo :: P.TemplateDriveInfo -> C.Parsed CGT.TemplateDriveInfo
toCapnpTemplateDriveInfo P.TemplateDriveInfo {..} =
  CGT.TemplateDriveInfo
    { CGT.diskImage = toCapnpNamedRefOpt tvdiDiskImage
    , CGT.interface = toCapnpDriveInterface tvdiInterface
    , CGT.hasMedia = isJust tvdiMedia
    , CGT.media = maybe (toCapnpDriveMedia minBound) toCapnpDriveMedia tvdiMedia
    , CGT.readOnly = tvdiReadOnly
    , CGT.cacheType = toCapnpCacheType tvdiCacheType
    , CGT.discard = tvdiDiscard
    , CGT.cloneStrategy = toCapnpTemplateCloneStrategy tvdiCloneStrategy
    , CGT.sizeMb = maybe 0 fromIntegral tvdiSizeMb
    , CGT.hasFormat = isJust tvdiFormat
    , CGT.format = maybe (toCapnpDriveFormat minBound) toCapnpDriveFormat tvdiFormat
    , CGT.hasEphemeral = isJust tvdiEphemeral
    , CGT.ephemeral = fromMaybe False tvdiEphemeral
    }

fromCapnpTemplateDriveInfo
  :: C.Parsed CGT.TemplateDriveInfo
  -> Either WireError P.TemplateDriveInfo
fromCapnpTemplateDriveInfo CGT.TemplateDriveInfo {..} = do
  iface <- fromCapnpDriveInterface interface
  cache <- fromCapnpCacheType cacheType
  strat <- fromCapnpTemplateCloneStrategy cloneStrategy
  med <-
    if hasMedia
      then Just <$> fromCapnpDriveMedia media
      else pure Nothing
  fmt <-
    if hasFormat
      then Just <$> fromCapnpDriveFormat format
      else pure Nothing
  pure
    P.TemplateDriveInfo
      { P.tvdiDiskImage = fromCapnpNamedRefOpt diskImage
      , P.tvdiInterface = iface
      , P.tvdiMedia = med
      , P.tvdiReadOnly = readOnly
      , P.tvdiCacheType = cache
      , P.tvdiDiscard = discard
      , P.tvdiCloneStrategy = strat
      , P.tvdiSizeMb = if sizeMb == 0 then Nothing else Just (fromIntegral sizeMb)
      , P.tvdiFormat = fmt
      , P.tvdiEphemeral = if hasEphemeral then Just ephemeral else Nothing
      }

-- ---------------------------------------------------------------------
-- TemplateNetIfInfo
-- ---------------------------------------------------------------------

toCapnpTemplateNetIfInfo :: P.TemplateNetIfInfo -> C.Parsed CGT.TemplateNetIfInfo
toCapnpTemplateNetIfInfo P.TemplateNetIfInfo {..} =
  CGT.TemplateNetIfInfo
    { CGT.type_ = toCapnpNetInterfaceType tvniType
    , CGT.model = toCapnpNetworkDeviceModel tvniModel
    , CGT.hostDevice = fromMaybe mempty tvniHostDevice
    , CGT.network = fromMaybe mempty tvniNetwork
    }

fromCapnpTemplateNetIfInfo
  :: C.Parsed CGT.TemplateNetIfInfo
  -> Either WireError P.TemplateNetIfInfo
fromCapnpTemplateNetIfInfo CGT.TemplateNetIfInfo {..} = do
  t <- fromCapnpNetInterfaceType type_
  model' <- fromCapnpNetworkDeviceModel model
  pure
    P.TemplateNetIfInfo
      { P.tvniType = t
      , P.tvniModel = model'
      , P.tvniHostDevice = if hostDevice == mempty then Nothing else Just hostDevice
      , P.tvniNetwork = if network == mempty then Nothing else Just network
      }

-- ---------------------------------------------------------------------
-- TemplateSshKeyInfo
-- ---------------------------------------------------------------------

toCapnpTemplateSshKeyInfo :: P.TemplateSshKeyInfo -> C.Parsed CGT.TemplateSshKeyInfo
toCapnpTemplateSshKeyInfo P.TemplateSshKeyInfo {..} =
  CGT.TemplateSshKeyInfo {CGT.id = tvskiId, CGT.name = tvskiName}

fromCapnpTemplateSshKeyInfo :: C.Parsed CGT.TemplateSshKeyInfo -> P.TemplateSshKeyInfo
fromCapnpTemplateSshKeyInfo CGT.TemplateSshKeyInfo {..} =
  P.TemplateSshKeyInfo {P.tvskiId = id, P.tvskiName = name}

-- ---------------------------------------------------------------------
-- TemplateSharedDirInfo
-- ---------------------------------------------------------------------

toCapnpTemplateSharedDirInfo :: P.TemplateSharedDirInfo -> C.Parsed CGT.TemplateSharedDirInfo
toCapnpTemplateSharedDirInfo P.TemplateSharedDirInfo {..} =
  CGT.TemplateSharedDirInfo
    { CGT.id = tvsdiId
    , CGT.path = tvsdiPath
    , CGT.tag = tvsdiTag
    , CGT.cache = toCapnpSharedDirCache tvsdiCache
    , CGT.readOnly = tvsdiReadOnly
    }

fromCapnpTemplateSharedDirInfo
  :: C.Parsed CGT.TemplateSharedDirInfo
  -> Either WireError P.TemplateSharedDirInfo
fromCapnpTemplateSharedDirInfo CGT.TemplateSharedDirInfo {..} = do
  c <- fromCapnpSharedDirCache cache
  pure
    P.TemplateSharedDirInfo
      { P.tvsdiId = id
      , P.tvsdiPath = path
      , P.tvsdiTag = tag
      , P.tvsdiCache = c
      , P.tvsdiReadOnly = readOnly
      }

-- ---------------------------------------------------------------------
-- TemplateDetails
-- ---------------------------------------------------------------------

toCapnpTemplateDetails :: P.TemplateDetails -> C.Parsed CGT.TemplateDetails
toCapnpTemplateDetails P.TemplateDetails {..} =
  CGT.TemplateDetails
    { CGT.id = tvdId
    , CGT.name = tvdName
    , CGT.cpuCount = fromIntegral tvdCpuCount
    , CGT.ramMb = fromIntegral tvdRamMb
    , CGT.description = fromMaybe mempty tvdDescription
    , CGT.headless = tvdHeadless
    , CGT.cloudInit = tvdCloudInit
    , CGT.guestAgent = tvdGuestAgent
    , CGT.tpm = tvdTpm
    , CGT.autostart = tvdAutostart
    , CGT.cloudInitConfig =
        maybe (toCapnpCloudInitInfo emptyCloudInitInfo) toCapnpCloudInitInfo tvdCloudInitConfig
    , CGT.createdAt = utcTimeToNanos tvdCreatedAt
    , CGT.drives = map toCapnpTemplateDriveInfo tvdDrives
    , CGT.netIfs = map toCapnpTemplateNetIfInfo tvdNetIfs
    , CGT.sshKeys = map toCapnpTemplateSshKeyInfo tvdSshKeys
    , CGT.rebootQuirk = tvdRebootQuirk
    , CGT.sharedDirs = map toCapnpTemplateSharedDirInfo tvdSharedDirs
    , CGT.audioDevices = map toCapnpTemplateAudioDeviceInfo tvdAudioDevices
    , CGT.graphicsAdapter = toCapnpGraphicsAdapter tvdGraphicsAdapter
    , CGT.vsock = tvdVsock
    , CGT.balloon = tvdBalloon
    , CGT.rng = tvdRng
    }

fromCapnpTemplateDetails
  :: C.Parsed CGT.TemplateDetails
  -> Either WireError P.TemplateDetails
fromCapnpTemplateDetails CGT.TemplateDetails {..} = do
  drives' <- traverse fromCapnpTemplateDriveInfo drives
  netIfs' <- traverse fromCapnpTemplateNetIfInfo netIfs
  sharedDirs' <- traverse fromCapnpTemplateSharedDirInfo sharedDirs
  audioDevices' <- traverse fromCapnpTemplateAudioDeviceInfo audioDevices
  graphicsAdapter' <- fromCapnpGraphicsAdapter graphicsAdapter
  let sshKeys' = map fromCapnpTemplateSshKeyInfo sshKeys
  let ci = fromCapnpCloudInitInfo cloudInitConfig
  pure
    P.TemplateDetails
      { P.tvdId = id
      , P.tvdName = name
      , P.tvdCpuCount = fromIntegral cpuCount
      , P.tvdRamMb = fromIntegral ramMb
      , P.tvdDescription = if description == mempty then Nothing else Just description
      , P.tvdHeadless = headless
      , P.tvdCloudInit = cloudInit
      , P.tvdGuestAgent = guestAgent
      , P.tvdTpm = tpm
      , P.tvdAutostart = autostart
      , P.tvdRebootQuirk = rebootQuirk
      , P.tvdCloudInitConfig = if ci == emptyCloudInitInfo then Nothing else Just ci
      , P.tvdCreatedAt = nanosToUtcTime createdAt
      , P.tvdDrives = drives'
      , P.tvdNetIfs = netIfs'
      , P.tvdSshKeys = sshKeys'
      , P.tvdSharedDirs = sharedDirs'
      , P.tvdAudioDevices = audioDevices'
      , P.tvdGraphicsAdapter = graphicsAdapter'
      , P.tvdVsock = vsock
      , P.tvdBalloon = balloon
      , P.tvdRng = rng
      }
