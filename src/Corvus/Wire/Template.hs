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
  ( fromCapnpCacheType
  , fromCapnpDriveFormat
  , fromCapnpDriveInterface
  , fromCapnpDriveMedia
  , fromCapnpNetInterfaceType
  , fromCapnpSharedDirCache
  , fromCapnpTemplateCloneStrategy
  , toCapnpCacheType
  , toCapnpDriveFormat
  , toCapnpDriveInterface
  , toCapnpDriveMedia
  , toCapnpNetInterfaceType
  , toCapnpSharedDirCache
  , toCapnpTemplateCloneStrategy
  )
import Corvus.Wire.Errors (WireError)
import Corvus.Wire.Time (nanosToUtcTime, utcTimeToNanos)
import Data.Maybe (fromMaybe, isJust)

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
    , CGT.autostart = tviAutostart
    , CGT.rebootQuirk = tviRebootQuirk
    }

fromCapnpTemplateVmInfo :: C.Parsed CGT.TemplateVmInfo -> P.TemplateVmInfo
fromCapnpTemplateVmInfo CGT.TemplateVmInfo {..} =
  P.TemplateVmInfo
    { P.tviId = id
    , P.tviName = name
    , P.tviCpuCount = fromIntegral cpuCount
    , P.tviRamMb = fromIntegral ramMb
    , P.tviDescription = if description == mempty then Nothing else Just description
    , P.tviHeadless = headless
    , P.tviGuestAgent = guestAgent
    , P.tviAutostart = autostart
    , P.tviRebootQuirk = rebootQuirk
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
    , CGT.hostDevice = fromMaybe mempty tvniHostDevice
    , CGT.network = fromMaybe mempty tvniNetwork
    }

fromCapnpTemplateNetIfInfo
  :: C.Parsed CGT.TemplateNetIfInfo
  -> Either WireError P.TemplateNetIfInfo
fromCapnpTemplateNetIfInfo CGT.TemplateNetIfInfo {..} = do
  t <- fromCapnpNetInterfaceType type_
  pure
    P.TemplateNetIfInfo
      { P.tvniType = t
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
    , CGT.autostart = tvdAutostart
    , CGT.cloudInitConfig =
        maybe (toCapnpCloudInitInfo emptyCloudInitInfo) toCapnpCloudInitInfo tvdCloudInitConfig
    , CGT.createdAt = utcTimeToNanos tvdCreatedAt
    , CGT.drives = map toCapnpTemplateDriveInfo tvdDrives
    , CGT.netIfs = map toCapnpTemplateNetIfInfo tvdNetIfs
    , CGT.sshKeys = map toCapnpTemplateSshKeyInfo tvdSshKeys
    , CGT.rebootQuirk = tvdRebootQuirk
    , CGT.sharedDirs = map toCapnpTemplateSharedDirInfo tvdSharedDirs
    }

fromCapnpTemplateDetails
  :: C.Parsed CGT.TemplateDetails
  -> Either WireError P.TemplateDetails
fromCapnpTemplateDetails CGT.TemplateDetails {..} = do
  drives' <- traverse fromCapnpTemplateDriveInfo drives
  netIfs' <- traverse fromCapnpTemplateNetIfInfo netIfs
  sharedDirs' <- traverse fromCapnpTemplateSharedDirInfo sharedDirs
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
      , P.tvdAutostart = autostart
      , P.tvdRebootQuirk = rebootQuirk
      , P.tvdCloudInitConfig = if ci == emptyCloudInitInfo then Nothing else Just ci
      , P.tvdCreatedAt = nanosToUtcTime createdAt
      , P.tvdDrives = drives'
      , P.tvdNetIfs = netIfs'
      , P.tvdSshKeys = sshKeys'
      , P.tvdSharedDirs = sharedDirs'
      }
