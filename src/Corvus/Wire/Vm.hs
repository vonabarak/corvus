{-# LANGUAGE RecordWildCards #-}

-- | Cap'n Proto conversion for VM info / details + nested drive and
-- net-interface info.
module Corvus.Wire.Vm
  ( toCapnpVmInfo
  , fromCapnpVmInfo
  , toCapnpDriveInfo
  , fromCapnpDriveInfo
  , toCapnpNetIfInfo
  , fromCapnpNetIfInfo
  , toCapnpVmDetails
  , fromCapnpVmDetails
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Vm as CGVm
import qualified Corvus.Protocol.CloudInit as PCI
import qualified Corvus.Protocol.SharedDir as PSD
import qualified Corvus.Protocol.Vm as P
import Corvus.Wire.CloudInit (fromCapnpCloudInitInfo, toCapnpCloudInitInfo)
import Corvus.Wire.Enums
  ( fromCapnpCacheType
  , fromCapnpDriveFormat
  , fromCapnpDriveInterface
  , fromCapnpDriveMedia
  , fromCapnpNetInterfaceType
  , fromCapnpVmStatus
  , toCapnpCacheType
  , toCapnpDriveFormat
  , toCapnpDriveInterface
  , toCapnpDriveMedia
  , toCapnpNetInterfaceType
  , toCapnpVmStatus
  )
import Corvus.Wire.Errors (WireError)
import Corvus.Wire.SharedDir (fromCapnpSharedDirInfo, toCapnpSharedDirInfo)
import Corvus.Wire.Time (nanosToUtcTime, nanosToUtcTimeMaybe, utcTimeToNanos, utcTimeToNanosMaybe)
import Data.Maybe (fromMaybe, isJust)

-- A 'CloudInitInfo' with all fields empty, used as the on-the-wire
-- "absent" sentinel for the @vmDetails.cloudInitConfig@ field.
emptyCloudInitInfo :: PCI.CloudInitInfo
emptyCloudInitInfo =
  PCI.CloudInitInfo
    { PCI.ciiUserData = Nothing
    , PCI.ciiNetworkConfig = Nothing
    , PCI.ciiInjectSshKeys = False
    }

-- ---------------------------------------------------------------------
-- VmInfo (list-view summary)
-- ---------------------------------------------------------------------

toCapnpVmInfo :: P.VmInfo -> C.Parsed CGVm.VmInfo
toCapnpVmInfo P.VmInfo {..} =
  CGVm.VmInfo
    { CGVm.id = viId
    , CGVm.name = viName
    , CGVm.nodeId = viNodeId
    , CGVm.nodeName = viNodeName
    , CGVm.status = toCapnpVmStatus viStatus
    , CGVm.cpuCount = fromIntegral viCpuCount
    , CGVm.ramMb = fromIntegral viRamMb
    , CGVm.headless = viHeadless
    , CGVm.guestAgent = viGuestAgent
    , CGVm.cloudInit = viCloudInit
    , CGVm.lastHealthcheck = utcTimeToNanosMaybe viHealthcheck
    , CGVm.autostart = viAutostart
    , CGVm.rebootQuirk = viRebootQuirk
    , CGVm.cpuModel = viCpuModel
    }

fromCapnpVmInfo :: C.Parsed CGVm.VmInfo -> Either WireError P.VmInfo
fromCapnpVmInfo CGVm.VmInfo {..} = do
  status' <- fromCapnpVmStatus status
  pure
    P.VmInfo
      { P.viId = id
      , P.viName = name
      , P.viNodeId = nodeId
      , P.viNodeName = nodeName
      , P.viStatus = status'
      , P.viCpuCount = fromIntegral cpuCount
      , P.viRamMb = fromIntegral ramMb
      , P.viHeadless = headless
      , P.viGuestAgent = guestAgent
      , P.viCloudInit = cloudInit
      , P.viHealthcheck = nanosToUtcTimeMaybe lastHealthcheck
      , P.viAutostart = autostart
      , P.viRebootQuirk = rebootQuirk
      , P.viCpuModel = cpuModel
      }

-- ---------------------------------------------------------------------
-- DriveInfo
-- ---------------------------------------------------------------------

-- The schema's DriveInfo.media is a non-optional enum, so 'Nothing' on
-- the Haskell side is encoded as MediaDisk (the default) and round-trip
-- through the wire is lossy: 'Nothing' becomes 'Just MediaDisk'.
toCapnpDriveInfo :: P.DriveInfo -> C.Parsed CGVm.DriveInfo
toCapnpDriveInfo P.DriveInfo {..} =
  CGVm.DriveInfo
    { CGVm.id = diId
    , CGVm.diskImageId = diDiskImageId
    , CGVm.diskImageName = diDiskImageName
    , CGVm.interface = toCapnpDriveInterface diInterface
    , CGVm.filePath = diFilePath
    , CGVm.format = toCapnpDriveFormat diFormat
    , CGVm.media = maybe (toCapnpDriveMedia minBound) toCapnpDriveMedia diMedia
    , CGVm.readOnly = diReadOnly
    , CGVm.cacheType = toCapnpCacheType diCacheType
    , CGVm.discard = diDiscard
    }

fromCapnpDriveInfo :: C.Parsed CGVm.DriveInfo -> Either WireError P.DriveInfo
fromCapnpDriveInfo CGVm.DriveInfo {..} = do
  iface <- fromCapnpDriveInterface interface
  fmt <- fromCapnpDriveFormat format
  med <- fromCapnpDriveMedia media
  cache <- fromCapnpCacheType cacheType
  pure
    P.DriveInfo
      { P.diId = id
      , P.diDiskImageId = diskImageId
      , P.diDiskImageName = diskImageName
      , P.diInterface = iface
      , P.diFilePath = filePath
      , P.diFormat = fmt
      , P.diMedia = Just med
      , P.diReadOnly = readOnly
      , P.diCacheType = cache
      , P.diDiscard = discard
      }

-- ---------------------------------------------------------------------
-- NetIfInfo
-- ---------------------------------------------------------------------

toCapnpNetIfInfo :: P.NetIfInfo -> C.Parsed CGVm.NetIfInfo
toCapnpNetIfInfo P.NetIfInfo {..} =
  CGVm.NetIfInfo
    { CGVm.id = niId
    , CGVm.type_ = toCapnpNetInterfaceType niType
    , CGVm.hostDevice = niHostDevice
    , CGVm.macAddress = niMacAddress
    , CGVm.networkId = fromMaybe 0 niNetworkId
    , CGVm.networkName = fromMaybe mempty niNetworkName
    , CGVm.guestIpAddresses = fromMaybe mempty niGuestIpAddresses
    , CGVm.ipAddress = fromMaybe mempty niIpAddress
    }

fromCapnpNetIfInfo :: C.Parsed CGVm.NetIfInfo -> Either WireError P.NetIfInfo
fromCapnpNetIfInfo CGVm.NetIfInfo {..} = do
  t <- fromCapnpNetInterfaceType type_
  pure
    P.NetIfInfo
      { P.niId = id
      , P.niType = t
      , P.niHostDevice = hostDevice
      , P.niMacAddress = macAddress
      , P.niNetworkId = if networkId == 0 then Nothing else Just networkId
      , P.niNetworkName = if networkName == mempty then Nothing else Just networkName
      , P.niGuestIpAddresses =
          if guestIpAddresses == mempty then Nothing else Just guestIpAddresses
      , P.niIpAddress =
          if ipAddress == mempty then Nothing else Just ipAddress
      }

-- ---------------------------------------------------------------------
-- VmDetails
-- ---------------------------------------------------------------------

-- The Cap'n Proto schema's 'VmDetails' carries 'sharedDirs', which the
-- existing 'Protocol.Vm.VmDetails' does not. The encoder accepts the
-- shared-dir list as an explicit argument; the server-side handler is
-- expected to fetch it alongside the rest of the VM state.
toCapnpVmDetails
  :: P.VmDetails
  -> [PSD.SharedDirInfo]
  -> C.Parsed CGVm.VmDetails
toCapnpVmDetails P.VmDetails {..} sharedDirs =
  CGVm.VmDetails
    { CGVm.id = vdId
    , CGVm.name = vdName
    , CGVm.nodeId = vdNodeId
    , CGVm.nodeName = vdNodeName
    , CGVm.createdAt = utcTimeToNanos vdCreatedAt
    , CGVm.status = toCapnpVmStatus vdStatus
    , CGVm.cpuCount = fromIntegral vdCpuCount
    , CGVm.ramMb = fromIntegral vdRamMb
    , CGVm.description = fromMaybe mempty vdDescription
    , CGVm.drives = map toCapnpDriveInfo vdDrives
    , CGVm.netIfs = map toCapnpNetIfInfo vdNetIfs
    , CGVm.sharedDirs = map toCapnpSharedDirInfo sharedDirs
    , CGVm.headless = vdHeadless
    , CGVm.monitorSocket = vdMonitorSocket
    , CGVm.spicePort = maybe 0 fromIntegral vdSpicePort
    , CGVm.vsockCid = maybe 0 fromIntegral vdVsockCid
    , CGVm.serialSocket = vdSerialSocket
    , CGVm.guestAgentSocket = vdGuestAgentSocket
    , CGVm.guestAgent = vdGuestAgent
    , CGVm.cloudInit = vdCloudInit
    , CGVm.cloudInitConfig =
        maybe (toCapnpCloudInitInfo emptyCloudInitInfo) toCapnpCloudInitInfo vdCloudInitConfig
    , CGVm.lastHealthcheck = utcTimeToNanosMaybe vdHealthcheck
    , CGVm.autostart = vdAutostart
    , CGVm.errorMessage = fromMaybe mempty vdErrorMessage
    , CGVm.lastErrorAt = utcTimeToNanosMaybe vdLastErrorAt
    , CGVm.rebootQuirk = vdRebootQuirk
    , CGVm.cpuModel = vdCpuModel
    , -- Slice 3 will populate this from the daemon-side stats
      -- ring buffer; until then every VmDetails ships a
      -- zero-filled sample so the wire encoder has the field.
      CGVm.stats = zeroVmStats
    }

-- | Default zero-filled 'VmStats'. Used as a placeholder until the
-- daemon's stats-cache (slice 3) attaches a real sample.
zeroVmStats :: C.Parsed CGVm.VmStats
zeroVmStats =
  CGVm.VmStats
    { CGVm.sampledAtNanos = 0
    , CGVm.intervalMillis = 0
    , CGVm.cpuJiffiesTotal = 0
    , CGVm.clkTck = 0
    , CGVm.hostRssBytes = 0
    , CGVm.balloonActualBytes = 0
    , CGVm.balloonMaxBytes = 0
    , CGVm.drives = []
    , CGVm.nets = []
    }

-- | Reverse direction. Returns shared-dirs separately so the caller
-- can recombine them with the protocol-side 'VmDetails' (which does
-- not carry them).
fromCapnpVmDetails
  :: C.Parsed CGVm.VmDetails
  -> Either WireError (P.VmDetails, [PSD.SharedDirInfo])
fromCapnpVmDetails CGVm.VmDetails {..} = do
  status' <- fromCapnpVmStatus status
  drives' <- traverse fromCapnpDriveInfo drives
  netIfs' <- traverse fromCapnpNetIfInfo netIfs
  sharedDirs' <- traverse fromCapnpSharedDirInfo sharedDirs
  let ci = fromCapnpCloudInitInfo cloudInitConfig
  pure
    ( P.VmDetails
        { P.vdId = id
        , P.vdName = name
        , P.vdNodeId = nodeId
        , P.vdNodeName = nodeName
        , P.vdCreatedAt = nanosToUtcTime createdAt
        , P.vdStatus = status'
        , P.vdCpuCount = fromIntegral cpuCount
        , P.vdRamMb = fromIntegral ramMb
        , P.vdDescription = if description == mempty then Nothing else Just description
        , P.vdDrives = drives'
        , P.vdNetIfs = netIfs'
        , P.vdHeadless = headless
        , P.vdMonitorSocket = monitorSocket
        , P.vdSpicePort = if spicePort == 0 then Nothing else Just (fromIntegral spicePort)
        , P.vdVsockCid = if vsockCid == 0 then Nothing else Just (fromIntegral vsockCid)
        , P.vdSerialSocket = serialSocket
        , P.vdGuestAgentSocket = guestAgentSocket
        , P.vdGuestAgent = guestAgent
        , P.vdCloudInit = cloudInit
        , P.vdCloudInitConfig = if ci == emptyCloudInitInfo then Nothing else Just ci
        , P.vdHealthcheck = nanosToUtcTimeMaybe lastHealthcheck
        , P.vdAutostart = autostart
        , P.vdErrorMessage = if errorMessage == mempty then Nothing else Just errorMessage
        , P.vdLastErrorAt = nanosToUtcTimeMaybe lastErrorAt
        , P.vdRebootQuirk = rebootQuirk
        , P.vdCpuModel = cpuModel
        }
    , sharedDirs'
    )
