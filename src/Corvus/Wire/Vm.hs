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
  , toCapnpAudioDeviceInfo
  , fromCapnpAudioDeviceInfo
  , toCapnpVmDetails
  , fromCapnpVmDetails
  , toCapnpVmStats
  , fromCapnpVmStats
  , toCapnpVmSnapshotInfo
  , fromCapnpVmSnapshotInfo
  , zeroVmStats
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Vm as CGVm
import qualified Corvus.Protocol.CloudInit as PCI
import qualified Corvus.Protocol.SharedDir as PSD
import qualified Corvus.Protocol.Vm as P
import Corvus.Wire.CloudInit (fromCapnpCloudInitInfo, toCapnpCloudInitInfo)
import Corvus.Wire.Common
  ( fromCapnpNamedRef
  , fromCapnpNamedRefOpt
  , toCapnpNamedRef
  , toCapnpNamedRefOpt
  )
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
  , fromCapnpVmStatus
  , toCapnpAudioBackend
  , toCapnpAudioDeviceModel
  , toCapnpCacheType
  , toCapnpDriveFormat
  , toCapnpDriveInterface
  , toCapnpDriveMedia
  , toCapnpGraphicsAdapter
  , toCapnpNetInterfaceType
  , toCapnpNetworkDeviceModel
  , toCapnpVmStatus
  )
import Corvus.Wire.Errors (WireError)
import Corvus.Wire.SharedDir (fromCapnpSharedDirInfo, toCapnpSharedDirInfo)
import Corvus.Wire.Time (nanosToUtcTime, nanosToUtcTimeMaybe, utcTimeToNanos, utcTimeToNanosMaybe)
import Data.Maybe (fromMaybe)

toCapnpAudioDeviceInfo :: P.AudioDeviceInfo -> C.Parsed CGVm.AudioDeviceInfo
toCapnpAudioDeviceInfo P.AudioDeviceInfo {..} =
  CGVm.AudioDeviceInfo
    { CGVm.id = adiId
    , CGVm.backend = toCapnpAudioBackend adiBackend
    , CGVm.model = toCapnpAudioDeviceModel adiModel
    , CGVm.options = adiOptions
    }

fromCapnpAudioDeviceInfo :: C.Parsed CGVm.AudioDeviceInfo -> Either WireError P.AudioDeviceInfo
fromCapnpAudioDeviceInfo CGVm.AudioDeviceInfo {..} = do
  backend' <- fromCapnpAudioBackend backend
  model' <- fromCapnpAudioDeviceModel model
  pure P.AudioDeviceInfo {P.adiId = id, P.adiBackend = backend', P.adiModel = model', P.adiOptions = options}

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
    , CGVm.node = toCapnpNamedRef viNode
    , CGVm.status = toCapnpVmStatus viStatus
    , CGVm.cpuCount = fromIntegral viCpuCount
    , CGVm.ram = fromIntegral viRam
    , CGVm.headless = viHeadless
    , CGVm.guestAgent = viGuestAgent
    , CGVm.tpm = viTpm
    , CGVm.cloudInit = viCloudInit
    , CGVm.lastHealthcheck = utcTimeToNanosMaybe viHealthcheck
    , CGVm.autostart = viAutostart
    , CGVm.rebootQuirk = viRebootQuirk
    , CGVm.cpuModel = viCpuModel
    , CGVm.graphicsAdapter = toCapnpGraphicsAdapter viGraphicsAdapter
    , CGVm.vsock = viVsock
    , CGVm.balloon = viBalloon
    , CGVm.rng = viRng
    }

fromCapnpVmInfo :: C.Parsed CGVm.VmInfo -> Either WireError P.VmInfo
fromCapnpVmInfo CGVm.VmInfo {..} = do
  status' <- fromCapnpVmStatus status
  graphicsAdapter' <- fromCapnpGraphicsAdapter graphicsAdapter
  pure
    P.VmInfo
      { P.viId = id
      , P.viName = name
      , P.viNode = fromCapnpNamedRef node
      , P.viStatus = status'
      , P.viCpuCount = fromIntegral cpuCount
      , P.viRam = fromIntegral ram
      , P.viHeadless = headless
      , P.viGuestAgent = guestAgent
      , P.viTpm = tpm
      , P.viCloudInit = cloudInit
      , P.viHealthcheck = nanosToUtcTimeMaybe lastHealthcheck
      , P.viAutostart = autostart
      , P.viRebootQuirk = rebootQuirk
      , P.viCpuModel = cpuModel
      , P.viGraphicsAdapter = graphicsAdapter'
      , P.viVsock = vsock
      , P.viBalloon = balloon
      , P.viRng = rng
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
    , CGVm.diskImage = toCapnpNamedRefOpt diDiskImage
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
      , P.diDiskImage = fromCapnpNamedRefOpt diskImage
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
    , CGVm.model = toCapnpNetworkDeviceModel niModel
    , CGVm.hostDevice = niHostDevice
    , CGVm.macAddress = niMacAddress
    , CGVm.network = toCapnpNamedRefOpt niNetwork
    , CGVm.guestIpAddresses = fromMaybe mempty niGuestIpAddresses
    , CGVm.ipAddress = fromMaybe mempty niIpAddress
    }

fromCapnpNetIfInfo :: C.Parsed CGVm.NetIfInfo -> Either WireError P.NetIfInfo
fromCapnpNetIfInfo CGVm.NetIfInfo {..} = do
  t <- fromCapnpNetInterfaceType type_
  model' <- fromCapnpNetworkDeviceModel model
  pure
    P.NetIfInfo
      { P.niId = id
      , P.niType = t
      , P.niModel = model'
      , P.niHostDevice = hostDevice
      , P.niMacAddress = macAddress
      , P.niNetwork = fromCapnpNamedRefOpt network
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
  -> C.Parsed CGVm.VmStats
  -- ^ Latest cached resource sample (or 'zeroVmStats' when the
  -- daemon hasn't seen one for this VM yet). The protocol-side
  -- @vdStats@ field is ignored here; the daemon owns the
  -- authoritative sample via its in-memory ring buffer.
  -> C.Parsed CGVm.VmDetails
toCapnpVmDetails P.VmDetails {..} sharedDirs stats =
  CGVm.VmDetails
    { CGVm.id = vdId
    , CGVm.name = vdName
    , CGVm.node = toCapnpNamedRef vdNode
    , CGVm.createdAt = utcTimeToNanos vdCreatedAt
    , CGVm.status = toCapnpVmStatus vdStatus
    , CGVm.cpuCount = fromIntegral vdCpuCount
    , CGVm.ram = fromIntegral vdRam
    , CGVm.description = fromMaybe mempty vdDescription
    , CGVm.drives = map toCapnpDriveInfo vdDrives
    , CGVm.netIfs = map toCapnpNetIfInfo vdNetIfs
    , CGVm.sharedDirs = map toCapnpSharedDirInfo sharedDirs
    , CGVm.audioDevices = map toCapnpAudioDeviceInfo vdAudioDevices
    , CGVm.headless = vdHeadless
    , CGVm.monitorSocket = vdMonitorSocket
    , CGVm.spicePort = maybe 0 fromIntegral vdSpicePort
    , CGVm.vsockCid = maybe 0 fromIntegral vdVsockCid
    , CGVm.vsock = vdVsock
    , CGVm.balloon = vdBalloon
    , CGVm.rng = vdRng
    , CGVm.serialSocket = vdSerialSocket
    , CGVm.guestAgentSocket = vdGuestAgentSocket
    , CGVm.guestAgent = vdGuestAgent
    , CGVm.tpm = vdTpm
    , CGVm.cloudInit = vdCloudInit
    , CGVm.cloudInitConfig =
        maybe (toCapnpCloudInitInfo emptyCloudInitInfo) toCapnpCloudInitInfo vdCloudInitConfig
    , CGVm.lastHealthcheck = utcTimeToNanosMaybe vdHealthcheck
    , CGVm.autostart = vdAutostart
    , CGVm.errorMessage = fromMaybe mempty vdErrorMessage
    , CGVm.lastErrorAt = utcTimeToNanosMaybe vdLastErrorAt
    , CGVm.rebootQuirk = vdRebootQuirk
    , CGVm.cpuModel = vdCpuModel
    , CGVm.graphicsAdapter = toCapnpGraphicsAdapter vdGraphicsAdapter
    , CGVm.stats = stats
    }

-- | Default zero-filled 'VmStats'. Used as a placeholder for VMs
-- the daemon has no cached sample for yet (e.g. just-created VM
-- before the agent's first push).
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
  graphicsAdapter' <- fromCapnpGraphicsAdapter graphicsAdapter
  drives' <- traverse fromCapnpDriveInfo drives
  netIfs' <- traverse fromCapnpNetIfInfo netIfs
  sharedDirs' <- traverse fromCapnpSharedDirInfo sharedDirs
  audioDevices' <- traverse fromCapnpAudioDeviceInfo audioDevices
  let ci = fromCapnpCloudInitInfo cloudInitConfig
  pure
    ( P.VmDetails
        { P.vdId = id
        , P.vdName = name
        , P.vdNode = fromCapnpNamedRef node
        , P.vdCreatedAt = nanosToUtcTime createdAt
        , P.vdStatus = status'
        , P.vdCpuCount = fromIntegral cpuCount
        , P.vdRam = fromIntegral ram
        , P.vdDescription = if description == mempty then Nothing else Just description
        , P.vdDrives = drives'
        , P.vdNetIfs = netIfs'
        , P.vdAudioDevices = audioDevices'
        , P.vdHeadless = headless
        , P.vdMonitorSocket = monitorSocket
        , P.vdSpicePort = if spicePort == 0 then Nothing else Just (fromIntegral spicePort)
        , P.vdVsockCid = if vsockCid == 0 then Nothing else Just (fromIntegral vsockCid)
        , P.vdVsock = vsock
        , P.vdBalloon = balloon
        , P.vdRng = rng
        , P.vdSerialSocket = serialSocket
        , P.vdGuestAgentSocket = guestAgentSocket
        , P.vdGuestAgent = guestAgent
        , P.vdTpm = tpm
        , P.vdCloudInit = cloudInit
        , P.vdCloudInitConfig = if ci == emptyCloudInitInfo then Nothing else Just ci
        , P.vdHealthcheck = nanosToUtcTimeMaybe lastHealthcheck
        , P.vdAutostart = autostart
        , P.vdErrorMessage = if errorMessage == mempty then Nothing else Just errorMessage
        , P.vdLastErrorAt = nanosToUtcTimeMaybe lastErrorAt
        , P.vdRebootQuirk = rebootQuirk
        , P.vdCpuModel = cpuModel
        , P.vdGraphicsAdapter = graphicsAdapter'
        , P.vdStats = fromCapnpVmStats stats
        }
    , sharedDirs'
    )

-- ---------------------------------------------------------------------------
-- VmStats converters

toCapnpVmStats :: P.VmStats -> C.Parsed CGVm.VmStats
toCapnpVmStats P.VmStats {..} =
  CGVm.VmStats
    { CGVm.sampledAtNanos = vstSampledAtNanos
    , CGVm.intervalMillis = vstIntervalMillis
    , CGVm.cpuJiffiesTotal = vstCpuJiffiesTotal
    , CGVm.clkTck = vstClkTck
    , CGVm.hostRssBytes = vstHostRssBytes
    , CGVm.balloonActualBytes = vstBalloonActualBytes
    , CGVm.balloonMaxBytes = vstBalloonMaxBytes
    , CGVm.drives = map toCapnpDriveIo vstDrives
    , CGVm.nets = map toCapnpNetIo vstNets
    }

fromCapnpVmStats :: C.Parsed CGVm.VmStats -> P.VmStats
fromCapnpVmStats CGVm.VmStats {..} =
  P.VmStats
    { P.vstSampledAtNanos = sampledAtNanos
    , P.vstIntervalMillis = intervalMillis
    , P.vstCpuJiffiesTotal = cpuJiffiesTotal
    , P.vstClkTck = clkTck
    , P.vstHostRssBytes = hostRssBytes
    , P.vstBalloonActualBytes = balloonActualBytes
    , P.vstBalloonMaxBytes = balloonMaxBytes
    , P.vstDrives = map fromCapnpDriveIo drives
    , P.vstNets = map fromCapnpNetIo nets
    }

toCapnpDriveIo :: P.DriveIo -> C.Parsed CGVm.DriveIo
toCapnpDriveIo P.DriveIo {..} =
  CGVm.DriveIo
    { CGVm.name = dioName
    , CGVm.readBytesTotal = dioReadBytesTotal
    , CGVm.writeBytesTotal = dioWriteBytesTotal
    , CGVm.readOpsTotal = dioReadOpsTotal
    , CGVm.writeOpsTotal = dioWriteOpsTotal
    }

fromCapnpDriveIo :: C.Parsed CGVm.DriveIo -> P.DriveIo
fromCapnpDriveIo CGVm.DriveIo {..} =
  P.DriveIo
    { P.dioName = name
    , P.dioReadBytesTotal = readBytesTotal
    , P.dioWriteBytesTotal = writeBytesTotal
    , P.dioReadOpsTotal = readOpsTotal
    , P.dioWriteOpsTotal = writeOpsTotal
    }

toCapnpNetIo :: P.NetIo -> C.Parsed CGVm.NetIo
toCapnpNetIo P.NetIo {..} =
  CGVm.NetIo
    { CGVm.tapName = nioTapName
    , CGVm.rxBytesTotal = nioRxBytesTotal
    , CGVm.txBytesTotal = nioTxBytesTotal
    }

fromCapnpNetIo :: C.Parsed CGVm.NetIo -> P.NetIo
fromCapnpNetIo CGVm.NetIo {..} =
  P.NetIo
    { P.nioTapName = tapName
    , P.nioRxBytesTotal = rxBytesTotal
    , P.nioTxBytesTotal = txBytesTotal
    }

-- ---------------------------------------------------------------------
-- VmSnapshotInfo
-- ---------------------------------------------------------------------

toCapnpVmSnapshotInfo :: P.VmSnapshotInfo -> C.Parsed CGVm.VmSnapshotInfo
toCapnpVmSnapshotInfo P.VmSnapshotInfo {..} =
  CGVm.VmSnapshotInfo
    { CGVm.name = vsiName
    , CGVm.createdAt = utcTimeToNanos vsiCreatedAt
    , CGVm.vm = toCapnpNamedRef vsiVm
    , CGVm.carrierDisk = toCapnpNamedRef vsiCarrierDisk
    , CGVm.diskCount = fromIntegral vsiDiskCount
    , CGVm.totalSize = vsiTotalSize
    }

fromCapnpVmSnapshotInfo :: C.Parsed CGVm.VmSnapshotInfo -> P.VmSnapshotInfo
fromCapnpVmSnapshotInfo CGVm.VmSnapshotInfo {..} =
  P.VmSnapshotInfo
    { P.vsiName = name
    , P.vsiCreatedAt = nanosToUtcTime createdAt
    , P.vsiVm = fromCapnpNamedRef vm
    , P.vsiCarrierDisk = fromCapnpNamedRef carrierDisk
    , P.vsiDiskCount = fromIntegral diskCount
    , P.vsiTotalSize = totalSize
    }
