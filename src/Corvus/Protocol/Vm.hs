{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | VM subsystem response data.
--
-- 'VmInfo' is the compact list-view summary; 'VmDetails' is the full
-- per-VM payload including attached drives, network interfaces, and an
-- optional custom cloud-init config.
module Corvus.Protocol.Vm
  ( VmInfo (..)
  , DriveInfo (..)
  , NetIfInfo (..)
  , VmDetails (..)
  )
where

import Corvus.Model (CacheType, DriveFormat, DriveInterface, DriveMedia, NetInterfaceType, VmStatus)
import Corvus.Protocol.CloudInit (CloudInitInfo)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | VM summary for list view
data VmInfo = VmInfo
  { viId :: !Int64
  , viName :: !Text
  , viStatus :: !VmStatus
  , viCpuCount :: !Int
  , viRamMb :: !Int
  , viHeadless :: !Bool
  , viGuestAgent :: !Bool
  , viCloudInit :: !Bool
  , viHealthcheck :: !(Maybe UTCTime)
  , viAutostart :: !Bool
  }
  deriving (Eq, Show, Generic, Binary)

-- | Drive info for details view
data DriveInfo = DriveInfo
  { diId :: !Int64
  , diDiskImageId :: !Int64
  , diDiskImageName :: !Text
  , diInterface :: !DriveInterface
  , diFilePath :: !Text
  , diFormat :: !DriveFormat
  , diMedia :: !(Maybe DriveMedia)
  , diReadOnly :: !Bool
  , diCacheType :: !CacheType
  , diDiscard :: !Bool
  }
  deriving (Eq, Show, Generic, Binary)

-- | Network interface info for details view
data NetIfInfo = NetIfInfo
  { niId :: !Int64
  , niType :: !NetInterfaceType
  , niHostDevice :: !Text
  , niMacAddress :: !Text
  , niNetworkId :: !(Maybe Int64)
  , niNetworkName :: !(Maybe Text)
  , niGuestIpAddresses :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Full VM details
data VmDetails = VmDetails
  { vdId :: !Int64
  , vdName :: !Text
  , vdCreatedAt :: !UTCTime
  , vdStatus :: !VmStatus
  , vdCpuCount :: !Int
  , vdRamMb :: !Int
  , vdDescription :: !(Maybe Text)
  , vdDrives :: ![DriveInfo]
  , vdNetIfs :: ![NetIfInfo]
  , vdHeadless :: !Bool
  , vdMonitorSocket :: !Text
  -- ^ Path to HMP monitor socket
  , vdSpiceSocket :: !Text
  -- ^ Path to SPICE socket
  , vdSerialSocket :: !Text
  -- ^ Path to serial console socket
  , vdGuestAgentSocket :: !Text
  -- ^ Path to QEMU Guest Agent socket
  , vdGuestAgent :: !Bool
  -- ^ Whether guest agent is enabled for this VM
  , vdCloudInit :: !Bool
  -- ^ Whether cloud-init is enabled for this VM
  , vdCloudInitConfig :: !(Maybe CloudInitInfo)
  -- ^ Custom cloud-init configuration (Nothing = using defaults)
  , vdHealthcheck :: !(Maybe UTCTime)
  -- ^ Last successful guest agent ping time
  , vdAutostart :: !Bool
  -- ^ Whether this VM autostarts when the daemon starts
  }
  deriving (Eq, Show, Generic, Binary)

instance ToJSON VmInfo where
  toJSON v =
    object
      [ "id" .= viId v
      , "name" .= viName v
      , "status" .= viStatus v
      , "cpuCount" .= viCpuCount v
      , "ramMb" .= viRamMb v
      , "headless" .= viHeadless v
      , "guestAgent" .= viGuestAgent v
      , "cloudInit" .= viCloudInit v
      , "healthcheck" .= viHealthcheck v
      , "autostart" .= viAutostart v
      ]

instance ToJSON DriveInfo where
  toJSON d =
    object
      [ "id" .= diId d
      , "diskImageId" .= diDiskImageId d
      , "diskImageName" .= diDiskImageName d
      , "interface" .= diInterface d
      , "filePath" .= diFilePath d
      , "format" .= diFormat d
      , "media" .= diMedia d
      , "readOnly" .= diReadOnly d
      , "cacheType" .= diCacheType d
      , "discard" .= diDiscard d
      ]

instance ToJSON NetIfInfo where
  toJSON n =
    object
      [ "id" .= niId n
      , "type" .= niType n
      , "hostDevice" .= niHostDevice n
      , "macAddress" .= niMacAddress n
      , "networkId" .= niNetworkId n
      , "networkName" .= niNetworkName n
      , "guestIpAddresses" .= niGuestIpAddresses n
      ]

instance ToJSON VmDetails where
  toJSON v =
    object
      [ "id" .= vdId v
      , "name" .= vdName v
      , "createdAt" .= vdCreatedAt v
      , "status" .= vdStatus v
      , "cpuCount" .= vdCpuCount v
      , "ramMb" .= vdRamMb v
      , "description" .= vdDescription v
      , "drives" .= vdDrives v
      , "networkInterfaces" .= vdNetIfs v
      , "headless" .= vdHeadless v
      , "monitorSocket" .= vdMonitorSocket v
      , "spiceSocket" .= vdSpiceSocket v
      , "serialSocket" .= vdSerialSocket v
      , "guestAgentSocket" .= vdGuestAgentSocket v
      , "guestAgent" .= vdGuestAgent v
      , "cloudInit" .= vdCloudInit v
      , "cloudInitConfig" .= vdCloudInitConfig v
      , "healthcheck" .= vdHealthcheck v
      , "autostart" .= vdAutostart v
      ]
