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
import Corvus.Protocol.Aeson (innerOptions)
import Corvus.Protocol.CloudInit (CloudInitInfo)
import Data.Aeson (ToJSON (..), genericToJSON)
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
  , vdSpicePort :: !(Maybe Int)
  -- ^ TCP port QEMU is serving SPICE on (when the VM is running and
  -- not headless). 'Nothing' for headless VMs or stopped VMs.
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
  toJSON = genericToJSON innerOptions

instance ToJSON DriveInfo where
  toJSON = genericToJSON innerOptions

instance ToJSON NetIfInfo where
  toJSON = genericToJSON innerOptions

instance ToJSON VmDetails where
  toJSON = genericToJSON innerOptions
