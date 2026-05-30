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
  , VmStats (..)
  , DriveIo (..)
  , NetIo (..)
  , zeroVmStats
  )
where

import Corvus.Model (CacheType, DriveFormat, DriveInterface, DriveMedia, NetInterfaceType, VmStatus)
import Corvus.Protocol.CloudInit (CloudInitInfo)
import Corvus.Protocol.JsonOptions (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

-- | VM summary for list view
data VmInfo = VmInfo
  { viId :: !Int64
  , viName :: !Text
  , viNodeId :: !Int64
  , viNodeName :: !Text
  -- ^ Display name of the node the VM lives on. "(deleted)" if
  -- the Node row was removed out from under the VM (shouldn't
  -- happen in practice — the FK is enforced — but the daemon
  -- returns a sentinel rather than aborting the list).
  , viStatus :: !VmStatus
  , viCpuCount :: !Int
  , viRamMb :: !Int
  , viHeadless :: !Bool
  , viGuestAgent :: !Bool
  , viCloudInit :: !Bool
  , viHealthcheck :: !(Maybe UTCTime)
  , viAutostart :: !Bool
  , viRebootQuirk :: !Bool
  , viCpuModel :: !Text
  -- ^ QEMU @-cpu@ model. Default @"host"@ for back-compat; see
  -- @schema/vm.capnp::VmInfo.cpuModel@ for migration-safety
  -- trade-off.
  }
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

-- | Network interface info for details view
data NetIfInfo = NetIfInfo
  { niId :: !Int64
  , niType :: !NetInterfaceType
  , niHostDevice :: !Text
  , niMacAddress :: !Text
  , niNetworkId :: !(Maybe Int64)
  , niNetworkName :: !(Maybe Text)
  , niGuestIpAddresses :: !(Maybe Text)
  , niIpAddress :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

-- | Full VM details
data VmDetails = VmDetails
  { vdId :: !Int64
  , vdName :: !Text
  , vdNodeId :: !Int64
  , vdNodeName :: !Text
  -- ^ Display name of the node the VM lives on. See 'viNodeName'
  -- for the deleted-row sentinel convention.
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
  , vdVsockCid :: !(Maybe Int)
  -- ^ AF_VSOCK CID assigned to the VM at creation, used as
  -- @guest-cid=@ on the @vhost-vsock-pci@ device. Stable across
  -- starts. 'Nothing' only for legacy rows that predate this field.
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
  , vdErrorMessage :: !(Maybe Text)
  -- ^ Last failure reason when the daemon transitioned this VM to
  -- 'VmError'. Cleared on successful transitions back to a healthy
  -- state. 'Nothing' when the VM is healthy or never errored.
  , vdLastErrorAt :: !(Maybe UTCTime)
  -- ^ Timestamp paired with 'vdErrorMessage'.
  , vdRebootQuirk :: !Bool
  -- ^ When true, QEMU runs with @-no-reboot@ and the agent
  -- re-spawns on each guest-initiated exit. Used to dodge OVMF
  -- firmware reboot hangs (tianocore/edk2#12441).
  , vdCpuModel :: !Text
  -- ^ See @viCpuModel@.
  , vdStats :: !VmStats
  -- ^ Most-recent resource-consumption sample from the agent's
  -- StatusPoller. 'zeroVmStats' (all zeros, sampledAtNanos == 0)
  -- when the VM is stopped or the daemon hasn't yet received its
  -- first sample.
  }
  deriving (Eq, Show, Generic)

-- | One resource-consumption sample. Cumulative counters +
-- instant gauges; consumers compute rates as
-- @delta(counter) / intervalMillis@. See @schema/vm.capnp::VmStats@
-- for the wire shape and @doc/observability.md@ for the metric
-- semantics.
data VmStats = VmStats
  { vstSampledAtNanos :: !Word64
  -- ^ Epoch nanos at sample time. @0@ when no sample has landed
  -- (zero placeholder).
  , vstIntervalMillis :: !Word32
  -- ^ Wall time since this VM's previous sample. @0@ on the
  -- first sample after VM start.
  , vstCpuJiffiesTotal :: !Word64
  -- ^ Cumulative user + system jiffies since QEMU launch. Convert
  -- to seconds via 'vstClkTck'.
  , vstClkTck :: !Word32
  -- ^ @_SC_CLK_TCK@ as observed on the agent host (typically 100).
  , vstHostRssBytes :: !Word64
  -- ^ /proc/<qemu-pid>/status:VmRSS — what the host pays for.
  , vstBalloonActualBytes :: !Word64
  -- ^ Current memory the guest sees, in bytes. @0@ when the VM
  -- has no virtio-balloon device.
  , vstBalloonMaxBytes :: !Word64
  -- ^ VM's configured RAM ceiling. @0@ when no balloon device.
  , vstDrives :: ![DriveIo]
  , vstNets :: ![NetIo]
  }
  deriving (Eq, Show, Generic)

-- | Per-drive cumulative I/O counters.
data DriveIo = DriveIo
  { dioName :: !Text
  -- ^ QEMU device id (matches @-blockdev id=@).
  , dioReadBytesTotal :: !Word64
  , dioWriteBytesTotal :: !Word64
  , dioReadOpsTotal :: !Word64
  , dioWriteOpsTotal :: !Word64
  }
  deriving (Eq, Show, Generic)

-- | Per-TAP cumulative throughput counters.
data NetIo = NetIo
  { nioTapName :: !Text
  , nioRxBytesTotal :: !Word64
  , nioTxBytesTotal :: !Word64
  }
  deriving (Eq, Show, Generic)

-- | Empty-sample sentinel. Used by the daemon when a VM has no
-- cached sample yet (e.g. newly created, just before the first
-- agent push) and by 'fromCapnpVmDetails' when decoding a wire
-- struct whose stats sub-struct is all zeros.
zeroVmStats :: VmStats
zeroVmStats =
  VmStats
    { vstSampledAtNanos = 0
    , vstIntervalMillis = 0
    , vstCpuJiffiesTotal = 0
    , vstClkTck = 0
    , vstHostRssBytes = 0
    , vstBalloonActualBytes = 0
    , vstBalloonMaxBytes = 0
    , vstDrives = []
    , vstNets = []
    }

instance ToJSON VmInfo where
  toJSON = genericToJSON innerOptions

instance ToJSON DriveInfo where
  toJSON = genericToJSON innerOptions

instance ToJSON NetIfInfo where
  toJSON = genericToJSON innerOptions

instance ToJSON VmDetails where
  toJSON = genericToJSON innerOptions

instance ToJSON VmStats where
  toJSON = genericToJSON innerOptions

instance ToJSON DriveIo where
  toJSON = genericToJSON innerOptions

instance ToJSON NetIo where
  toJSON = genericToJSON innerOptions
