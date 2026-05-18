-- | QEMU virtual machine management — convenience re-exports.
--
-- After the VM-abstraction refactor (Phase 3), every host-side
-- subprocess (QEMU, virtiofsd) is owned by the agent. The daemon
-- talks to the agent through 'Corvus.NodeAgentClient', not
-- through this umbrella. The umbrella now re-exports only the
-- daemon-side pieces a few legacy importers still reach for:
-- QEMU config + runtime directory paths + QMP wrappers + chardev
-- helpers + disk image surface.
--
-- New code should import the specific @Corvus.Node.*@ submodule
-- it needs.
module Corvus.Qemu
  ( -- * Configuration
    QemuConfig (..)
  , defaultQemuConfig
  , getEffectiveBasePath

    -- * Runtime directories
  , createVmRuntimeDir
  , getVmRuntimeDir
  , getMonitorSocket
  , getSerialSocket
  , getQmpSocket
  , getGuestAgentSocket

    -- * QMP interaction
  , QmpResult (..)
  , qmpShutdown
  , qmpContinue
  , qmpStop
  , qmpSetSpicePassword
  , qmpExpireSpicePassword
  , qmpSendCtrlAltDel
  , qmpBlockdevAdd
  , qmpDeviceAddDrive
  , qmpDeviceDel
  , qmpBlockdevDel
  , qmpQQ

    -- * Command generation (daemon-side preview path; agent has its own builder)
  , generateQemuCommand
  , generateQemuCommandIO

    -- * Guest Agent (daemon-side helpers; slice C migrates these to the agent)
  , GuestExecResult (..)
  , GuestIpAddress (..)
  , GuestNetIf (..)
  , GuestAgentConns
  , closeGuestAgentConn
  , guestExec
  , guestPing
  , guestShutdown
  , guestNetworkGetInterfaces

    -- * Disk image management (read-only helpers; mutating ops go through the agent)
  , createImage
  , deleteImage
  , resizeImage
  , rebaseImage
  , getImageInfo
  , createSnapshot
  , deleteSnapshot
  , rollbackSnapshot
  , mergeSnapshot
  , listSnapshots
  , ImageInfo (..)
  , SnapshotData (..)
  , ImageResult (..)
  )
where

import Corvus.Node.Command
import Corvus.Node.GuestAgent
import Corvus.Node.Image
import Corvus.Node.Qmp
import Corvus.Node.Runtime
import Corvus.Qemu.Config
