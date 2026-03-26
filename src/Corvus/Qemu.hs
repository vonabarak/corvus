-- | QEMU virtual machine management.
--
-- This module re-exports the main functionality from submodules:
--
-- * "Corvus.Qemu.Config" - QEMU configuration options
-- * "Corvus.Qemu.Runtime" - Runtime directory and socket management
-- * "Corvus.Qemu.Qmp" - QMP protocol interaction
-- * "Corvus.Qemu.Process" - VM process management
-- * "Corvus.Qemu.Command" - QEMU command line generation
-- * "Corvus.Qemu.Virtiofsd" - Virtiofsd process management for shared directories
-- * "Corvus.Qemu.Image" - Disk image management using qemu-img
module Corvus.Qemu
  ( -- * Configuration
    QemuConfig (..)
  , defaultQemuConfig
  , getEffectiveBasePath

    -- * Runtime directories
  , createVmRuntimeDir
  , getVmRuntimeDir
  , getMonitorSocket
  , getSpiceSocket
  , getSerialSocket
  , getQmpSocket
  , getGuestAgentSocket

    -- * VM execution
  , startVm
  , StartVmResult (..)

    -- * QMP interaction
  , QmpResult (..)
  , qmpShutdown
  , qmpContinue
  , qmpStop
  , qmpBlockdevAdd
  , qmpDeviceAddDrive
  , qmpDeviceDel
  , qmpBlockdevDel
  , qmpQQ

    -- * Process management
  , killVmProcess
  , KillResult (..)

    -- * Virtiofsd (shared directories)
  , startVirtiofsdProcesses
  , killVirtiofsdProcesses
  , VirtiofsdResult (..)

    -- * Command generation
  , generateQemuCommand
  , generateQemuCommandIO

    -- * Guest Agent
  , GuestExecResult (..)
  , GuestIpAddress (..)
  , GuestNetIf (..)
  , guestExec
  , guestPing
  , guestShutdown
  , guestNetworkGetInterfaces

    -- * Disk image management
  , createImage
  , deleteImage
  , resizeImage
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

import Corvus.Qemu.Command
import Corvus.Qemu.Config
import Corvus.Qemu.GuestAgent
import Corvus.Qemu.Image
import Corvus.Qemu.Process
import Corvus.Qemu.Qmp
import Corvus.Qemu.Runtime
import Corvus.Qemu.Virtiofsd
