{-# LANGUAGE OverloadedStrings #-}

-- | Runtime directory management for VMs.
-- Handles creation of runtime directories and socket paths.
-- All functions accept QemuConfig to support per-daemon runtime directories.
module Corvus.Qemu.Runtime
  ( -- * Runtime directories
    getVmRuntimeDir
  , createVmRuntimeDir

    -- * Network runtime directories
  , getNetworkRuntimeDir
  , createNetworkRuntimeDir
  , getBridgeName
  , getTapUpScript
  , getTapInterfaceName
  , getDnsmasqPidFile
  , getDnsmasqLeaseFile

    -- * Socket paths
  , getMonitorSocket
  , getQmpSocket
  , getSpiceSocket
  , getSerialSocket
  , getGuestAgentSocket
  , getPidFile
  )
where

import Corvus.Qemu.Config (QemuConfig, getEffectiveRuntimeDir)
import Data.Int (Int64)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Runtime Directory Management
--------------------------------------------------------------------------------

-- | Get the runtime directory for a specific VM
getVmRuntimeDir :: QemuConfig -> Int64 -> IO FilePath
getVmRuntimeDir config vmId = do
  baseDir <- getEffectiveRuntimeDir config
  pure $ baseDir </> "vms" </> show vmId

-- | Create runtime directory for a VM
-- Returns the path to the created directory
createVmRuntimeDir :: QemuConfig -> Int64 -> IO FilePath
createVmRuntimeDir config vmId = do
  vmDir <- getVmRuntimeDir config vmId
  createDirectoryIfMissing True vmDir
  pure vmDir

--------------------------------------------------------------------------------
-- Network Runtime Directories
--------------------------------------------------------------------------------

-- | Get the runtime directory for a specific network
getNetworkRuntimeDir :: QemuConfig -> Int64 -> IO FilePath
getNetworkRuntimeDir config networkId = do
  baseDir <- getEffectiveRuntimeDir config
  pure $ baseDir </> "networks" </> show networkId

-- | Create runtime directory for a network
createNetworkRuntimeDir :: QemuConfig -> Int64 -> IO FilePath
createNetworkRuntimeDir config networkId = do
  netDir <- getNetworkRuntimeDir config networkId
  createDirectoryIfMissing True netDir
  pure netDir

-- | Get the bridge name for a network (deterministic from ID)
getBridgeName :: Int64 -> String
getBridgeName networkId = "crv" ++ show networkId

-- | Get path to the tap-up script for a network
getTapUpScript :: QemuConfig -> Int64 -> IO FilePath
getTapUpScript config networkId = do
  netDir <- getNetworkRuntimeDir config networkId
  pure $ netDir </> "tap-up.sh"

-- | Get the TAP interface name for a network (deterministic from ID)
getTapInterfaceName :: Int64 -> String
getTapInterfaceName networkId = "crv" ++ show networkId

-- | Get path to dnsmasq PID file for a network
getDnsmasqPidFile :: QemuConfig -> Int64 -> IO FilePath
getDnsmasqPidFile config networkId = do
  netDir <- getNetworkRuntimeDir config networkId
  pure $ netDir </> "dnsmasq.pid"

-- | Get path to dnsmasq lease file for a network
getDnsmasqLeaseFile :: QemuConfig -> Int64 -> IO FilePath
getDnsmasqLeaseFile config networkId = do
  netDir <- getNetworkRuntimeDir config networkId
  pure $ netDir </> "dnsmasq.leases"

--------------------------------------------------------------------------------
-- Socket Paths
--------------------------------------------------------------------------------

-- | Get path to HMP monitor socket for a VM
getMonitorSocket :: QemuConfig -> Int64 -> IO FilePath
getMonitorSocket config vmId = do
  vmDir <- getVmRuntimeDir config vmId
  pure $ vmDir </> "monitor.sock"

-- | Get path to QMP socket for a VM
getQmpSocket :: QemuConfig -> Int64 -> IO FilePath
getQmpSocket config vmId = do
  vmDir <- getVmRuntimeDir config vmId
  pure $ vmDir </> "qmp.sock"

-- | Get path to SPICE socket for a VM
getSpiceSocket :: QemuConfig -> Int64 -> IO FilePath
getSpiceSocket config vmId = do
  vmDir <- getVmRuntimeDir config vmId
  pure $ vmDir </> "spice.sock"

-- | Get path to serial console socket for a VM
getSerialSocket :: QemuConfig -> Int64 -> IO FilePath
getSerialSocket config vmId = do
  vmDir <- getVmRuntimeDir config vmId
  pure $ vmDir </> "serial.sock"

-- | Get path to QEMU Guest Agent socket for a VM
getGuestAgentSocket :: QemuConfig -> Int64 -> IO FilePath
getGuestAgentSocket config vmId = do
  vmDir <- getVmRuntimeDir config vmId
  pure $ vmDir </> "qga.sock"

-- | Get path to PID file for a VM
getPidFile :: QemuConfig -> Int64 -> IO FilePath
getPidFile config vmId = do
  vmDir <- getVmRuntimeDir config vmId
  pure $ vmDir </> "qemu.pid"
