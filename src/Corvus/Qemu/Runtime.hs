{-# LANGUAGE OverloadedStrings #-}

-- | Runtime directory management for VMs.
-- Handles creation of runtime directories and socket paths.
module Corvus.Qemu.Runtime
  ( -- * Runtime directories
    getCorvusRuntimeDir
  , getVmRuntimeDir
  , createVmRuntimeDir

    -- * Network runtime directories
  , getNetworkRuntimeDir
  , createNetworkRuntimeDir
  , getVdeSwitchSocket
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

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Runtime Directory Management
--------------------------------------------------------------------------------

-- | Get the base runtime directory for corvus
-- Uses $XDG_RUNTIME_DIR/corvus, falls back to /tmp/corvus-$UID
getCorvusRuntimeDir :: IO FilePath
getCorvusRuntimeDir = do
  mXdg <- lookupEnv "XDG_RUNTIME_DIR"
  case mXdg of
    Just xdg -> pure $ xdg </> "corvus"
    Nothing -> do
      mUid <- lookupEnv "UID"
      let uid = fromMaybe "1000" mUid
      pure $ "/tmp/corvus-" ++ uid

-- | Get the runtime directory for a specific VM
getVmRuntimeDir :: Int64 -> IO FilePath
getVmRuntimeDir vmId = do
  baseDir <- getCorvusRuntimeDir
  pure $ baseDir </> show vmId

-- | Create runtime directory for a VM
-- Returns the path to the created directory
createVmRuntimeDir :: Int64 -> IO FilePath
createVmRuntimeDir vmId = do
  vmDir <- getVmRuntimeDir vmId
  createDirectoryIfMissing True vmDir
  pure vmDir

--------------------------------------------------------------------------------
-- Network Runtime Directories
--------------------------------------------------------------------------------

-- | Get the runtime directory for a specific network
getNetworkRuntimeDir :: Int64 -> IO FilePath
getNetworkRuntimeDir networkId = do
  baseDir <- getCorvusRuntimeDir
  pure $ baseDir </> "networks" </> show networkId

-- | Create runtime directory for a network
createNetworkRuntimeDir :: Int64 -> IO FilePath
createNetworkRuntimeDir networkId = do
  netDir <- getNetworkRuntimeDir networkId
  createDirectoryIfMissing True netDir
  pure netDir

-- | Get path to vde_switch socket for a network
getVdeSwitchSocket :: Int64 -> IO FilePath
getVdeSwitchSocket networkId = do
  netDir <- getNetworkRuntimeDir networkId
  pure $ netDir </> "switch.ctl"

-- | Get the TAP interface name for a network (deterministic from ID)
getTapInterfaceName :: Int64 -> String
getTapInterfaceName networkId = "crv" ++ show networkId

-- | Get path to dnsmasq PID file for a network
getDnsmasqPidFile :: Int64 -> IO FilePath
getDnsmasqPidFile networkId = do
  netDir <- getNetworkRuntimeDir networkId
  pure $ netDir </> "dnsmasq.pid"

-- | Get path to dnsmasq lease file for a network
getDnsmasqLeaseFile :: Int64 -> IO FilePath
getDnsmasqLeaseFile networkId = do
  netDir <- getNetworkRuntimeDir networkId
  pure $ netDir </> "dnsmasq.leases"

--------------------------------------------------------------------------------
-- Socket Paths
--------------------------------------------------------------------------------

-- | Get path to HMP monitor socket for a VM
getMonitorSocket :: Int64 -> IO FilePath
getMonitorSocket vmId = do
  vmDir <- getVmRuntimeDir vmId
  pure $ vmDir </> "monitor.sock"

-- | Get path to QMP socket for a VM
getQmpSocket :: Int64 -> IO FilePath
getQmpSocket vmId = do
  vmDir <- getVmRuntimeDir vmId
  pure $ vmDir </> "qmp.sock"

-- | Get path to SPICE socket for a VM
getSpiceSocket :: Int64 -> IO FilePath
getSpiceSocket vmId = do
  vmDir <- getVmRuntimeDir vmId
  pure $ vmDir </> "spice.sock"

-- | Get path to serial console socket for a VM
getSerialSocket :: Int64 -> IO FilePath
getSerialSocket vmId = do
  vmDir <- getVmRuntimeDir vmId
  pure $ vmDir </> "serial.sock"

-- | Get path to QEMU Guest Agent socket for a VM
getGuestAgentSocket :: Int64 -> IO FilePath
getGuestAgentSocket vmId = do
  vmDir <- getVmRuntimeDir vmId
  pure $ vmDir </> "qga.sock"

-- | Get path to PID file for a VM
getPidFile :: Int64 -> IO FilePath
getPidFile vmId = do
  vmDir <- getVmRuntimeDir vmId
  pure $ vmDir </> "qemu.pid"
