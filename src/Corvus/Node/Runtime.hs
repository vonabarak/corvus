{-# LANGUAGE OverloadedStrings #-}

-- | Runtime directory management for VMs.
-- Handles creation of runtime directories and socket paths.
-- All functions accept QemuConfig to support per-daemon runtime directories.
module Corvus.Node.Runtime
  ( -- * Runtime directories
    getVmRuntimeDir
  , createVmRuntimeDir

    -- * Socket paths
  , getMonitorSocket
  , getQmpSocket
  , getSerialSocket
  , getGuestAgentSocket

    -- * Persistent per-VM files (basePath, not runtimeDir)
  , getSavedStateFile
  )
where

import Corvus.Qemu.Config (QemuConfig, getEffectiveBasePath, getEffectiveRuntimeDir)
import Data.Int (Int64)
import qualified Data.Text as T
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

--------------------------------------------------------------------------------
-- Persistent per-VM Files
--------------------------------------------------------------------------------

-- | Path to the per-VM saved-state file, anchored at @basePath@
-- (where disk images live) — NOT @runtimeDir@ — because the file
-- must survive host reboots to be useful. Convention matches the
-- per-VM directory used for cloud-init ISOs:
-- @\<basePath\>/\<vmName\>/state.qemu@. No directory is created
-- here; callers that write the file ensure the parent exists.
getSavedStateFile :: QemuConfig -> T.Text -> IO FilePath
getSavedStateFile config vmName = do
  basePath <- getEffectiveBasePath config
  pure $ basePath </> T.unpack vmName </> "state.qemu"
