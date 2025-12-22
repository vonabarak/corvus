{-# LANGUAGE OverloadedStrings #-}

-- | Runtime directory management for VMs.
-- Handles creation of runtime directories and socket paths.
module Corvus.Qemu.Runtime
  ( -- * Runtime directories
    getCorvusRuntimeDir,
    getVmRuntimeDir,
    createVmRuntimeDir,

    -- * Socket paths
    getMonitorSocket,
    getQmpSocket,
    getSpiceSocket,
    getPidFile,
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

-- | Get path to PID file for a VM
getPidFile :: Int64 -> IO FilePath
getPidFile vmId = do
  vmDir <- getVmRuntimeDir vmId
  pure $ vmDir </> "qemu.pid"
