{-# LANGUAGE OverloadedStrings #-}

-- | Common helpers for VM-based integration tests.
module Test.VM.Common
  ( withTestVm,
  )
where

import Control.Exception (bracket)
import Corvus.Client (DiskResult (..), SshKeyResult (..), diskCreateOverlay, diskDelete, diskRegister, sshKeyAttach, sshKeyCreate)
import Corvus.Model (DriveFormat (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import System.FilePath ((</>))
import System.IO.Temp (getCanonicalTemporaryDirectory)
import Test.DSL.Daemon
import Test.Daemon (startTestDaemon, stopTestDaemon, withDaemonConnection)
import Test.Database (TestEnv)
import Test.Settings (getImageConfig)
import Test.VM.Image (ensureBaseImage)

-- | Run a test with a daemon-managed VM.
-- This handles all setup: daemon, disk image, VM creation, and cleanup.
withTestVm :: TestEnv -> VmConfig -> (DaemonVm -> IO a) -> IO a
withTestVm env config action = do
  daemon <- startTestDaemon env

  -- Ensure base image exists (locally)
  imageResult <- ensureBaseImage (vmcOsName config)
  localBasePath <- case imageResult of
    Left err -> do
      stopTestDaemon daemon
      fail $ "Failed to get base image: " <> T.unpack err
    Right path -> pure path

  -- Register base image with daemon
  resBase <- withDaemonConnection daemon $ \conn ->
    diskRegister conn "base-image" (T.pack localBasePath) FormatQcow2 Nothing
  baseDiskId <- case resBase of
    Right (Right (DiskCreated dId)) -> pure dId
    Right (Left err) -> fail $ "Failed to register base disk: " <> show err
    Right (Right other) -> fail $ "Unexpected response registering base disk: " <> show other
    Left err -> fail $ "Connection error registering base disk: " <> show err

  -- Create overlay via daemon
  resOverlay <- withDaemonConnection daemon $ \conn ->
    diskCreateOverlay conn "test-overlay" baseDiskId
  overlayDiskId <- case resOverlay of
    Right (Right (DiskCreated dId)) -> pure dId
    Right (Left err) -> fail $ "Failed to create overlay: " <> show err
    Right (Right other) -> fail $ "Unexpected response creating overlay: " <> show other
    Left err -> fail $ "Connection error creating overlay: " <> show err

  -- Use bracket to ensure cleanup of the VM and overlay
  bracket
    (pure ())
    ( \_ -> do
        -- Delete overlay via RPC (also deletes the file since it's in daemon's basePath)
        _ <- withDaemonConnection daemon $ \conn -> diskDelete conn overlayDiskId
        stopTestDaemon daemon
    )
    ( \_ -> do
        withDaemonVmWithConfig daemon overlayDiskId config action
    )
