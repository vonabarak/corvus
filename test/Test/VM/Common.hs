{-# LANGUAGE OverloadedStrings #-}

-- | Common helpers for VM-based integration tests.
module Test.VM.Common
  ( withTestVm
  , withTestVmBios
  )
where

import Control.Exception (bracket)
import Corvus.Client (DiskResult (..), SshKeyResult (..), diskClone, diskCreateOverlay, diskDelete, diskRegister, sshKeyAttach, sshKeyCreate)
import Corvus.Model (DriveFormat (..), DriveInterface (..))
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

-- | Generate unique disk names for test isolation
uniqueDiskNames :: IO (Text, Text, Text, Text, Text)
uniqueDiskNames = do
  uuid <- nextRandom
  let suffix = T.take 8 (toText uuid)
  pure
    ( "base-image-" <> suffix
    , "test-overlay-" <> suffix
    , "ovmf-code-" <> suffix
    , "ovmf-vars-template-" <> suffix
    , "ovmf-vars-" <> suffix
    )

-- | Run a test with a daemon-managed VM.
-- This handles all setup: daemon, disk image, VM creation, and cleanup.
withTestVm :: TestEnv -> VmConfig -> (DaemonVm -> IO a) -> IO a
withTestVm env config action = do
  daemon <- startTestDaemon env
  (baseName, overlayName, ovmfCodeName, ovmfVarsTemplateName, ovmfVarsName) <- uniqueDiskNames

  -- Ensure base image exists (locally)
  imageResult <- ensureBaseImage (vmcOsName config)
  localBasePath <- case imageResult of
    Left err -> do
      stopTestDaemon daemon
      fail $ "Failed to get base image: " <> T.unpack err
    Right path -> pure path

  -- Register base image with daemon
  resBase <- withDaemonConnection daemon $ \conn ->
    diskRegister conn baseName (T.pack localBasePath) FormatQcow2 Nothing
  baseDiskId <- case resBase of
    Right (Right (DiskCreated dId)) -> pure dId
    Right (Left err) -> fail $ "Failed to register base disk: " <> show err
    Right (Right other) -> fail $ "Unexpected response registering base disk: " <> show other
    Left err -> fail $ "Connection error registering base disk: " <> show err

  -- Create overlay via daemon
  resOverlay <- withDaemonConnection daemon $ \conn ->
    diskCreateOverlay conn overlayName baseDiskId
  overlayDiskId <- case resOverlay of
    Right (Right (DiskCreated dId)) -> pure dId
    Right (Left err) -> fail $ "Failed to create overlay: " <> show err
    Right (Right other) -> fail $ "Unexpected response creating overlay: " <> show other
    Left err -> fail $ "Connection error creating overlay: " <> show err

  -- Register OVMF disks
  resOvmfCode <- withDaemonConnection daemon $ \conn ->
    diskRegister conn ovmfCodeName "/usr/share/edk2/OvmfX64/OVMF_CODE.fd" FormatRaw Nothing
  ovmfCodeId <- case resOvmfCode of
    Right (Right (DiskCreated dId)) -> pure dId
    Right (Left err) -> fail $ "Failed to register OVMF_CODE disk: " <> show err
    _ -> fail "Unexpected response registering OVMF_CODE"

  resOvmfVarsTemplate <- withDaemonConnection daemon $ \conn ->
    diskRegister conn ovmfVarsTemplateName "/usr/share/edk2/OvmfX64/OVMF_VARS.fd" FormatRaw Nothing
  ovmfVarsTemplateId <- case resOvmfVarsTemplate of
    Right (Right (DiskCreated dId)) -> pure dId
    Right (Left err) -> fail $ "Failed to register OVMF_VARS template disk: " <> show err
    _ -> fail "Unexpected response registering OVMF_VARS template"

  -- Clone OVMF vars for this test
  resOvmfVars <- withDaemonConnection daemon $ \conn ->
    diskClone conn ovmfVarsName ovmfVarsTemplateId Nothing
  ovmfVarsId <- case resOvmfVars of
    Right (Right (DiskCreated dId)) -> pure dId
    Right (Left err) -> fail $ "Failed to clone OVMF_VARS: " <> show err
    _ -> fail "Unexpected response cloning OVMF_VARS"

  let configWithOvmf =
        config
          { vmcAdditionalDisks =
              [ (ovmfCodeId, InterfacePflash, True) -- Read-only
              , (ovmfVarsId, InterfacePflash, False) -- Read-write
              ]
          }

  -- Use bracket to ensure cleanup of the VM, overlay, and cloned OVMF vars
  bracket
    (pure ())
    ( \_ -> do
        -- Delete overlay via RPC (also deletes the file since it's in daemon's basePath)
        _ <- withDaemonConnection daemon $ \conn -> diskDelete conn overlayDiskId
        -- Delete cloned OVMF vars
        _ <- withDaemonConnection daemon $ \conn -> diskDelete conn ovmfVarsId
        stopTestDaemon daemon
    )
    ( \_ -> do
        withDaemonVmWithConfig daemon overlayDiskId configWithOvmf action
    )

-- | Run a test with a BIOS-booted daemon-managed VM (no OVMF firmware).
-- Use this for images that boot via legacy BIOS (e.g. alpine-3.20-bios).
withTestVmBios :: TestEnv -> VmConfig -> (DaemonVm -> IO a) -> IO a
withTestVmBios env config action = do
  daemon <- startTestDaemon env
  (baseName, overlayName, _, _, _) <- uniqueDiskNames

  -- Ensure base image exists (locally)
  imageResult <- ensureBaseImage (vmcOsName config)
  localBasePath <- case imageResult of
    Left err -> do
      stopTestDaemon daemon
      fail $ "Failed to get base image: " <> T.unpack err
    Right path -> pure path

  -- Register base image with daemon
  resBase <- withDaemonConnection daemon $ \conn ->
    diskRegister conn baseName (T.pack localBasePath) FormatQcow2 Nothing
  baseDiskId <- case resBase of
    Right (Right (DiskCreated dId)) -> pure dId
    Right (Left err) -> fail $ "Failed to register base disk: " <> show err
    Right (Right other) -> fail $ "Unexpected response registering base disk: " <> show other
    Left err -> fail $ "Connection error registering base disk: " <> show err

  -- Create overlay via daemon
  resOverlay <- withDaemonConnection daemon $ \conn ->
    diskCreateOverlay conn overlayName baseDiskId
  overlayDiskId <- case resOverlay of
    Right (Right (DiskCreated dId)) -> pure dId
    Right (Left err) -> fail $ "Failed to create overlay: " <> show err
    Right (Right other) -> fail $ "Unexpected response creating overlay: " <> show other
    Left err -> fail $ "Connection error creating overlay: " <> show err

  -- No OVMF firmware for BIOS boot
  bracket
    (pure ())
    ( \_ -> do
        _ <- withDaemonConnection daemon $ \conn -> diskDelete conn overlayDiskId
        stopTestDaemon daemon
    )
    ( \_ -> do
        withDaemonVmWithConfig daemon overlayDiskId config action
    )
