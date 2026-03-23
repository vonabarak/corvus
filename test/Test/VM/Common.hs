{-# LANGUAGE OverloadedStrings #-}

-- | Common helpers for VM-based integration tests.
module Test.VM.Common
  ( -- * Single-VM convenience wrappers
    withTestVm
  , withTestVmBios
  , withTestVmConsole
  , withTestVmBiosConsole

    -- * Building blocks for multi-VM tests
  , withTestDiskSetup
  )
where

import Control.Exception (bracket)
import Corvus.Client (DiskResult (..), diskClone, diskCreateOverlay, diskDelete, diskRegister)
import Corvus.Model (DriveFormat (..), DriveInterface (..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Test.DSL.Daemon
import Test.Daemon (TestDaemon, withDaemonConnection, withTestDaemon)
import Test.Database (TestEnv)
import Test.VM.Console (SerialConsole)
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

--------------------------------------------------------------------------------
-- Building blocks
--------------------------------------------------------------------------------

-- | Set up disk images and optionally OVMF firmware on an existing daemon.
-- Registers base image, creates overlay, optionally sets up UEFI firmware.
-- Handles cleanup of overlay and OVMF vars clone.
--
-- Use this directly for multi-VM tests where multiple VMs share one daemon.
withTestDiskSetup
  :: TestDaemon
  -> VmConfig
  -> Bool
  -- ^ Use UEFI (True) or BIOS (False)
  -> (Int64 -> VmConfig -> IO a)
  -- ^ Callback: overlay disk ID, final config (with OVMF disks if UEFI)
  -> IO a
withTestDiskSetup daemon config useUefi callback = do
  (baseName, overlayName, ovmfCodeName, ovmfVarsTemplateName, ovmfVarsName) <- uniqueDiskNames

  -- Ensure base image exists (locally)
  imageResult <- ensureBaseImage (vmcOsName config)
  localBasePath <- case imageResult of
    Left err -> fail $ "Failed to get base image: " <> T.unpack err
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

  if useUefi
    then do
      -- Register OVMF firmware disks
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
                  [ (ovmfCodeId, InterfacePflash, True)
                  , (ovmfVarsId, InterfacePflash, False)
                  ]
              }

      bracket
        (pure ())
        ( \_ -> do
            _ <- withDaemonConnection daemon $ \conn -> diskDelete conn overlayDiskId
            _ <- withDaemonConnection daemon $ \conn -> diskDelete conn ovmfVarsId
            pure ()
        )
        (\_ -> callback overlayDiskId configWithOvmf)
    else
      bracket
        (pure ())
        ( \_ -> do
            _ <- withDaemonConnection daemon $ \conn -> diskDelete conn overlayDiskId
            pure ()
        )
        (\_ -> callback overlayDiskId config)

--------------------------------------------------------------------------------
-- Single-VM convenience wrappers
--------------------------------------------------------------------------------

-- | Run a test with a UEFI daemon-managed VM with SSH access.
withTestVm :: TestEnv -> VmConfig -> (DaemonVm -> IO a) -> IO a
withTestVm env config action =
  withTestDaemon env $ \daemon ->
    withTestDiskSetup daemon config True $ \diskId cfg ->
      withDaemonVmWithConfig daemon diskId cfg action

-- | Run a test with a BIOS-booted daemon-managed VM with SSH access.
withTestVmBios :: TestEnv -> VmConfig -> (DaemonVm -> IO a) -> IO a
withTestVmBios env config action =
  withTestDaemon env $ \daemon ->
    withTestDiskSetup daemon config False $ \diskId cfg ->
      withDaemonVmWithConfig daemon diskId cfg action

-- | Run a test with a UEFI daemon-managed VM connected via serial console.
-- Starts the VM and connects to the serial console immediately (no SSH).
withTestVmConsole :: TestEnv -> VmConfig -> (SerialConsole -> IO a) -> IO a
withTestVmConsole env config action =
  withTestDaemon env $ \daemon ->
    withTestDiskSetup daemon config True $ \diskId cfg ->
      withDaemonVmConsole daemon diskId cfg action

-- | Run a test with a BIOS-booted daemon-managed VM connected via serial console.
-- Starts the VM and connects to the serial console immediately (no SSH).
withTestVmBiosConsole :: TestEnv -> VmConfig -> (SerialConsole -> IO a) -> IO a
withTestVmBiosConsole env config action =
  withTestDaemon env $ \daemon ->
    withTestDiskSetup daemon config False $ \diskId cfg ->
      withDaemonVmConsole daemon diskId cfg action
