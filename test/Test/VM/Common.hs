{-# LANGUAGE OverloadedStrings #-}

-- | Common helpers for VM-based integration tests.
module Test.VM.Common
  ( -- * Types (re-exported from Test.VM.Types)
    VmConfig (..)
  , DefaultVmConfig (..)
  , TestVm (..)
  , biosVmConfig
  , cloudVmConfig
  , prebakedImageName
  , prebakedSshKeyPath

    -- * All-in-one VM wrappers (TestEnv → daemon → disk → VM)
  , withTestVm
  , withTestVmConsole
  , withTestVmGuestExec

    -- * Daemon-level VM wrappers (disk setup + VM creation)
  , withTestVmOnDaemon
  , withTestVmConsoleOnDaemon
  , withTestVmGuestExecOnDaemon

    -- * Low-level VM wrappers (explicit disk ID)
  , withTestVmSshWithDisk
  , withTestVmGuestExecWithDisk
  , withTestVmConsoleWithDisk

    -- * VM lifecycle helpers
  , startTestVmAndWait
  , startTestVmAndWaitGuestAgent

    -- * Building blocks for multi-VM / custom disk tests
  , withTestDiskSetup

    -- * Utilities
  , findFreePort
  , waitForVmStopped
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Corvus.Client (DiskResult (..), diskClone, diskCreateOverlay, diskDelete, diskRegister)
import Corvus.Model (DriveFormat (..), DriveInterface (..))
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Network.Socket
  ( Family (..)
  , SocketType (..)
  , close
  , defaultProtocol
  , socket
  )
import qualified Network.Socket as NS
import System.IO.Temp (withSystemTempDirectory)
import Test.Database (TestEnv)
import Test.VM.Console (SerialConsole, connectSerialConsole)
import Test.VM.Daemon (TestDaemon (..), withDaemonConnection, withTestDaemon)
import Test.VM.Image (ensureBaseImage)
import Test.VM.Rpc
import Test.VM.Ssh (waitForTestVmSshWithKey)
import Test.VM.Types

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
  -> (Int64 -> VmConfig -> IO a)
  -- ^ Callback: overlay disk ID, final config (with OVMF disks if UEFI)
  -> IO a
withTestDiskSetup daemon config callback = do
  let useUefi = vmcUefi config
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
    diskCreateOverlay conn overlayName (T.pack (show baseDiskId)) Nothing
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
        diskClone conn ovmfVarsName (T.pack (show ovmfVarsTemplateId)) (Just (ovmfVarsName <> ".fd"))
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
            _ <- withDaemonConnection daemon $ \conn -> diskDelete conn (T.pack (show overlayDiskId))
            _ <- withDaemonConnection daemon $ \conn -> diskDelete conn (T.pack (show ovmfVarsId))
            pure ()
        )
        (\_ -> callback overlayDiskId configWithOvmf)
    else
      bracket
        (pure ())
        ( \_ -> do
            _ <- withDaemonConnection daemon $ \conn -> diskDelete conn (T.pack (show overlayDiskId))
            pure ()
        )
        (\_ -> callback overlayDiskId config)

--------------------------------------------------------------------------------
-- Low-level VM wrappers (explicit disk ID)
--------------------------------------------------------------------------------

-- | Run an action with a test VM using a config and explicit disk ID.
-- Creates the VM with necessary configuration, sets up SSH keys,
-- starts it, runs the action, then stops and deletes the VM.
withTestVmSshWithDisk
  :: TestDaemon
  -> Int64
  -- ^ Disk image ID to use for the boot disk
  -> VmConfig
  -- ^ VM configuration
  -> (TestVm -> IO a)
  -> IO a
withTestVmSshWithDisk daemon diskImageId config action = do
  -- Use a unique name for the VM
  vmUuid <- nextRandom
  let vmName = "test-vm-" <> T.take 8 (toText vmUuid)
      -- Enable cloud-init for cloud images that need SSH key injection
      needsCloudInit = isNothing (vmcPrebakedSshKey config)

  -- Use bracket for robust VM lifecycle management
  bracket
    (createTestVmWithOptions daemon vmName (vmcCpuCount config) (vmcRamMb config) (vmcDescription config) (vmcHeadless config) False needsCloudInit)
    ( \vmId -> do
        -- Cleanup: stop and delete the VM
        stopTestVmAndWait daemon vmId 30
        deleteTestVm daemon vmId
    )
    $ \vmId -> do
      -- Add boot disk
      addVmDisk daemon vmId diskImageId (vmcDiskInterface config) (vmcDiskCache config) (vmcDiskDiscard config) False

      -- Add additional disks
      mapM_ (\(dId, iface, ro) -> addVmDisk daemon vmId dId iface (vmcDiskCache config) (vmcDiskDiscard config) ro) (vmcAdditionalDisks config)

      -- Find a free port for SSH forwarding
      sshPort <- findFreePort

      -- Add network interface with SSH port forwarding (server generates MAC)
      let hostFwd = "hostfwd=tcp::" <> T.pack (show sshPort) <> "-:22"
      addVmNetIf daemon vmId (vmcNetworkType config) hostFwd Nothing

      -- Add VDE network interface for virtual network if requested
      forM_ (vmcNetworkId config) (addVmNetIfWithNetwork daemon vmId)

      -- Add shared directory if requested
      case vmcSharedDir config of
        Nothing -> pure ()
        Just path -> addVmSharedDir daemon vmId (T.pack path) "share" (vmcSharedDirCache config)

      let startAndRun privateKey sshKeyId = do
            let vm =
                  TestVm
                    { tvmId = vmId
                    , tvmDiskId = diskImageId
                    , tvmSshPort = sshPort
                    , tvmSshHost = "localhost"
                    , tvmDaemon = daemon
                    , tvmSshPrivateKey = privateKey
                    , tvmSshKeyId = sshKeyId
                    , tvmSshUser = vmcSshUser config
                    }

            -- Start the VM and wait for SSH
            putStrLn "[test] Starting VM and waiting for SSH..."
            vmStartTime <- getCurrentTime
            startTestVmAndWait vm (vmcWaitSshTimeout config)
            vmReadyTime <- getCurrentTime
            let bootSec = round (diffUTCTime vmReadyTime vmStartTime) :: Int
            putStrLn $ "[test] SSH is ready (boot to SSH: " <> show bootSec <> "s)"
            putStrLn $ "[test] ssh " <> T.unpack (tvmSshUser vm) <> "@" <> tvmSshHost vm <> " -p " <> show (tvmSshPort vm) <> " -i " <> tvmSshPrivateKey vm

            -- Run the action
            action vm

      case vmcPrebakedSshKey config of
        Just keyPath ->
          -- Pre-baked image: SSH key is already in the image, skip cloud-init
          startAndRun keyPath 0
        Nothing ->
          -- Cloud image: generate SSH key and deploy via cloud-init
          withSystemTempDirectory "corvus-test-ssh" $ \tmpDir -> do
            bracket
              (setupVmSshKey daemon vmId tmpDir)
              (\(sshKeyId, _, _) -> cleanupSshKey daemon sshKeyId)
              $ \(sshKeyId, privateKey, _publicKey) ->
                startAndRun privateKey sshKeyId

-- | Run an action with a test VM using guest agent execution and explicit disk ID.
-- Creates the VM with guest agent enabled, sets up disks and network,
-- starts it, waits for the guest agent to become available, then runs the action.
-- No SSH setup is needed.
withTestVmGuestExecWithDisk
  :: TestDaemon
  -> Int64
  -- ^ Disk image ID to use for the boot disk
  -> VmConfig
  -- ^ VM configuration
  -> (TestVm -> IO a)
  -> IO a
withTestVmGuestExecWithDisk daemon diskImageId config action = do
  -- Use a unique name for the VM
  vmUuid <- nextRandom
  let vmName = "test-vm-" <> T.take 8 (toText vmUuid)

  -- Use bracket for robust VM lifecycle management
  bracket
    (createTestVmWithGuestAgent daemon vmName (vmcCpuCount config) (vmcRamMb config) (vmcDescription config) (vmcHeadless config))
    ( \vmId -> do
        -- Cleanup: stop and delete the VM
        stopTestVmAndWait daemon vmId 30
        deleteTestVm daemon vmId
    )
    $ \vmId -> do
      -- Add boot disk
      addVmDisk daemon vmId diskImageId (vmcDiskInterface config) (vmcDiskCache config) (vmcDiskDiscard config) False

      -- Add additional disks
      mapM_ (\(dId, iface, ro) -> addVmDisk daemon vmId dId iface (vmcDiskCache config) (vmcDiskDiscard config) ro) (vmcAdditionalDisks config)

      -- Add network interface (no SSH port forwarding needed)
      addVmNetIf daemon vmId (vmcNetworkType config) "" Nothing

      -- Add VDE network interface for virtual network if requested
      forM_ (vmcNetworkId config) (addVmNetIfWithNetwork daemon vmId)

      -- Add shared directory if requested
      case vmcSharedDir config of
        Nothing -> pure ()
        Just path -> addVmSharedDir daemon vmId (T.pack path) "share" (vmcSharedDirCache config)

      let vm =
            TestVm
              { tvmId = vmId
              , tvmDiskId = diskImageId
              , tvmSshPort = 0
              , tvmSshHost = ""
              , tvmDaemon = daemon
              , tvmSshPrivateKey = ""
              , tvmSshKeyId = 0
              , tvmSshUser = ""
              }

      -- Start the VM and wait for guest agent
      putStrLn "[test] Starting VM and waiting for guest agent..."
      vmStartTime <- getCurrentTime
      startTestVm daemon vmId
      waitForGuestAgent daemon vmId (vmcWaitSshTimeout config)
      vmReadyTime <- getCurrentTime
      let bootSec = round (diffUTCTime vmReadyTime vmStartTime) :: Int
      putStrLn $ "[test] Guest agent is ready (boot to agent: " <> show bootSec <> "s)"

      -- Run the action
      action vm

-- | Run an action with a test VM connected via serial console and explicit disk ID.
-- Creates the VM, adds disks and network, starts it, connects to the
-- serial console immediately (no SSH setup, no waiting for SSH).
withTestVmConsoleWithDisk
  :: TestDaemon
  -> Int64
  -- ^ Disk image ID to use for the boot disk
  -> VmConfig
  -- ^ VM configuration (vmcHeadless should be True)
  -> (SerialConsole -> IO a)
  -> IO a
withTestVmConsoleWithDisk daemon diskImageId config action = do
  -- Use a unique name for the VM
  vmUuid <- nextRandom
  let vmName = "test-vm-" <> T.take 8 (toText vmUuid)

  -- Use bracket for robust VM lifecycle management
  bracket
    (createTestVm daemon vmName (vmcCpuCount config) (vmcRamMb config) (vmcDescription config) (vmcHeadless config))
    ( \vmId -> do
        -- Cleanup: stop and delete the VM
        stopTestVmAndWait daemon vmId 30
        deleteTestVm daemon vmId
    )
    $ \vmId -> do
      -- Add boot disk
      addVmDisk daemon vmId diskImageId (vmcDiskInterface config) (vmcDiskCache config) (vmcDiskDiscard config) False

      -- Add additional disks
      mapM_ (\(dId, iface, ro) -> addVmDisk daemon vmId dId iface (vmcDiskCache config) (vmcDiskDiscard config) ro) (vmcAdditionalDisks config)

      -- Add network interface (no SSH port forwarding needed, but network may still be useful)
      addVmNetIf daemon vmId (vmcNetworkType config) "" Nothing

      -- Start the VM
      putStrLn "[test] Starting VM for serial console access..."
      startTestVm daemon vmId

      -- Connect to serial console immediately
      connectSerialConsole (ssQemuConfig (tdState daemon)) vmId action

--------------------------------------------------------------------------------
-- VM lifecycle helpers
--------------------------------------------------------------------------------

-- | Start a VM and wait for SSH connection to be established.
-- Fails if SSH is not established within timeout.
startTestVmAndWait :: TestVm -> Int -> IO ()
startTestVmAndWait vm timeoutSec = do
  startTestVm (tvmDaemon vm) (tvmId vm)
  waitForTestVmSshWithKey (tvmSshHost vm) (tvmSshPort vm) (tvmSshPrivateKey vm) (tvmSshUser vm) timeoutSec

-- | Start a VM and wait for the guest agent to become available.
-- Fails if the guest agent is not ready within timeout.
startTestVmAndWaitGuestAgent :: TestVm -> Int -> IO ()
startTestVmAndWaitGuestAgent vm timeoutSec = do
  startTestVm (tvmDaemon vm) (tvmId vm)
  waitForGuestAgent (tvmDaemon vm) (tvmId vm) timeoutSec

--------------------------------------------------------------------------------
-- Daemon-level VM convenience wrappers (disk setup + VM creation)
--------------------------------------------------------------------------------

-- | Run a test with a VM on an existing daemon (SSH access).
-- Uses vmcUefi from config to select UEFI or BIOS boot.
withTestVmOnDaemon :: TestDaemon -> VmConfig -> (TestVm -> IO a) -> IO a
withTestVmOnDaemon daemon config action =
  withTestDiskSetup daemon config $ \diskId cfg ->
    withTestVmSshWithDisk daemon diskId cfg action

-- | Run a test with a VM connected via serial console on an existing daemon.
withTestVmConsoleOnDaemon :: TestDaemon -> VmConfig -> (SerialConsole -> IO a) -> IO a
withTestVmConsoleOnDaemon daemon config action =
  withTestDiskSetup daemon config $ \diskId cfg ->
    withTestVmConsoleWithDisk daemon diskId cfg action

-- | Run a test with a VM using guest-exec on an existing daemon.
withTestVmGuestExecOnDaemon :: TestDaemon -> VmConfig -> (TestVm -> IO a) -> IO a
withTestVmGuestExecOnDaemon daemon config action =
  withTestDiskSetup daemon config $ \diskId cfg ->
    withTestVmGuestExecWithDisk daemon diskId cfg action

--------------------------------------------------------------------------------
-- All-in-one VM wrappers (TestEnv → daemon → disk → VM)
--------------------------------------------------------------------------------

-- | Run a test with a VM (SSH access). Creates daemon and handles all setup.
-- Uses vmcUefi from config to select UEFI or BIOS boot.
withTestVm :: TestEnv -> VmConfig -> (TestVm -> IO a) -> IO a
withTestVm env config action =
  withTestDaemon env $ \daemon ->
    withTestVmOnDaemon daemon config action

-- | Run a test with a VM connected via serial console. Creates daemon and handles all setup.
withTestVmConsole :: TestEnv -> VmConfig -> (SerialConsole -> IO a) -> IO a
withTestVmConsole env config action =
  withTestDaemon env $ \daemon ->
    withTestVmConsoleOnDaemon daemon config action

-- | Run a test with a VM using guest-exec. Creates daemon and handles all setup.
withTestVmGuestExec :: TestEnv -> VmConfig -> (TestVm -> IO a) -> IO a
withTestVmGuestExec env config action =
  withTestDaemon env $ \daemon ->
    withTestVmGuestExecOnDaemon daemon config action

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Wait for VM to fully stop (dummy for now, just delays)
waitForVmStopped :: Int -> IO ()
waitForVmStopped seconds = threadDelay (seconds * 1000000)

-- | Find a free TCP port
findFreePort :: IO Int
findFreePort = do
  sock <- socket AF_INET NS.Stream defaultProtocol
  NS.bind sock (NS.SockAddrInet 0 0)
  addr <- NS.getSocketName sock
  close sock
  case addr of
    NS.SockAddrInet port _ -> pure $ fromIntegral port
    _ -> pure 2222 -- Fallback
