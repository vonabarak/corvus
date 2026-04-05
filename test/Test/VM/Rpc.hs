{-# LANGUAGE OverloadedStrings #-}

-- | RPC call wrappers for the Corvus daemon.
-- This module provides functions to create, configure, and manage VMs
-- via RPC calls to a running daemon.
module Test.VM.Rpc
  ( -- * VM lifecycle
    createTestVm
  , createTestVmWithGuestAgent
  , createTestVmWithOptions
  , startTestVm
  , startTestVmSync
  , stopTestVm
  , stopTestVmAndWait
  , deleteTestVm

    -- * VM edit
  , editTestVm

    -- * VM configuration
  , addVmDisk
  , addVmNetIf
  , removeVmNetIf
  , listVmNetIfs
  , addVmSharedDir

    -- * SSH key management
  , setupVmSshKey
  , createSshKey
  , attachSshKey
  , cleanupSshKey

    -- * Guest execution
  , runInVm
  , runInVm_
  , runViaGuestAgent
  , runViaGuestAgent_

    -- * Virtual network management
  , createNetwork
  , createNetworkWithSubnet
  , createNetworkWithNat
  , deleteNetwork
  , startNetwork
  , stopNetwork
  , showNetwork
  , addVmNetIfWithNetwork

    -- * Cloud-init config management
  , setCloudInitConfig
  , getCloudInitConfig
  , deleteCloudInitConfig
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Corvus.Client
import Corvus.Client.Rpc (CloudInitResult (..), GuestExecResult (..), NetIfResult (..), NetworkResult (..), cloudInitDelete, cloudInitGet, cloudInitSet, networkCreate, networkDelete, networkShow, networkStart, networkStop, vmExec)
import Corvus.Model
import Corvus.Protocol (NetIfInfo (..), NetworkInfo (..), VmDetails (..))
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Qemu.Runtime (getQmpSocket)
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import Test.VM.Daemon (TestDaemon (..), withDaemonConnection)
import Test.VM.Ssh (SshKeyPair (..), generateSshKeyPair)
import Test.VM.Types (TestVm (..))

--------------------------------------------------------------------------------
-- VM Lifecycle
--------------------------------------------------------------------------------

-- | Create a VM via daemon RPC
createTestVm :: TestDaemon -> Text -> Int -> Int -> Maybe Text -> Bool -> IO Int64
createTestVm daemon name cpus ram mDesc headless =
  createTestVmFull daemon name cpus ram mDesc headless False

-- | Create a VM with guest agent via daemon RPC
createTestVmWithGuestAgent :: TestDaemon -> Text -> Int -> Int -> Maybe Text -> Bool -> IO Int64
createTestVmWithGuestAgent daemon name cpus ram mDesc headless =
  createTestVmFull daemon name cpus ram mDesc headless True

-- | Create a VM via daemon RPC (full version with all fields)
createTestVmFull :: TestDaemon -> Text -> Int -> Int -> Maybe Text -> Bool -> Bool -> IO Int64
createTestVmFull daemon name cpus ram mDesc headless guestAgent =
  createTestVmWithOptions daemon name cpus ram mDesc headless guestAgent False

-- | Create a VM via daemon RPC with all options including cloud-init
createTestVmWithOptions :: TestDaemon -> Text -> Int -> Int -> Maybe Text -> Bool -> Bool -> Bool -> IO Int64
createTestVmWithOptions daemon name cpus ram mDesc headless guestAgent cloudInit = do
  result <- withDaemonConnection daemon $ \conn ->
    vmCreate conn name cpus ram mDesc headless guestAgent cloudInit
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error creating VM: " <> show err
    Right (Right (VmCreated vmId)) -> pure vmId
    Right (Right other) -> fail $ "Unexpected response creating VM: " <> show other

-- | Start a VM via daemon RPC (async — returns immediately)
startTestVm :: TestDaemon -> Int64 -> IO ()
startTestVm daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    vmStart conn (T.pack (show vmId)) False
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error starting VM: " <> show err
    Right (Right (VmActionSuccess _)) -> pure ()
    Right (Right other) -> fail $ "Failed to start VM: " <> show other

-- | Start a VM via daemon RPC (sync — blocks until VmRunning, including guest agent)
startTestVmSync :: TestDaemon -> Int64 -> IO ()
startTestVmSync daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    vmStart conn (T.pack (show vmId)) True
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error starting VM: " <> show err
    Right (Right (VmActionSuccess _)) -> pure ()
    Right (Right other) -> fail $ "Failed to start VM: " <> show other

-- | Stop a VM via daemon RPC
stopTestVm :: TestDaemon -> Int64 -> IO ()
stopTestVm daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    vmStop conn (T.pack (show vmId)) False
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error stopping VM: " <> show err
    Right (Right _) -> pure ()

-- | Stop a VM and wait for it to reach VmStopped status via polling.
-- Also waits for the QEMU process to fully exit so disk locks are released.
-- Fails if VM does not stop within timeout.
stopTestVmAndWait :: TestDaemon -> Int64 -> Int -> IO ()
stopTestVmAndWait daemon vmId timeoutSec = do
  stopTestVm daemon vmId
  -- Poll for status
  let go 0 = do
        -- Force stop if graceful shutdown failed
        _ <- withDaemonConnection daemon $ \conn -> vmReset conn (T.pack (show vmId))
        pure ()
      go n = do
        res <- withDaemonConnection daemon $ \conn -> showVm conn (T.pack (show vmId))
        case res of
          Right (Right (Just details)) ->
            if vdStatus details == VmStopped
              then pure ()
              else threadDelay 1000000 >> go (n - 1)
          _ -> threadDelay 1000000 >> go (n - 1)
  go timeoutSec
  -- Wait for the QEMU process to fully exit and release file locks.
  -- The daemon sets VmStopped after waitForProcess returns, but there's a
  -- brief window where the process has exited but the OS hasn't released
  -- all file locks yet. The QMP socket disappearing confirms full cleanup.
  waitForQemuExit (ssQemuConfig (tdState daemon)) vmId

-- | Delete a VM via daemon RPC (best-effort cleanup, ignores errors)
deleteTestVm :: TestDaemon -> Int64 -> IO ()
deleteTestVm daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    vmDelete conn (T.pack (show vmId))
  case result of
    Right (Right VmDeleted) -> pure ()
    _ -> pure ()

-- | Edit a VM's properties via daemon RPC
editTestVm :: TestDaemon -> Int64 -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> IO ()
editTestVm daemon vmId mCpus mRam mDesc mHeadless = do
  result <- withDaemonConnection daemon $ \conn ->
    vmEdit conn (T.pack (show vmId)) mCpus mRam mDesc mHeadless Nothing Nothing
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error editing VM: " <> show err
    Right (Right VmEdited) -> pure ()
    Right (Right other) -> fail $ "Failed to edit VM: " <> show other

--------------------------------------------------------------------------------
-- VM Configuration
--------------------------------------------------------------------------------

-- | Add a disk to a VM
addVmDisk :: TestDaemon -> Int64 -> Int64 -> DriveInterface -> CacheType -> Bool -> Bool -> IO ()
addVmDisk daemon vmId diskImageId iface cache discard ro = do
  result <- withDaemonConnection daemon $ \conn ->
    diskAttach conn (T.pack (show vmId)) (T.pack (show diskImageId)) iface Nothing ro discard cache
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error attaching disk: " <> show err
    Right (Right (DriveAttached _)) -> pure ()
    Right (Right other) -> fail $ "Failed to attach disk: " <> show other

-- | Add a network interface to a VM
addVmNetIf :: TestDaemon -> Int64 -> NetInterfaceType -> Text -> Maybe Text -> IO ()
addVmNetIf daemon vmId ifaceType hostDevice mac = do
  result <- withDaemonConnection daemon $ \conn ->
    netIfAdd conn (T.pack (show vmId)) ifaceType hostDevice mac Nothing
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error adding network interface: " <> show err
    Right (Right (NetIfAdded _)) -> pure ()
    Right (Right other) -> fail $ "Failed to add network interface: " <> show other

-- | Remove a network interface from a VM
removeVmNetIf :: TestDaemon -> Int64 -> Int64 -> IO ()
removeVmNetIf daemon vmId netIfId = do
  result <- withDaemonConnection daemon $ \conn ->
    netIfRemove conn (T.pack (show vmId)) netIfId
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error removing network interface: " <> show err
    Right (Right _) -> pure ()

-- | List network interfaces for a VM
listVmNetIfs :: TestDaemon -> Int64 -> IO [NetIfInfo]
listVmNetIfs daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    netIfList conn (T.pack (show vmId))
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error listing network interfaces: " <> show err
    Right (Right (NetIfListResult netIfs)) -> pure netIfs
    Right (Right other) -> fail $ "Unexpected response listing network interfaces: " <> show other

-- | Add a shared directory to a VM
addVmSharedDir :: TestDaemon -> Int64 -> Text -> Text -> SharedDirCache -> IO ()
addVmSharedDir daemon vmId path tag cache = do
  result <- withDaemonConnection daemon $ \conn ->
    sharedDirAdd conn (T.pack (show vmId)) path tag cache False
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error adding shared directory: " <> show err
    Right (Right (SharedDirAdded _)) -> pure ()
    Right (Right other) -> fail $ "Failed to add shared directory: " <> show other

--------------------------------------------------------------------------------
-- SSH Key Management
--------------------------------------------------------------------------------

-- | Set up SSH key for VM access.
-- Creates a new SSH key pair, registers it with the daemon, and attaches to the VM.
-- Returns the key ID and key pair paths for cleanup.
setupVmSshKey :: TestDaemon -> Int64 -> FilePath -> IO (Int64, FilePath, FilePath)
setupVmSshKey daemon vmId tmpDir = do
  -- Generate SSH key pair
  keyPairResult <- generateSshKeyPair tmpDir
  (privateKey, publicKey) <- case keyPairResult of
    Left err -> fail $ "Failed to generate SSH key pair: " <> T.unpack err
    Right keyPair -> pure (skpPrivateKey keyPair, skpPublicKey keyPair)
  putStrLn $ "[test] SSH private key: " <> privateKey

  -- Read the public key content
  pubKeyContent <- T.pack <$> readFile publicKey

  -- Create the SSH key in the daemon with a unique name
  keyUuid <- nextRandom
  let keyName = "test-key-" <> T.take 8 (toText keyUuid)
  keyId <- createSshKey daemon keyName pubKeyContent

  -- Attach the key to the VM
  attachSshKey daemon vmId keyId

  pure (keyId, privateKey, publicKey)

-- | Create an SSH key via daemon RPC
createSshKey :: TestDaemon -> Text -> Text -> IO Int64
createSshKey daemon name publicKey = do
  result <- withDaemonConnection daemon $ \conn ->
    sshKeyCreate conn name publicKey
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error creating SSH key: " <> show err
    Right (Right (SshKeyCreated keyId)) -> pure keyId
    Right (Right other) -> fail $ "Unexpected response creating SSH key: " <> show other

-- | Attach an SSH key to a VM via daemon RPC
attachSshKey :: TestDaemon -> Int64 -> Int64 -> IO ()
attachSshKey daemon vmId keyId = do
  result <- withDaemonConnection daemon $ \conn ->
    sshKeyAttach conn (T.pack (show vmId)) (T.pack (show keyId))
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error attaching SSH key: " <> show err
    Right (Right SshKeyOk) -> pure ()
    Right (Right other) -> fail $ "Unexpected response attaching SSH key: " <> show other

-- | Delete an SSH key via daemon RPC
cleanupSshKey :: TestDaemon -> Int64 -> IO ()
cleanupSshKey daemon keyId = do
  result <- withDaemonConnection daemon $ \conn ->
    sshKeyDelete conn (T.pack (show keyId))
  case result of
    Left _ -> pure () -- Ignore errors during cleanup
    Right _ -> pure ()

--------------------------------------------------------------------------------
-- Virtual Network Management
--------------------------------------------------------------------------------

-- | Create a virtual network via daemon RPC (no subnet, no DHCP)
createNetwork :: TestDaemon -> Text -> IO Int64
createNetwork daemon name = do
  result <- withDaemonConnection daemon $ \conn ->
    networkCreate conn name "" False False
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "Connection error creating network: " <> show err
    Right (Right (NetworkCreated nwId)) -> pure nwId
    Right (Right (NetworkError msg)) -> fail $ "Failed to create network: " <> T.unpack msg
    Right (Right other) -> fail $ "Unexpected response creating network: " <> show other

-- | Create a virtual network with a subnet and DHCP via daemon RPC
createNetworkWithSubnet :: TestDaemon -> Text -> Text -> IO Int64
createNetworkWithSubnet daemon name subnet = do
  result <- withDaemonConnection daemon $ \conn ->
    networkCreate conn name subnet True False
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "Connection error creating network: " <> show err
    Right (Right (NetworkCreated nwId)) -> pure nwId
    Right (Right (NetworkError msg)) -> fail $ "Failed to create network: " <> T.unpack msg
    Right (Right other) -> fail $ "Unexpected response creating network: " <> show other

-- | Create a virtual network with subnet, DHCP, and NAT via daemon RPC
createNetworkWithNat :: TestDaemon -> Text -> Text -> IO Int64
createNetworkWithNat daemon name subnet = do
  result <- withDaemonConnection daemon $ \conn ->
    networkCreate conn name subnet True True
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "Connection error creating network: " <> show err
    Right (Right (NetworkCreated nwId)) -> pure nwId
    Right (Right (NetworkError msg)) -> fail $ "Failed to create network: " <> T.unpack msg
    Right (Right other) -> fail $ "Unexpected response creating network: " <> show other

-- | Delete a virtual network via daemon RPC
deleteNetwork :: TestDaemon -> Int64 -> IO ()
deleteNetwork daemon nwId = do
  result <- withDaemonConnection daemon $ \conn ->
    networkDelete conn (T.pack (show nwId))
  case result of
    Right (Right NetworkDeleted) -> pure ()
    _ -> pure () -- Best-effort cleanup

-- | Start a virtual network via daemon RPC
startNetwork :: TestDaemon -> Int64 -> IO ()
startNetwork daemon nwId = do
  result <- withDaemonConnection daemon $ \conn ->
    networkStart conn (T.pack (show nwId))
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "Connection error starting network: " <> show err
    Right (Right NetworkStarted) -> pure ()
    Right (Right NetworkAlreadyRunning) -> pure ()
    Right (Right (NetworkError msg)) -> fail $ "Failed to start network: " <> T.unpack msg
    Right (Right other) -> fail $ "Unexpected response starting network: " <> show other

-- | Stop a virtual network via daemon RPC
stopNetwork :: TestDaemon -> Int64 -> IO ()
stopNetwork daemon nwId = do
  result <- withDaemonConnection daemon $ \conn ->
    networkStop conn (T.pack (show nwId)) True -- force stop
  case result of
    Right (Right NetworkStopped) -> pure ()
    _ -> pure () -- Best-effort cleanup

-- | Show virtual network details via daemon RPC
showNetwork :: TestDaemon -> Int64 -> IO NetworkInfo
showNetwork daemon nwId = do
  result <- withDaemonConnection daemon $ \conn ->
    networkShow conn (T.pack (show nwId))
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "Connection error showing network: " <> show err
    Right (Right (NetworkDetails info)) -> pure info
    Right (Right NetworkNotFound) -> fail $ "Network not found: " <> show nwId
    Right (Right (NetworkError msg)) -> fail $ "Failed to show network: " <> T.unpack msg
    Right (Right other) -> fail $ "Unexpected response showing network: " <> show other

-- | Add a network interface to a VM connected to a virtual network
addVmNetIfWithNetwork :: TestDaemon -> Int64 -> Int64 -> IO ()
addVmNetIfWithNetwork daemon vmId nwId = do
  result <- withDaemonConnection daemon $ \conn ->
    netIfAdd conn (T.pack (show vmId)) NetManaged "" Nothing (Just (T.pack (show nwId)))
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error adding network interface: " <> show err
    Right (Right (NetIfAdded _)) -> pure ()
    Right (Right other) -> fail $ "Failed to add network interface: " <> show other

-- | Wait for the QEMU process to fully exit by checking the QMP socket.
-- When QEMU exits, the socket file is removed by the OS.
waitForQemuExit :: QemuConfig -> Int64 -> IO ()
waitForQemuExit config vmId = do
  qmpSock <- getQmpSocket config vmId
  go qmpSock (20 :: Int) -- up to 2s
  where
    go _ 0 = pure ()
    go sock n = do
      alive <- doesFileExist sock
      when alive $ do
        threadDelay 100000 -- 100ms
        go sock (n - 1)

--------------------------------------------------------------------------------
-- Guest Execution
--------------------------------------------------------------------------------

-- | Run a command inside a test VM via the QEMU guest agent.
-- Convenience wrapper that takes a TestVm, matching the runInTestVm interface.
runInVm :: TestVm -> Text -> IO (ExitCode, Text, Text)
runInVm vm = runViaGuestAgent (tvmDaemon vm) (tvmId vm)

-- | Run a command via guest agent, failing on non-zero exit code.
-- Convenience wrapper that takes a TestVm, matching the runInTestVm_ interface.
runInVm_ :: TestVm -> Text -> IO ()
runInVm_ vm = runViaGuestAgent_ (tvmDaemon vm) (tvmId vm)

-- | Run a command inside a test VM via the QEMU guest agent.
-- Returns (ExitCode, stdout, stderr) similar to runInTestVm.
runViaGuestAgent :: TestDaemon -> Int64 -> Text -> IO (ExitCode, Text, Text)
runViaGuestAgent daemon vmId command = do
  result <- withDaemonConnection daemon $ \conn ->
    vmExec conn (T.pack (show vmId)) command
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "Connection error executing guest command: " <> show err
    Right (Right (GuestExecOk exitcode stdout stderr)) ->
      pure (if exitcode == 0 then ExitSuccess else ExitFailure exitcode, stdout, stderr)
    Right (Right GuestExecVmNotFound) -> fail "VM not found for guest-exec"
    Right (Right GuestExecNotEnabled) -> fail "Guest agent not enabled on VM"
    Right (Right (GuestExecInvalidState _ msg)) -> fail $ "Guest-exec invalid state: " <> T.unpack msg
    Right (Right (GuestExecAgentError msg)) -> fail $ "Guest agent error: " <> T.unpack msg

-- | Run a command inside a test VM via the QEMU guest agent, failing on non-zero exit code.
runViaGuestAgent_ :: TestDaemon -> Int64 -> Text -> IO ()
runViaGuestAgent_ daemon vmId command = do
  (code, _, stderr) <- runViaGuestAgent daemon vmId command
  case code of
    ExitSuccess -> pure ()
    ExitFailure c ->
      fail $ "Guest command failed with exit code " <> show c <> ": " <> T.unpack stderr

--------------------------------------------------------------------------------
-- Cloud-Init Config Management
--------------------------------------------------------------------------------

-- | Set custom cloud-init config for a VM
setCloudInitConfig :: TestDaemon -> Int64 -> Maybe Text -> Maybe Text -> Bool -> IO ()
setCloudInitConfig daemon vmId mUserData mNetworkConfig injectKeys = do
  result <- withDaemonConnection daemon $ \conn ->
    cloudInitSet conn (T.pack (show vmId)) mUserData mNetworkConfig injectKeys
  case result of
    Right (Right CloudInitOk) -> pure ()
    Right (Right (CloudInitError msg)) -> fail $ "Cloud-init config set error: " <> T.unpack msg
    Right (Right CloudInitNotFound) -> fail "VM not found for cloud-init config set"
    other -> fail $ "Failed to set cloud-init config: " <> show other

-- | Get cloud-init config for a VM
getCloudInitConfig :: TestDaemon -> Int64 -> IO CloudInitResult
getCloudInitConfig daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    cloudInitGet conn (T.pack (show vmId))
  case result of
    Right (Right r) -> pure r
    other -> fail $ "Failed to get cloud-init config: " <> show other

-- | Delete cloud-init config for a VM
deleteCloudInitConfig :: TestDaemon -> Int64 -> IO ()
deleteCloudInitConfig daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    cloudInitDelete conn (T.pack (show vmId))
  case result of
    Right (Right CloudInitOk) -> pure ()
    other -> fail $ "Failed to delete cloud-init config: " <> show other
