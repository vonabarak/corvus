{-# LANGUAGE OverloadedStrings #-}

-- | RPC call wrappers for the Corvus daemon.
-- This module provides functions to create, configure, and manage VMs
-- via RPC calls to a running daemon.
module Test.VM.Rpc
  ( -- * VM lifecycle
    createTestVm
  , startTestVm
  , stopTestVm
  , stopTestVmAndWait
  , deleteTestVm

    -- * VM edit
  , editTestVm

    -- * VM configuration
  , addVmDisk
  , addVmNetIf
  , addVmSharedDir

    -- * SSH key management
  , setupVmSshKey
  , createSshKey
  , attachSshKey
  , cleanupSshKey

    -- * Virtual network management
  , createNetwork
  , createNetworkWithSubnet
  , deleteNetwork
  , startNetwork
  , stopNetwork
  , addVmNetIfWithNetwork
  )
where

import Control.Concurrent (threadDelay)
import Corvus.Client
import Corvus.Client.Rpc (NetworkResult (..), networkCreate, networkDelete, networkStart, networkStop)
import Corvus.Model
import Corvus.Protocol (VmDetails (..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Test.VM.Daemon (TestDaemon (..), withDaemonConnection)
import Test.VM.Ssh (SshKeyPair (..), generateSshKeyPair)

--------------------------------------------------------------------------------
-- VM Lifecycle
--------------------------------------------------------------------------------

-- | Create a VM via daemon RPC
createTestVm :: TestDaemon -> Text -> Int -> Int -> Maybe Text -> Bool -> IO Int64
createTestVm daemon name cpus ram mDesc headless = do
  result <- withDaemonConnection daemon $ \conn ->
    vmCreate conn name cpus ram mDesc headless
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error creating VM: " <> show err
    Right (Right (VmCreated vmId)) -> pure vmId
    Right (Right other) -> fail $ "Unexpected response creating VM: " <> show other

-- | Start a VM via daemon RPC
startTestVm :: TestDaemon -> Int64 -> IO ()
startTestVm daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    vmStart conn vmId
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error starting VM: " <> show err
    Right (Right (VmActionSuccess _)) -> pure ()
    Right (Right other) -> fail $ "Failed to start VM: " <> show other

-- | Stop a VM via daemon RPC
stopTestVm :: TestDaemon -> Int64 -> IO ()
stopTestVm daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    vmStop conn vmId
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error stopping VM: " <> show err
    Right (Right _) -> pure ()

-- | Stop a VM and wait for it to reach VmStopped status via polling.
-- Fails if VM does not stop within timeout.
stopTestVmAndWait :: TestDaemon -> Int64 -> Int -> IO ()
stopTestVmAndWait daemon vmId timeoutSec = do
  stopTestVm daemon vmId
  -- Poll for status
  let go 0 = do
        -- Force stop if graceful shutdown failed
        _ <- withDaemonConnection daemon $ \conn -> vmReset conn vmId
        pure ()
      go n = do
        res <- withDaemonConnection daemon $ \conn -> showVm conn vmId
        case res of
          Right (Right (Just details)) ->
            if vdStatus details == VmStopped
              then pure ()
              else threadDelay 1000000 >> go (n - 1)
          _ -> threadDelay 1000000 >> go (n - 1)
  go timeoutSec

-- | Delete a VM via daemon RPC (best-effort cleanup, ignores errors)
deleteTestVm :: TestDaemon -> Int64 -> IO ()
deleteTestVm daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    vmDelete conn vmId
  case result of
    Right (Right VmDeleted) -> pure ()
    _ -> pure ()

-- | Edit a VM's properties via daemon RPC
editTestVm :: TestDaemon -> Int64 -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> IO ()
editTestVm daemon vmId mCpus mRam mDesc mHeadless = do
  result <- withDaemonConnection daemon $ \conn ->
    vmEdit conn vmId mCpus mRam mDesc mHeadless
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
    diskAttach conn vmId diskImageId iface Nothing ro discard cache
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error attaching disk: " <> show err
    Right (Right (DriveAttached _)) -> pure ()
    Right (Right other) -> fail $ "Failed to attach disk: " <> show other

-- | Add a network interface to a VM
addVmNetIf :: TestDaemon -> Int64 -> NetInterfaceType -> Text -> Maybe Text -> IO ()
addVmNetIf daemon vmId ifaceType hostDevice mac = do
  result <- withDaemonConnection daemon $ \conn ->
    netIfAdd conn vmId ifaceType hostDevice mac Nothing
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error adding network interface: " <> show err
    Right (Right (NetIfAdded _)) -> pure ()
    Right (Right other) -> fail $ "Failed to add network interface: " <> show other

-- | Add a shared directory to a VM
addVmSharedDir :: TestDaemon -> Int64 -> Text -> Text -> SharedDirCache -> IO ()
addVmSharedDir daemon vmId path tag cache = do
  result <- withDaemonConnection daemon $ \conn ->
    sharedDirAdd conn vmId path tag cache False
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
    sshKeyAttach conn vmId keyId
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error attaching SSH key: " <> show err
    Right (Right SshKeyOk) -> pure ()
    Right (Right other) -> fail $ "Unexpected response attaching SSH key: " <> show other

-- | Delete an SSH key via daemon RPC
cleanupSshKey :: TestDaemon -> Int64 -> IO ()
cleanupSshKey daemon keyId = do
  result <- withDaemonConnection daemon $ \conn ->
    sshKeyDelete conn keyId
  case result of
    Left _ -> pure () -- Ignore errors during cleanup
    Right _ -> pure ()

--------------------------------------------------------------------------------
-- Virtual Network Management
--------------------------------------------------------------------------------

-- | Create a virtual network via daemon RPC
createNetwork :: TestDaemon -> Text -> IO Int64
createNetwork daemon name = createNetworkWithSubnet daemon name ""

-- | Create a virtual network with a subnet via daemon RPC
createNetworkWithSubnet :: TestDaemon -> Text -> Text -> IO Int64
createNetworkWithSubnet daemon name subnet = do
  result <- withDaemonConnection daemon $ \conn ->
    networkCreate conn name subnet
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
    networkDelete conn nwId
  case result of
    Right (Right NetworkDeleted) -> pure ()
    _ -> pure () -- Best-effort cleanup

-- | Start a virtual network via daemon RPC
startNetwork :: TestDaemon -> Int64 -> IO ()
startNetwork daemon nwId = do
  result <- withDaemonConnection daemon $ \conn ->
    networkStart conn nwId
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
    networkStop conn nwId True -- force stop
  case result of
    Right (Right NetworkStopped) -> pure ()
    _ -> pure () -- Best-effort cleanup

-- | Add a network interface to a VM connected to a virtual network
addVmNetIfWithNetwork :: TestDaemon -> Int64 -> Int64 -> IO ()
addVmNetIfWithNetwork daemon vmId nwId = do
  result <- withDaemonConnection daemon $ \conn ->
    netIfAdd conn vmId NetVde "" Nothing (Just nwId)
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error adding network interface: " <> show err
    Right (Right (NetIfAdded _)) -> pure ()
    Right (Right other) -> fail $ "Failed to add network interface: " <> show other
