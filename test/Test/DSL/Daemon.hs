{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | DSL helpers for running VMs through the Corvus daemon.
-- This module provides functions to create, configure, and manage VMs
-- via RPC calls to a running daemon, with SSH access for command execution.
module Test.DSL.Daemon
  ( -- * VM Configuration
    VmConfig (..)
  , DefaultVmConfig (..)

    -- * Daemon VM lifecycle
  , DaemonVm (..)
  , withDaemonVm
  , withDaemonVmWithConfig
  , withDaemonVmConsole
  , createDaemonVm
  , startDaemonVm
  , startDaemonVmAndWait
  , stopDaemonVm
  , stopDaemonVmAndWait
  , deleteDaemonVm

    -- * VM configuration
  , addVmDisk
  , addVmNetIf
  , addVmSharedDir

    -- * SSH key management
  , setupVmSshKey
  , createSshKey
  , attachSshKey
  , cleanupSshKey

    -- * Command execution in daemon VMs
  , runInDaemonVm
  , runInDaemonVm_

    -- * Utilities
  , waitForDaemonVmSsh
  , waitForDaemonVmSshWithKey
  , generateMacAddress
  , findFreePort
  , waitForVmStopped
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Corvus.Client
import Corvus.Model
import Corvus.Protocol (VmDetails (..))
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
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
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Daemon (TestDaemon (..), withDaemonConnection)
import Test.VM.Console (SerialConsole, connectSerialConsole)
import Test.VM.Ssh (SshKeyPair (..), generateSshKeyPair)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Configuration for a test VM.
data VmConfig = VmConfig
  { vmcCpuCount :: Int
  , vmcRamMb :: Int
  , vmcOsName :: Text
  , vmcSharedDir :: Maybe FilePath
  , vmcDescription :: Maybe Text
  , vmcDiskInterface :: DriveInterface
  , vmcNetworkType :: NetInterfaceType
  , vmcWaitSshTimeout :: Int
  , vmcDiskCache :: CacheType
  , vmcDiskDiscard :: Bool
  , vmcSharedDirCache :: SharedDirCache
  , vmcSshUser :: Text
  , vmcAdditionalDisks :: [(Int64, DriveInterface, Bool)]
  , vmcHeadless :: Bool
  }
  deriving (Show, Eq)

-- | Type class for default VM configuration.
class DefaultVmConfig a where
  defaultVmConfig :: a

instance DefaultVmConfig VmConfig where
  defaultVmConfig =
    VmConfig
      { vmcCpuCount = 2
      , vmcRamMb = 2048
      , vmcOsName = "almalinux-10"
      , vmcSharedDir = Nothing
      , vmcDescription = Nothing
      , vmcDiskInterface = InterfaceVirtio
      , vmcNetworkType = NetUser
      , vmcWaitSshTimeout = 120
      , vmcDiskCache = CacheWriteback
      , vmcDiskDiscard = True
      , vmcSharedDirCache = CacheAuto
      , vmcSshUser = "corvus"
      , vmcAdditionalDisks = []
      , vmcHeadless = True
      }

-- | A VM running through the daemon with SSH access
data DaemonVm = DaemonVm
  { dvmId :: !Int64
  -- ^ VM ID in the database
  , dvmDiskId :: !Int64
  -- ^ ID of the boot disk
  , dvmSshPort :: !Int
  -- ^ SSH port
  , dvmSshHost :: !String
  -- ^ SSH host (IP address of the VM)
  , dvmDaemon :: !TestDaemon
  -- ^ Test daemon reference
  , dvmSshPrivateKey :: !FilePath
  -- ^ Path to private key file for SSH access
  , dvmSshKeyId :: !Int64
  -- ^ SSH key ID in the daemon
  , dvmSshUser :: !Text
  -- ^ SSH user name
  }

--------------------------------------------------------------------------------
-- VM Lifecycle
--------------------------------------------------------------------------------

-- | Run an action with a daemon-managed VM using a config.
-- Creates the VM with necessary configuration, sets up SSH keys,
-- starts it, runs the action, then stops and deletes the VM.
withDaemonVmWithConfig
  :: TestDaemon
  -> Int64
  -- ^ Disk image ID to use for the boot disk
  -> VmConfig
  -- ^ VM configuration
  -> (DaemonVm -> IO a)
  -> IO a
withDaemonVmWithConfig daemon diskImageId config action = do
  -- Use a unique name for the VM
  vmUuid <- nextRandom
  let vmName = "test-vm-" <> T.take 8 (toText vmUuid)

  -- Use bracket for robust VM lifecycle management
  bracket
    (createDaemonVm daemon vmName (vmcCpuCount config) (vmcRamMb config) (vmcDescription config) (vmcHeadless config))
    ( \vmId -> do
        -- Cleanup: stop and delete the VM
        stopDaemonVmAndWait daemon vmId 30
        deleteDaemonVm daemon vmId
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

      -- Add shared directory if requested
      case vmcSharedDir config of
        Nothing -> pure ()
        Just path -> addVmSharedDir daemon vmId (T.pack path) "share" (vmcSharedDirCache config)

      -- Use a temporary directory for the SSH key
      withSystemTempDirectory "corvus-test-ssh" $ \tmpDir -> do
        -- Set up SSH key for access (creates cloud-init ISO automatically)
        bracket
          (setupVmSshKey daemon vmId tmpDir)
          (\(sshKeyId, _, _) -> cleanupSshKey daemon sshKeyId)
          $ \(sshKeyId, privateKey, _publicKey) -> do
            let vm =
                  DaemonVm
                    { dvmId = vmId
                    , dvmDiskId = diskImageId
                    , dvmSshPort = sshPort
                    , dvmSshHost = "localhost"
                    , dvmDaemon = daemon
                    , dvmSshPrivateKey = privateKey
                    , dvmSshKeyId = sshKeyId
                    , dvmSshUser = vmcSshUser config
                    }

            -- Start the VM and wait for SSH
            putStrLn "[test] Starting VM and waiting for SSH..."
            vmStartTime <- getCurrentTime
            startDaemonVmAndWait vm (vmcWaitSshTimeout config)
            vmReadyTime <- getCurrentTime
            let bootSec = round (diffUTCTime vmReadyTime vmStartTime) :: Int
            putStrLn $ "[test] SSH is ready (boot to SSH: " <> show bootSec <> "s)"
            putStrLn $ "[test] ssh " <> T.unpack (dvmSshUser vm) <> "@" <> dvmSshHost vm <> " -p " <> show (dvmSshPort vm) <> " -i " <> dvmSshPrivateKey vm

            -- Run the action
            action vm

-- | Legacy wrapper for backward compatibility.
withDaemonVm
  :: TestDaemon
  -> Int64
  -- ^ Disk image ID to use for the boot disk
  -> Maybe FilePath
  -- ^ Shared directory path (optional)
  -> (DaemonVm -> IO a)
  -> IO a
withDaemonVm daemon diskImageId mSharedDir =
  let config = defaultVmConfig {vmcSharedDir = mSharedDir}
   in withDaemonVmWithConfig daemon diskImageId config

-- | Run an action with a daemon-managed VM connected via serial console.
-- Creates the VM, adds disks and network, starts it, connects to the
-- serial console immediately (no SSH setup, no waiting for SSH).
-- Use this for headless VMs that may not have SSH.
withDaemonVmConsole
  :: TestDaemon
  -> Int64
  -- ^ Disk image ID to use for the boot disk
  -> VmConfig
  -- ^ VM configuration (vmcHeadless should be True)
  -> (SerialConsole -> IO a)
  -> IO a
withDaemonVmConsole daemon diskImageId config action = do
  -- Use a unique name for the VM
  vmUuid <- nextRandom
  let vmName = "test-vm-" <> T.take 8 (toText vmUuid)

  -- Use bracket for robust VM lifecycle management
  bracket
    (createDaemonVm daemon vmName (vmcCpuCount config) (vmcRamMb config) (vmcDescription config) (vmcHeadless config))
    ( \vmId -> do
        -- Cleanup: stop and delete the VM
        stopDaemonVmAndWait daemon vmId 30
        deleteDaemonVm daemon vmId
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
      startDaemonVm daemon vmId

      -- Connect to serial console immediately
      connectSerialConsole vmId action

-- | Create a VM via daemon RPC
createDaemonVm :: TestDaemon -> Text -> Int -> Int -> Maybe Text -> Bool -> IO Int64
createDaemonVm daemon name cpus ram mDesc headless = do
  result <- withDaemonConnection daemon $ \conn ->
    vmCreate conn name cpus ram mDesc headless
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error creating VM: " <> show err
    Right (Right (VmCreated vmId)) -> pure vmId
    Right (Right other) -> fail $ "Unexpected response creating VM: " <> show other

-- | Start a VM via daemon RPC
startDaemonVm :: TestDaemon -> Int64 -> IO ()
startDaemonVm daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    vmStart conn vmId
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error starting VM: " <> show err
    Right (Right (VmActionSuccess _)) -> pure ()
    Right (Right other) -> fail $ "Failed to start VM: " <> show other

-- | Start a VM and wait for SSH connection to be established.
-- Fails if SSH is not established within timeout.
startDaemonVmAndWait :: DaemonVm -> Int -> IO ()
startDaemonVmAndWait vm timeoutSec = do
  startDaemonVm (dvmDaemon vm) (dvmId vm)
  waitForDaemonVmSshWithKey (dvmSshHost vm) (dvmSshPort vm) (dvmSshPrivateKey vm) (dvmSshUser vm) timeoutSec

-- | Stop a VM via daemon RPC
stopDaemonVm :: TestDaemon -> Int64 -> IO ()
stopDaemonVm daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    vmStop conn vmId
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error stopping VM: " <> show err
    Right (Right _) -> pure ()

-- | Stop a VM and wait for it to reach VmStopped status via polling.
-- Fails if VM does not stop within timeout.
stopDaemonVmAndWait :: TestDaemon -> Int64 -> Int -> IO ()
stopDaemonVmAndWait daemon vmId timeoutSec = do
  stopDaemonVm daemon vmId
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
deleteDaemonVm :: TestDaemon -> Int64 -> IO ()
deleteDaemonVm daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    vmDelete conn vmId
  case result of
    Right (Right VmDeleted) -> pure ()
    _ -> pure ()

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
    netIfAdd conn vmId ifaceType hostDevice mac
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
-- Command Execution
--------------------------------------------------------------------------------

-- | Run a command in the daemon VM via SSH using the configured key
runInDaemonVm :: DaemonVm -> Text -> IO (ExitCode, Text, Text)
runInDaemonVm vm cmd = do
  let args =
        [ "-o"
        , "StrictHostKeyChecking=no"
        , "-o"
        , "UserKnownHostsFile=/dev/null"
        , "-o"
        , "BatchMode=yes"
        , "-o"
        , "ConnectTimeout=10"
        , "-i"
        , dvmSshPrivateKey vm
        , "-p"
        , show (dvmSshPort vm)
        , T.unpack (dvmSshUser vm) ++ "@" ++ dvmSshHost vm
        , T.unpack cmd
        ]
  putStrLn $ "[ssh-run] Executing: " <> T.unpack cmd
  (code, stdout, stderr) <- readProcessWithExitCode "ssh" args ""
  putStrLn $ "[ssh-run] Exit code: " <> show code
  pure (code, T.pack stdout, T.pack stderr)

-- | Run a command in the daemon VM, failing on non-zero exit
runInDaemonVm_ :: DaemonVm -> Text -> IO ()
runInDaemonVm_ vm cmd = do
  (code, stdout, stderr) <- runInDaemonVm vm cmd
  case code of
    ExitSuccess -> pure ()
    ExitFailure n ->
      fail $
        "Command failed with exit code "
          <> show n
          <> "\nCommand: "
          <> T.unpack cmd
          <> "\nStdout: "
          <> T.unpack stdout
          <> "\nStderr: "
          <> T.unpack stderr

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | SSH connection attempt result
data SshProbeResult
  = -- | SSH auth succeeded
    SshOk
  | -- | SSH server is up but key auth was rejected
    SshAuthRejected !String
  | -- | SSH server not reachable yet (connection refused, timeout, etc.)
    SshNotReady

-- | Wait for SSH to be available on the VM (legacy, without key)
waitForDaemonVmSsh :: String -> Int -> Int -> IO ()
waitForDaemonVmSsh host port = waitForDaemonVmSshWithKey host port "" "corvus"

-- | Wait for SSH to be available on the VM using a specific key.
-- Uses wall-clock time for accurate timeout tracking.
-- Fails fast if SSH is up but key authentication is rejected (after a
-- grace period for cloud-init to finish deploying keys).
waitForDaemonVmSshWithKey :: String -> Int -> FilePath -> Text -> Int -> IO ()
waitForDaemonVmSshWithKey host port privateKey user timeoutSec = do
  startTime <- getCurrentTime
  go startTime Nothing
  where
    authGracePeriod :: Int
    authGracePeriod = 60

    elapsedSec :: UTCTime -> IO Int
    elapsedSec start = do
      now <- getCurrentTime
      pure $ round (diffUTCTime now start)

    go startTime mAuthStart = do
      elapsed <- elapsedSec startTime
      if elapsed >= timeoutSec
        then
          fail $
            "Timeout waiting for SSH on "
              <> host
              <> ":"
              <> show port
              <> " (after "
              <> show elapsed
              <> "s)"
        else do
          result <- trySshConnection host port privateKey
          case result of
            SshOk -> pure ()
            SshAuthRejected stderr -> do
              now <- getCurrentTime
              let authStart = fromMaybe now mAuthStart
                  authElapsed = round (diffUTCTime now authStart) :: Int
              putStrLn $
                "[ssh] Auth rejected, grace period: "
                  <> show (authGracePeriod - authElapsed)
                  <> "s remaining"
                  <> " (total elapsed: "
                  <> show elapsed
                  <> "s)"
              if authElapsed >= authGracePeriod
                then
                  fail $
                    "SSH key authentication failed on "
                      <> host
                      <> ":"
                      <> show port
                      <> " (server is up but key was rejected after "
                      <> show authGracePeriod
                      <> "s grace period).\n"
                      <> "This likely means cloud-init did not deploy the SSH key.\n"
                      <> "SSH stderr: "
                      <> stderr
                else do
                  threadDelay 2000000
                  go startTime (Just authStart)
            SshNotReady -> do
              putStrLn $ "[ssh] Not ready, waiting... (" <> show elapsed <> "s elapsed)"
              threadDelay 2000000
              go startTime Nothing

    trySshConnection :: String -> Int -> FilePath -> IO SshProbeResult
    trySshConnection sshHost sshPort keyFile = do
      let keyArgs = if null keyFile then [] else ["-i", keyFile]
          args =
            [ "-o"
            , "StrictHostKeyChecking=no"
            , "-o"
            , "UserKnownHostsFile=/dev/null"
            , "-o"
            , "BatchMode=yes"
            , "-o"
            , "ConnectTimeout=3"
            , "-p"
            , show sshPort
            ]
              ++ keyArgs
              ++ [T.unpack user ++ "@" ++ sshHost, "true"]
      result <- try $ readProcessWithExitCode "ssh" args ""
      case result of
        Left (_ :: SomeException) -> pure SshNotReady
        Right (ExitSuccess, _, _) -> pure SshOk
        Right (ExitFailure _, _, stderr)
          | "Permission denied" `isInfixOf` stderr -> pure $ SshAuthRejected stderr
          | otherwise -> pure SshNotReady

-- | Wait for VM to fully stop (dummy for now, just delays)
waitForVmStopped :: Int -> IO ()
waitForVmStopped seconds = threadDelay (seconds * 1000000)

-- | Generate a random MAC address
generateMacAddress :: IO Text
generateMacAddress = do
  uuid <- nextRandom
  let uuidText = toText uuid
      bytes = T.take 12 $ T.filter (/= '-') uuidText
      b1 = T.take 2 bytes
      b2 = T.take 2 $ T.drop 2 bytes
      b3 = T.take 2 $ T.drop 4 bytes
      b4 = T.take 2 $ T.drop 6 bytes
      b5 = T.take 2 $ T.drop 8 bytes
      b6 = T.take 2 $ T.drop 10 bytes
  -- Start with 52:54:00 (QEMU prefix) for local VM
  pure $ "52:54:00:" <> b4 <> ":" <> b5 <> ":" <> b6

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
