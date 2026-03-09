{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | DSL helpers for running VMs through the Corvus daemon.
-- This module provides functions to create, configure, and manage VMs
-- via RPC calls to a running daemon, with SSH access for command execution.
module Test.DSL.Daemon
  ( -- * Daemon VM lifecycle
    DaemonVm (..),
    withDaemonVm,
    createDaemonVm,
    startDaemonVm,
    stopDaemonVm,
    deleteDaemonVm,

    -- * VM configuration
    addVmDisk,
    addVmNetIf,
    addVmSharedDir,

    -- * SSH key management
    setupVmSshKey,
    createSshKey,
    attachSshKey,
    cleanupSshKey,

    -- * Command execution in daemon VMs
    runInDaemonVm,
    runInDaemonVm_,

    -- * Utilities
    waitForDaemonVmSsh,
    generateMacAddress,
    findFreePort,
  )
where

import           Control.Concurrent     (threadDelay)
import           Control.Exception      (SomeException, try)
import           Control.Monad          (unless, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Corvus.Client
import           Corvus.Model           (DriveFormat (..), DriveInterface (..),
                                         NetInterfaceType (..),
                                         SharedDirCache (..))
import           Data.Int               (Int64)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.UUID              (toText)
import           Data.UUID.V4           (nextRandom)
import           Network.Socket         (Family (..), SocketType (..), close,
                                         defaultProtocol, socket)
import qualified Network.Socket         as NS
import           System.Directory       (doesFileExist, removeFile)
import           System.Exit            (ExitCode (..))
import           System.FilePath        ((</>))
import           System.IO.Temp         (withSystemTempDirectory)
import           System.Process         (readProcessWithExitCode)
import           Test.Daemon            (TestDaemon (..), withDaemonConnection)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A VM running through the daemon with SSH access
data DaemonVm = DaemonVm
  { -- | VM ID in the database
    dvmId            :: !Int64,
    -- | SSH port
    dvmSshPort       :: !Int,
    -- | SSH host (IP address of the VM)
    dvmSshHost       :: !String,
    -- | Test daemon reference
    dvmDaemon        :: !TestDaemon,
    -- | Path to private key file for SSH access
    dvmSshPrivateKey :: !FilePath,
    -- | SSH key ID in the daemon
    dvmSshKeyId      :: !Int64
  }

--------------------------------------------------------------------------------
-- VM Lifecycle
--------------------------------------------------------------------------------

-- | Run an action with a daemon-managed VM.
-- Creates the VM with necessary configuration, sets up SSH keys,
-- starts it, runs the action, then stops and deletes the VM.
withDaemonVm ::
  TestDaemon ->
  -- | Disk image ID to use for the boot disk
  Int64 ->
  -- | Shared directory path (optional)
  Maybe FilePath ->
  (DaemonVm -> IO a) ->
  IO a
withDaemonVm daemon diskImageId mSharedDir action = do
  withSystemTempDirectory "corvus-test-ssh" $ \tmpDir -> do
    -- Find a free port for SSH forwarding
    sshPort <- findFreePort

    -- Create the VM with a unique name
    vmUuid <- nextRandom
    let vmName = "test-vm-" <> T.take 8 (toText vmUuid)
    vmId <- createDaemonVm daemon vmName 2 2048

    -- Add boot disk
    addVmDisk daemon vmId diskImageId

    -- Add user-mode network interface with SSH port forwarding
    mac <- generateMacAddress
    let hostFwd = "hostfwd=tcp::" <> T.pack (show sshPort) <> "-:22"
    addVmNetIf daemon vmId NetUser hostFwd mac

    -- Add shared directory if requested
    case mSharedDir of
      Nothing   -> pure ()
      Just path -> addVmSharedDir daemon vmId (T.pack path) "share"

    -- Set up SSH key for access (creates cloud-init ISO automatically)
    (sshKeyId, privateKey, _publicKey) <- setupVmSshKey daemon vmId tmpDir
    putStrLn $ "SSH private key: " <> privateKey

    -- Start the VM
    putStrLn "[test] Starting VM..."
    startDaemonVm daemon vmId
    putStrLn "[test] VM started, waiting for SSH..."

    let sshHost = "localhost"

    -- Wait for SSH to be available (with the configured key)
    waitForDaemonVmSshWithKey sshHost sshPort privateKey 80
    putStrLn "[test] SSH is ready"

    let vm =
          DaemonVm
            { dvmId = vmId,
              dvmSshPort = sshPort,
              dvmSshHost = sshHost,
              dvmDaemon = daemon,
              dvmSshPrivateKey = privateKey,
              dvmSshKeyId = sshKeyId
            }

    -- Run the action and clean up
    result <- action vm

    -- Stop and delete the VM
    stopDaemonVm daemon vmId
    deleteDaemonVm daemon vmId

    -- Clean up SSH key from daemon
    cleanupSshKey daemon sshKeyId

    pure result

-- | Create a VM via daemon RPC
createDaemonVm :: TestDaemon -> Text -> Int -> Int -> IO Int64
createDaemonVm daemon name cpus ram = do
  result <- withDaemonConnection daemon $ \conn ->
    vmCreate conn name cpus ram Nothing
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

-- | Stop a VM via daemon RPC
stopDaemonVm :: TestDaemon -> Int64 -> IO ()
stopDaemonVm daemon vmId = do
  -- First try graceful shutdown
  result <- withDaemonConnection daemon $ \conn ->
    vmStop conn vmId
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error stopping VM: " <> show err
    Right (Right _) -> do
      -- Wait a bit for shutdown, then reset to ensure it's stopped
      threadDelay 2000000 -- 2 seconds
      _ <- withDaemonConnection daemon $ \conn -> vmReset conn vmId
      pure ()

-- | Delete a VM via daemon RPC (best-effort cleanup, ignores errors)
deleteDaemonVm :: TestDaemon -> Int64 -> IO ()
deleteDaemonVm daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    vmDelete conn vmId
  case result of
    Right (Right VmDeleted) -> pure ()
    _                       -> pure ()

--------------------------------------------------------------------------------
-- VM Configuration
--------------------------------------------------------------------------------

-- | Add a disk to a VM
addVmDisk :: TestDaemon -> Int64 -> Int64 -> IO ()
addVmDisk daemon vmId diskImageId = do
  result <- withDaemonConnection daemon $ \conn ->
    diskAttach conn vmId diskImageId InterfaceVirtio Nothing
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error attaching disk: " <> show err
    Right (Right (DriveAttached _)) -> pure ()
    Right (Right other) -> fail $ "Failed to attach disk: " <> show other

-- | Add a network interface to a VM
addVmNetIf :: TestDaemon -> Int64 -> NetInterfaceType -> Text -> Text -> IO ()
addVmNetIf daemon vmId ifaceType hostDevice mac = do
  result <- withDaemonConnection daemon $ \conn ->
    netIfAdd conn vmId ifaceType hostDevice mac
  case result of
    Left err -> fail $ "Failed to connect to daemon: " <> show err
    Right (Left err) -> fail $ "RPC error adding network interface: " <> show err
    Right (Right (NetIfAdded _)) -> pure ()
    Right (Right other) -> fail $ "Failed to add network interface: " <> show other

-- | Add a shared directory to a VM
addVmSharedDir :: TestDaemon -> Int64 -> Text -> Text -> IO ()
addVmSharedDir daemon vmId path tag = do
  result <- withDaemonConnection daemon $ \conn ->
    sharedDirAdd conn vmId path tag CacheAuto False
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
    Left err      -> fail $ "Failed to generate SSH key pair: " <> T.unpack err
    Right keyPair -> pure (skpPrivateKey keyPair, skpPublicKey keyPair)

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
    Left _  -> pure () -- Ignore errors during cleanup
    Right _ -> pure ()

-- | Generate SSH key pair (local helper)
generateSshKeyPair :: FilePath -> IO (Either Text SshKeyPair)
generateSshKeyPair tmpDir = do
  let privateKey = tmpDir </> "test_vm_key"
      publicKey = privateKey ++ ".pub"

  -- Remove existing keys if present
  privExists <- doesFileExist privateKey
  when privExists $ removeFile privateKey
  pubExists <- doesFileExist publicKey
  when pubExists $ removeFile publicKey

  -- Generate new key pair
  (code, _, stderr) <-
    readProcessWithExitCode
      "ssh-keygen"
      [ "-t",
        "ed25519",
        "-f",
        privateKey,
        "-N",
        "",
        "-C",
        "corvus-test@localhost"
      ]
      ""

  case code of
    ExitSuccess ->
      pure $
        Right
          SshKeyPair
            { skpPrivateKey = privateKey,
              skpPublicKey = publicKey
            }
    ExitFailure n ->
      pure $
        Left $
          "Failed to generate SSH key (exit " <> T.pack (show n) <> "): " <> T.pack stderr

-- | SSH key pair data
data SshKeyPair = SshKeyPair
  { skpPrivateKey :: !FilePath,
    skpPublicKey  :: !FilePath
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Command Execution
--------------------------------------------------------------------------------

-- | Run a command in the daemon VM via SSH using the configured key
runInDaemonVm :: DaemonVm -> Text -> IO (ExitCode, Text, Text)
runInDaemonVm vm cmd = do
  let args =
        [ "-o",
          "StrictHostKeyChecking=no",
          "-o",
          "UserKnownHostsFile=/dev/null",
          "-o",
          "BatchMode=yes",
          "-o",
          "ConnectTimeout=10",
          "-i",
          dvmSshPrivateKey vm,
          "-p",
          show (dvmSshPort vm),
          "corvus@" ++ dvmSshHost vm,
          T.unpack cmd
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
waitForDaemonVmSsh host port timeoutSec = waitForDaemonVmSshWithKey host port "" timeoutSec

-- | Wait for SSH to be available on the VM using a specific key.
-- Fails fast if SSH is up but key authentication is rejected (after a
-- grace period for cloud-init to finish deploying keys).
waitForDaemonVmSshWithKey :: String -> Int -> FilePath -> Int -> IO ()
waitForDaemonVmSshWithKey host port privateKey timeoutSec = go timeoutSec Nothing
  where
    authGracePeriod :: Int
    authGracePeriod = 30

    go 0 _ = fail $ "Timeout waiting for SSH on " <> host <> ":" <> show port
    go n mAuthCountdown = do
      result <- trySshConnection host port privateKey
      case result of
        SshOk -> pure ()
        SshAuthRejected stderr -> do
          let countdown = case mAuthCountdown of
                Just c  -> c - 1
                Nothing -> authGracePeriod
          putStrLn $ "[ssh] Auth rejected, grace period: " <> show countdown <> "s remaining"
          if countdown <= 0
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
              threadDelay 1000000
              go (n - 1) (Just countdown)
        SshNotReady -> do
          let elapsed = timeoutSec - n
          putStrLn $ "[ssh] Not ready, waiting... (" <> show elapsed <> "s elapsed)"
          threadDelay 1000000
          go (n - 1) Nothing

    trySshConnection :: String -> Int -> FilePath -> IO SshProbeResult
    trySshConnection sshHost sshPort keyFile = do
      let keyArgs = if null keyFile then [] else ["-i", keyFile]
          user = if null keyFile then "almalinux" else "corvus"
          args =
            [ "-o",
              "StrictHostKeyChecking=no",
              "-o",
              "UserKnownHostsFile=/dev/null",
              "-o",
              "BatchMode=yes",
              "-o",
              "ConnectTimeout=5",
              "-p",
              show sshPort
            ]
              ++ keyArgs
              ++ [user ++ "@" ++ sshHost, "true"]
      result <- try $ readProcessWithExitCode "ssh" args ""
      case result of
        Left (_ :: SomeException) -> pure SshNotReady
        Right (ExitSuccess, _, _) -> pure SshOk
        Right (ExitFailure _, _, stderr)
          | "Permission denied" `isInfixOf` stderr -> pure $ SshAuthRejected stderr
          | otherwise -> pure SshNotReady

    isInfixOf :: String -> String -> Bool
    isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _              = True
    isPrefixOf _ []              = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

    tails :: String -> [String]
    tails []         = [[]]
    tails s@(_ : xs) = s : tails xs

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
    _                      -> pure 2222 -- Fallback
