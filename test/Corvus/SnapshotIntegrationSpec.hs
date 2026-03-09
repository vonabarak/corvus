{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration tests for snapshot operations via the Corvus daemon.
-- These tests verify that snapshots correctly capture and restore disk state.
--
-- Requirements:
--   - QEMU with KVM support
--   - genisoimage or mkisofs
--   - SSH client
--   - PostgreSQL for test database
--   - Debian cloud image (downloaded automatically on first run)
--
-- Run with: cabal test --test-arguments="--match SnapshotIntegration"
module Corvus.SnapshotIntegrationSpec (spec) where

import Control.Concurrent (threadDelay)

import Control.Exception (bracket)
import Control.Monad (when)
import Corvus.Client
import Corvus.Model (DriveFormat (..), DriveInterface (..), NetInterfaceType (NetUser))
import Corvus.Protocol (SnapshotInfo (..))
import Data.Int (Int64)
import Data.List (sort)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (getCanonicalTemporaryDirectory, withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Daemon (TestDaemon (..), startTestDaemon, stopTestDaemon, withDaemonConnection)
import Test.Database (TestEnv, withTestDb)
import Test.DSL.Daemon (findFreePort, generateMacAddress)
import Test.Hspec
import Test.VM.Image (createOverlay, defaultImageConfig, ensureBaseImage, removeOverlay)

spec :: Spec
spec = withTestDb $ do
  describe "Snapshot integration through daemon" $ do
    -- These tests require a fully functioning cloud-init setup.
    -- They have been verified to work manually but are flaky in CI due to
    -- timing issues with cloud-init user creation.
    it "can rollback to restore deleted file" $ \env -> do
      testSnapshotRollback env

    it "can create multiple snapshots and rollback to specific one" $ \env -> do
      testMultipleSnapshots env

    it "snapshot list shows created snapshots" $ \env -> do
      testSnapshotList env

    it "can merge snapshot to consolidate changes" $ \env -> do
      testSnapshotMerge env

    it "allows duplicate snapshot names" $ \env -> do
      testSnapshotNameDuplicates env

    it "rejects snapshot operations on running VM" $ \env -> do
      testSnapshotOnRunningVmRejected env

    it "handles concurrent snapshot operations" $ \env -> do
      testConcurrentSnapshotOperations env

-- | Test that snapshot rollback restores a deleted file
testSnapshotRollback :: TestEnv -> IO ()
testSnapshotRollback env = do
  daemon <- startTestDaemon env

  withSystemTempDirectory "snapshot-test-ssh" $ \tmpDir -> do
    ctx <- setupTestVm daemon tmpDir

    uuid <- nextRandom
    let testContent = "SNAPSHOT-TEST:" <> T.unpack (toText uuid)
        testFile = "/home/corvus/testfile.txt"

    startVm daemon (tcVmId ctx)
    waitForSshWithKey ctx 120

    (code1, _, _) <- runSshCommandWithKey ctx $ "echo '" <> testContent <> "' > " <> testFile
    code1 `shouldBe` ExitSuccess

    (code2, stdout2, _) <- runSshCommandWithKey ctx $ "cat " <> testFile
    code2 `shouldBe` ExitSuccess
    T.strip (T.pack stdout2) `shouldBe` T.pack testContent

    stopVm daemon (tcVmId ctx)
    waitForVmStopped 10

    snapshotId <- createSnapshot daemon (tcDiskId ctx) "before-delete"

    startVm daemon (tcVmId ctx)
    waitForSshWithKey ctx 120

    _ <- runSshCommandWithKey ctx $ "rm -f " <> testFile

    (code3, _, _) <- runSshCommandWithKey ctx $ "test -f " <> testFile
    code3 `shouldNotBe` ExitSuccess

    stopVm daemon (tcVmId ctx)
    waitForVmStopped 10

    rollbackSnapshot daemon (tcDiskId ctx) snapshotId

    startVm daemon (tcVmId ctx)
    waitForSshWithKey ctx 120

    (code4, stdout4, _) <- runSshCommandWithKey ctx $ "cat " <> testFile
    code4 `shouldBe` ExitSuccess
    T.strip (T.pack stdout4) `shouldBe` T.pack testContent

    stopVm daemon (tcVmId ctx)
    waitForVmStopped 10

    deleteSnapshot daemon (tcDiskId ctx) snapshotId
    cleanupTestContext daemon ctx

  stopTestDaemon daemon

-- | Test creating multiple snapshots and rolling back to a specific one
testMultipleSnapshots :: TestEnv -> IO ()
testMultipleSnapshots env = do
  daemon <- startTestDaemon env

  withSystemTempDirectory "snapshot-test-ssh" $ \tmpDir -> do
    ctx <- setupTestVm daemon tmpDir
    let testFile = "/home/corvus/counter.txt"

    startVm daemon (tcVmId ctx)
    waitForSshWithKey ctx 120
    _ <- runSshCommandWithKey ctx "echo '1' > /home/corvus/counter.txt"
    stopVm daemon (tcVmId ctx)
    waitForVmStopped 10

    snap1 <- createSnapshot daemon (tcDiskId ctx) "state-1"

    startVm daemon (tcVmId ctx)
    waitForSshWithKey ctx 120
    _ <- runSshCommandWithKey ctx "echo '2' > /home/corvus/counter.txt"
    stopVm daemon (tcVmId ctx)
    waitForVmStopped 10

    snap2 <- createSnapshot daemon (tcDiskId ctx) "state-2"

    startVm daemon (tcVmId ctx)
    waitForSshWithKey ctx 120
    _ <- runSshCommandWithKey ctx "echo '3' > /home/corvus/counter.txt"
    (_, stdout3, _) <- runSshCommandWithKey ctx $ "cat " <> testFile
    T.strip (T.pack stdout3) `shouldBe` "3"
    stopVm daemon (tcVmId ctx)
    waitForVmStopped 10

    rollbackSnapshot daemon (tcDiskId ctx) snap1

    startVm daemon (tcVmId ctx)
    waitForSshWithKey ctx 120
    (_, stdout1, _) <- runSshCommandWithKey ctx $ "cat " <> testFile
    T.strip (T.pack stdout1) `shouldBe` "1"
    stopVm daemon (tcVmId ctx)
    waitForVmStopped 10

    deleteSnapshot daemon (tcDiskId ctx) snap2
    deleteSnapshot daemon (tcDiskId ctx) snap1

    cleanupTestContext daemon ctx

  stopTestDaemon daemon

-- | Test that snapshot list correctly shows created snapshots
testSnapshotList :: TestEnv -> IO ()
testSnapshotList env = do
  daemon <- startTestDaemon env

  withSystemTempDirectory "snapshot-test-ssh" $ \tmpDir -> do
    ctx <- setupTestVm daemon tmpDir

    snap1 <- createSnapshot daemon (tcDiskId ctx) "snapshot-alpha"
    snap2 <- createSnapshot daemon (tcDiskId ctx) "snapshot-beta"

    snapshots <- listSnapshots daemon (tcDiskId ctx)
    length snapshots `shouldSatisfy` (>= 2)

    let names = map sniName snapshots
    names `shouldSatisfy` elem "snapshot-alpha"
    names `shouldSatisfy` elem "snapshot-beta"

    deleteSnapshot daemon (tcDiskId ctx) snap2
    deleteSnapshot daemon (tcDiskId ctx) snap1
    cleanupTestContext daemon ctx

  stopTestDaemon daemon

-- | Test that merging a snapshot consolidates changes and removes it
testSnapshotMerge :: TestEnv -> IO ()
testSnapshotMerge env = do
  daemon <- startTestDaemon env

  withSystemTempDirectory "snapshot-test-ssh" $ \tmpDir -> do
    ctx <- setupTestVm daemon tmpDir
    let testFile = "/home/corvus/merge-test.txt"

    startVm daemon (tcVmId ctx)
    waitForSshWithKey ctx 120
    _ <- runSshCommandWithKey ctx "echo 'initial' > /home/corvus/merge-test.txt"
    stopVm daemon (tcVmId ctx)
    waitForVmStopped 10

    snap1 <- createSnapshot daemon (tcDiskId ctx) "to-merge"

    startVm daemon (tcVmId ctx)
    waitForSshWithKey ctx 120
    _ <- runSshCommandWithKey ctx "echo 'modified' > /home/corvus/merge-test.txt"
    stopVm daemon (tcVmId ctx)
    waitForVmStopped 10

    snapshotsBefore <- listSnapshots daemon (tcDiskId ctx)
    let countBefore = length snapshotsBefore

    mergeSnapshot daemon (tcDiskId ctx) snap1

    snapshotsAfter <- listSnapshots daemon (tcDiskId ctx)
    let countAfter = length snapshotsAfter
    countAfter `shouldBe` (countBefore - 1)

    let snapIds = map sniId snapshotsAfter
    snapIds `shouldNotSatisfy` elem snap1

    startVm daemon (tcVmId ctx)
    waitForSshWithKey ctx 120
    (code, stdout, _) <- runSshCommandWithKey ctx $ "cat " <> testFile
    code `shouldBe` ExitSuccess
    T.strip (T.pack stdout) `shouldBe` "modified"
    stopVm daemon (tcVmId ctx)
    waitForVmStopped 5

    cleanupTestContext daemon ctx

  stopTestDaemon daemon

-- | Test that snapshots with duplicate names are allowed (each gets unique ID)
testSnapshotNameDuplicates :: TestEnv -> IO ()
testSnapshotNameDuplicates env = do
  daemon <- startTestDaemon env

  withSystemTempDirectory "snapshot-test-ssh" $ \tmpDir -> do
    ctx <- setupTestVm daemon tmpDir

    snap1 <- createSnapshot daemon (tcDiskId ctx) "same-name"
    snap2 <- createSnapshot daemon (tcDiskId ctx) "same-name"
    snap3 <- createSnapshot daemon (tcDiskId ctx) "same-name"

    snap1 `shouldNotBe` snap2
    snap2 `shouldNotBe` snap3
    snap1 `shouldNotBe` snap3

    snapshots <- listSnapshots daemon (tcDiskId ctx)
    let sameNameSnaps = filter (\s -> sniName s == "same-name") snapshots
    length sameNameSnaps `shouldBe` 3

    let ids = sort $ map sniId sameNameSnaps
    ids `shouldBe` sort [snap1, snap2, snap3]

    deleteSnapshot daemon (tcDiskId ctx) snap3
    deleteSnapshot daemon (tcDiskId ctx) snap2
    deleteSnapshot daemon (tcDiskId ctx) snap1
    cleanupTestContext daemon ctx

  stopTestDaemon daemon

-- | Test that snapshot operations on running VMs are rejected
testSnapshotOnRunningVmRejected :: TestEnv -> IO ()
testSnapshotOnRunningVmRejected env = do
  daemon <- startTestDaemon env

  withSystemTempDirectory "snapshot-test-ssh" $ \tmpDir -> do
    ctx <- setupTestVm daemon tmpDir

    snap1 <- createSnapshot daemon (tcDiskId ctx) "test-snap"

    startVm daemon (tcVmId ctx)
    waitForSshWithKey ctx 120

    createResult <- tryCreateSnapshot daemon (tcDiskId ctx) "should-fail"
    createResult `shouldBe` SnapshotVmMustBeStopped

    rollbackResult <- tryRollbackSnapshot daemon (tcDiskId ctx) snap1
    rollbackResult `shouldBe` SnapshotVmMustBeStopped

    mergeResult <- tryMergeSnapshot daemon (tcDiskId ctx) snap1
    mergeResult `shouldBe` SnapshotVmMustBeStopped

    stopVm daemon (tcVmId ctx)
    waitForVmStopped 10

    snap2 <- createSnapshot daemon (tcDiskId ctx) "after-stop"
    snap2 `shouldSatisfy` (> 0)

    deleteSnapshot daemon (tcDiskId ctx) snap2
    deleteSnapshot daemon (tcDiskId ctx) snap1
    cleanupTestContext daemon ctx

  stopTestDaemon daemon

-- | Test that multiple rapid snapshot operations are handled correctly
testConcurrentSnapshotOperations :: TestEnv -> IO ()
testConcurrentSnapshotOperations env = do
  daemon <- startTestDaemon env

  withSystemTempDirectory "snapshot-test-ssh" $ \tmpDir -> do
    ctx <- setupTestVm daemon tmpDir

    snap1 <- createSnapshot daemon (tcDiskId ctx) "concurrent-1"
    snap2 <- createSnapshot daemon (tcDiskId ctx) "concurrent-2"
    snap3 <- createSnapshot daemon (tcDiskId ctx) "concurrent-3"

    let ids = [snap1, snap2, snap3]
    length ids `shouldBe` 3
    length (filter (> 0) ids) `shouldBe` 3

    snapshots <- listSnapshots daemon (tcDiskId ctx)
    let concurrentSnaps = filter (\s -> "concurrent-" `T.isPrefixOf` sniName s) snapshots
    length concurrentSnaps `shouldBe` 3

    deleteSnapshot daemon (tcDiskId ctx) snap3
    deleteSnapshot daemon (tcDiskId ctx) snap2
    deleteSnapshot daemon (tcDiskId ctx) snap1
    cleanupTestContext daemon ctx

  stopTestDaemon daemon

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Test context with resources that need cleanup
data TestContext = TestContext
  { tcDiskId :: !Int64,
    tcVmId :: !Int64,
    tcSshPort :: !Int,
    tcOverlayPath :: !FilePath,
    tcSshKeyId :: !Int64,
    tcPrivateKeyPath :: !FilePath
  }

-- | Set up a test VM with disk and network, returning test context
-- The caller must call cleanupTestContext when done
setupTestVm :: TestDaemon -> FilePath -> IO TestContext
setupTestVm daemon tmpDir = do
  -- Ensure base image exists
  imageResult <- ensureBaseImage defaultImageConfig
  basePath <- case imageResult of
    Left err -> fail $ "Failed to get base image: " <> T.unpack err
    Right path -> pure path

  -- Create unique overlay for this test
  uuid <- nextRandom
  sysTmp <- getCanonicalTemporaryDirectory
  let overlayPath = sysTmp </> ("snapshot-test-" <> T.unpack (T.take 8 (toText uuid)) <> ".qcow2")

  overlayResult <- createOverlay basePath overlayPath
  case overlayResult of
    Left err -> fail $ "Failed to create overlay: " <> T.unpack err
    Right _ -> pure ()

  -- Register the overlay as a disk
  diskId <- registerDisk daemon overlayPath

  vmId <- createVm daemon

  attachDisk daemon vmId diskId

  sshPort <- findFreePort
  mac <- generateMacAddress
  let hostFwd = "hostfwd=tcp::" <> T.pack (show sshPort) <> "-:22"
  addNetworkInterface daemon vmId hostFwd mac

  -- Set up SSH key for cloud-init
  (sshKeyId, privateKeyPath) <- setupSshKey daemon vmId tmpDir

  pure TestContext
    { tcDiskId = diskId,
      tcVmId = vmId,
      tcSshPort = sshPort,
      tcOverlayPath = overlayPath,
      tcSshKeyId = sshKeyId,
      tcPrivateKeyPath = privateKeyPath
    }

-- | Set up SSH key for VM access via cloud-init
setupSshKey :: TestDaemon -> Int64 -> FilePath -> IO (Int64, FilePath)
setupSshKey daemon vmId tmpDir = do
  -- Generate SSH key pair
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
      ["-t", "ed25519", "-f", privateKey, "-N", "", "-C", "corvus-test@localhost"]
      ""

  case code of
    ExitFailure n -> fail $ "Failed to generate SSH key: " ++ stderr
    ExitSuccess -> pure ()

  -- Read public key content
  pubKeyContent <- T.pack <$> readFile publicKey

  -- Create SSH key in daemon with a unique name
  keyUuid <- nextRandom
  let keyName = "test-key-" <> T.take 8 (toText keyUuid)
  keyResult <- withDaemonConnection daemon $ \conn ->
    sshKeyCreate conn keyName pubKeyContent
  keyId <- case keyResult of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error creating SSH key: " <> show err
    Right (Right (SshKeyCreated kId)) -> pure kId
    Right (Right other) -> fail $ "Unexpected response: " <> show other

  -- Attach key to VM (triggers cloud-init ISO generation)
  attachResult <- withDaemonConnection daemon $ \conn ->
    sshKeyAttach conn vmId keyId
  case attachResult of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error attaching SSH key: " <> show err
    Right (Right SshKeyOk) -> pure ()
    Right (Right other) -> fail $ "Unexpected response: " <> show other

  pure (keyId, privateKey)

-- | Register a disk image with the daemon
registerDisk :: TestDaemon -> FilePath -> IO Int64
registerDisk daemon imagePath = do
  uuid <- nextRandom
  let diskName = "test-disk-" <> T.take 8 (toText uuid)
  result <- withDaemonConnection daemon $ \conn ->
    diskRegister conn diskName (T.pack imagePath) FormatQcow2 Nothing
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right (DiskCreated dId)) -> pure dId
    Right (Right other) -> fail $ "Unexpected response: " <> show other

-- | Create a VM via daemon
createVm :: TestDaemon -> IO Int64
createVm daemon = do
  uuid <- nextRandom
  let name = "snapshot-test-" <> T.take 8 (toText uuid)
  result <- withDaemonConnection daemon $ \conn ->
    vmCreate conn name 2 2048 Nothing
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right (VmCreated vId)) -> pure vId
    Right (Right other) -> fail $ "Unexpected response: " <> show other

-- | Attach a disk to a VM
attachDisk :: TestDaemon -> Int64 -> Int64 -> IO ()
attachDisk daemon vmId diskId = do
  result <- withDaemonConnection daemon $ \conn ->
    diskAttach conn vmId diskId InterfaceVirtio Nothing
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right (DriveAttached _)) -> pure ()
    Right (Right other) -> fail $ "Unexpected response: " <> show other

-- | Add a network interface to a VM
addNetworkInterface :: TestDaemon -> Int64 -> T.Text -> T.Text -> IO ()
addNetworkInterface daemon vmId hostDevice mac = do
  result <- withDaemonConnection daemon $ \conn ->
    netIfAdd conn vmId NetUser hostDevice mac
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right (NetIfAdded _)) -> pure ()
    Right (Right other) -> fail $ "Unexpected response: " <> show other

-- | Start a VM
startVm :: TestDaemon -> Int64 -> IO ()
startVm daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    vmStart conn vmId
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right (VmActionSuccess _)) -> pure ()
    Right (Right other) -> fail $ "Unexpected response: " <> show other

-- | Stop a VM
stopVm :: TestDaemon -> Int64 -> IO ()
stopVm daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    vmStop conn vmId
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right _) -> pure ()

-- | Wait for VM to fully stop
waitForVmStopped :: Int -> IO ()
waitForVmStopped seconds = threadDelay (seconds * 1000000)

-- | Create a snapshot
createSnapshot :: TestDaemon -> Int64 -> T.Text -> IO Int64
createSnapshot daemon diskId name = do
  result <- withDaemonConnection daemon $ \conn ->
    snapshotCreate conn diskId name
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right (SnapshotCreated sId)) -> pure sId
    Right (Right other) -> fail $ "Unexpected response: " <> show other

-- | Rollback to a snapshot
rollbackSnapshot :: TestDaemon -> Int64 -> Int64 -> IO ()
rollbackSnapshot daemon diskId snapshotId = do
  result <- withDaemonConnection daemon $ \conn ->
    snapshotRollback conn diskId snapshotId
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right SnapshotOk) -> pure ()
    Right (Right other) -> fail $ "Unexpected response: " <> show other

-- | Delete a snapshot
deleteSnapshot :: TestDaemon -> Int64 -> Int64 -> IO ()
deleteSnapshot daemon diskId snapshotId = do
  result <- withDaemonConnection daemon $ \conn ->
    snapshotDelete conn diskId snapshotId
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right SnapshotOk) -> pure ()
    Right (Right SnapshotNotFound) -> pure ()
    Right (Right other) -> fail $ "Unexpected response: " <> show other

-- | Merge a snapshot
mergeSnapshot :: TestDaemon -> Int64 -> Int64 -> IO ()
mergeSnapshot daemon diskId snapshotId = do
  result <- withDaemonConnection daemon $ \conn ->
    snapshotMerge conn diskId snapshotId
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right SnapshotOk) -> pure ()
    Right (Right other) -> fail $ "Unexpected response: " <> show other

-- | List snapshots for a disk
listSnapshots :: TestDaemon -> Int64 -> IO [SnapshotInfo]
listSnapshots daemon diskId = do
  result <- withDaemonConnection daemon $ \conn ->
    snapshotList conn diskId
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right (SnapshotListResult snaps)) -> pure snaps
    Right (Right other) -> fail $ "Unexpected response: " <> show other

-- | Try to create a snapshot, returning the result type for error checking
tryCreateSnapshot :: TestDaemon -> Int64 -> T.Text -> IO SnapshotResult
tryCreateSnapshot daemon diskId name = do
  result <- withDaemonConnection daemon $ \conn ->
    snapshotCreate conn diskId name
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right res) -> pure res

-- | Try to rollback a snapshot, returning the result type for error checking
tryRollbackSnapshot :: TestDaemon -> Int64 -> Int64 -> IO SnapshotResult
tryRollbackSnapshot daemon diskId snapshotId = do
  result <- withDaemonConnection daemon $ \conn ->
    snapshotRollback conn diskId snapshotId
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right res) -> pure res

-- | Try to merge a snapshot, returning the result type for error checking
tryMergeSnapshot :: TestDaemon -> Int64 -> Int64 -> IO SnapshotResult
tryMergeSnapshot daemon diskId snapshotId = do
  result <- withDaemonConnection daemon $ \conn ->
    snapshotMerge conn diskId snapshotId
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right res) -> pure res

-- | Run an SSH command on the VM using the test context
runSshCommandWithKey :: TestContext -> String -> IO (ExitCode, String, String)
runSshCommandWithKey ctx cmd = do
  let args =
        [ "-o", "StrictHostKeyChecking=no",
          "-o", "UserKnownHostsFile=/dev/null",
          "-o", "BatchMode=yes",
          "-o", "ConnectTimeout=10",
          "-i", tcPrivateKeyPath ctx,
          "-p", show (tcSshPort ctx),
          "corvus@localhost",
          cmd
        ]
  readProcessWithExitCode "ssh" args ""

-- | Wait for SSH to become available using the test context
waitForSshWithKey :: TestContext -> Int -> IO ()
waitForSshWithKey ctx timeoutSec = go timeoutSec
  where
    go 0 = fail $ "Timeout waiting for SSH on port " <> show (tcSshPort ctx)
    go n = do
      (code, _, _) <- runSshCommandWithKey ctx "true"
      case code of
        ExitSuccess -> pure ()
        _ -> do
          threadDelay 1000000
          go (n - 1)

-- | Clean up test resources
cleanupTestContext :: TestDaemon -> TestContext -> IO ()
cleanupTestContext daemon ctx = do
  -- Delete VM
  result <- withDaemonConnection daemon $ \conn ->
    vmDelete conn (tcVmId ctx)
  case result of
    Right (Right VmDeleted) -> pure ()
    Right (Right VmDeleteRunning) -> do
      _ <- withDaemonConnection daemon $ \conn -> vmReset conn (tcVmId ctx)
      threadDelay 2000000
      _ <- withDaemonConnection daemon $ \conn -> vmDelete conn (tcVmId ctx)
      pure ()
    _ -> pure ()

  -- Delete SSH key
  _ <- withDaemonConnection daemon $ \conn ->
    sshKeyDelete conn (tcSshKeyId ctx)

  -- Remove overlay file
  removeOverlay (tcOverlayPath ctx)
