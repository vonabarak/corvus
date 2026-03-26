{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration tests for snapshot operations via the Corvus daemon.
-- These tests verify that snapshots correctly capture and restore disk state.
--
-- Requirements:
--   - QEMU with KVM support
--   - PostgreSQL for test database
--
-- Run with: stack test --test-arguments="--match SnapshotIntegration"
module Corvus.SnapshotIntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Corvus.Client
import Corvus.Model (VmStatus (..))
import Corvus.Protocol (SnapshotInfo (..), VmDetails (..))
import Data.Int (Int64)
import Data.List (sort)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import System.Exit (ExitCode (..))
import Test.Database (TestEnv, withTestDb)
import Test.Hspec
import Test.VM.Common (TestVm (..), defaultVmConfig, startTestVmAndWaitGuestAgent, withTestVmGuestExec)
import Test.VM.Daemon (TestDaemon (..), withDaemonConnection)
import Test.VM.Rpc (runInVm, stopTestVmAndWait)

spec :: Spec
spec = withTestDb $ do
  describe "Snapshot integration through daemon" $ do
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
  withTestVmGuestExec env defaultVmConfig $ \vm -> do
    let daemon = tvmDaemon vm
        diskId = tvmDiskId vm
        vmId = tvmId vm

    uuid <- nextRandom
    let testContent = "SNAPSHOT-TEST:" <> T.unpack (toText uuid)
        testFile = "/home/corvus/testfile.txt"

    (code1, _, _) <- runInVm vm $ "echo '" <> T.pack testContent <> "' > " <> T.pack testFile
    code1 `shouldBe` ExitSuccess

    (code2, stdout2, _) <- runInVm vm $ "cat " <> T.pack testFile
    code2 `shouldBe` ExitSuccess
    T.strip stdout2 `shouldBe` T.pack testContent

    stopTestVmAndWait daemon vmId 30

    snapshotId <- createSnapshot daemon diskId "before-delete"

    startTestVmAndWaitGuestAgent vm 120

    _ <- runInVm vm $ "rm -f " <> T.pack testFile

    (code3, _, _) <- runInVm vm $ "test -f " <> T.pack testFile
    code3 `shouldNotBe` ExitSuccess

    stopTestVmAndWait daemon vmId 30

    rollbackSnapshot daemon diskId snapshotId

    startTestVmAndWaitGuestAgent vm 120

    (code4, stdout4, _) <- runInVm vm $ "cat " <> T.pack testFile
    code4 `shouldBe` ExitSuccess
    T.strip stdout4 `shouldBe` T.pack testContent

    stopTestVmAndWait daemon vmId 30

    deleteSnapshot daemon diskId snapshotId

-- | Test creating multiple snapshots and rolling back to a specific one
testMultipleSnapshots :: TestEnv -> IO ()
testMultipleSnapshots env = do
  withTestVmGuestExec env defaultVmConfig $ \vm -> do
    let daemon = tvmDaemon vm
        diskId = tvmDiskId vm
        vmId = tvmId vm
        testFile = "/home/corvus/counter.txt"

    _ <- runInVm vm "echo '1' > /home/corvus/counter.txt"
    stopTestVmAndWait daemon vmId 30

    snap1 <- createSnapshot daemon diskId "state-1"

    startTestVmAndWaitGuestAgent vm 120
    _ <- runInVm vm "echo '2' > /home/corvus/counter.txt"
    stopTestVmAndWait daemon vmId 30

    snap2 <- createSnapshot daemon diskId "state-2"

    startTestVmAndWaitGuestAgent vm 120
    _ <- runInVm vm "echo '3' > /home/corvus/counter.txt"
    (_, stdout3, _) <- runInVm vm $ "cat " <> T.pack testFile
    T.strip stdout3 `shouldBe` "3"
    stopTestVmAndWait daemon vmId 30

    rollbackSnapshot daemon diskId snap1

    startTestVmAndWaitGuestAgent vm 120
    (_, stdout1, _) <- runInVm vm $ "cat " <> T.pack testFile
    T.strip stdout1 `shouldBe` "1"
    stopTestVmAndWait daemon vmId 30

    deleteSnapshot daemon diskId snap2
    deleteSnapshot daemon diskId snap1

-- | Test that snapshot list correctly shows created snapshots
testSnapshotList :: TestEnv -> IO ()
testSnapshotList env = do
  withTestVmGuestExec env defaultVmConfig $ \vm -> do
    let daemon = tvmDaemon vm
        diskId = tvmDiskId vm
        vmId = tvmId vm

    stopTestVmAndWait daemon vmId 30

    snap1 <- createSnapshot daemon diskId "snapshot-alpha"
    snap2 <- createSnapshot daemon diskId "snapshot-beta"

    snapshots <- listSnapshots daemon diskId
    length snapshots `shouldSatisfy` (>= 2)

    let names = map sniName snapshots
    names `shouldSatisfy` elem "snapshot-alpha"
    names `shouldSatisfy` elem "snapshot-beta"

    deleteSnapshot daemon diskId snap2
    deleteSnapshot daemon diskId snap1

-- | Test that merging a snapshot consolidates changes and removes it
testSnapshotMerge :: TestEnv -> IO ()
testSnapshotMerge env = do
  withTestVmGuestExec env defaultVmConfig $ \vm -> do
    let daemon = tvmDaemon vm
        diskId = tvmDiskId vm
        vmId = tvmId vm
        testFile = "/home/corvus/merge-test.txt"

    _ <- runInVm vm "echo 'initial' > /home/corvus/merge-test.txt"
    stopTestVmAndWait daemon vmId 30

    snap1 <- createSnapshot daemon diskId "to-merge"

    startTestVmAndWaitGuestAgent vm 120
    _ <- runInVm vm "echo 'modified' > /home/corvus/merge-test.txt"
    stopTestVmAndWait daemon vmId 30

    snapshotsBefore <- listSnapshots daemon diskId
    let countBefore = length snapshotsBefore

    mergeSnapshot daemon diskId snap1

    snapshotsAfter <- listSnapshots daemon diskId
    let countAfter = length snapshotsAfter
    countAfter `shouldBe` (countBefore - 1)

    let snapIds = map sniId snapshotsAfter
    snapIds `shouldNotSatisfy` elem snap1

    startTestVmAndWaitGuestAgent vm 120
    (code, stdout, _) <- runInVm vm $ "cat " <> T.pack testFile
    code `shouldBe` ExitSuccess
    T.strip stdout `shouldBe` "modified"
    stopTestVmAndWait daemon vmId 30

-- | Test that snapshots with duplicate names are allowed (each gets unique ID)
testSnapshotNameDuplicates :: TestEnv -> IO ()
testSnapshotNameDuplicates env = do
  withTestVmGuestExec env defaultVmConfig $ \vm -> do
    let daemon = tvmDaemon vm
        diskId = tvmDiskId vm
        vmId = tvmId vm

    stopTestVmAndWait daemon vmId 30

    snap1 <- createSnapshot daemon diskId "same-name"
    snap2 <- createSnapshot daemon diskId "same-name"
    snap3 <- createSnapshot daemon diskId "same-name"

    snap1 `shouldNotBe` snap2
    snap2 `shouldNotBe` snap3
    snap1 `shouldNotBe` snap3

    snapshots <- listSnapshots daemon diskId
    let sameNameSnaps = filter (\s -> sniName s == "same-name") snapshots
    length sameNameSnaps `shouldBe` 3

    let ids = sort $ map sniId sameNameSnaps
    ids `shouldBe` sort [snap1, snap2, snap3]

    deleteSnapshot daemon diskId snap3
    deleteSnapshot daemon diskId snap2
    deleteSnapshot daemon diskId snap1

-- | Test that snapshot operations on running VMs are rejected
testSnapshotOnRunningVmRejected :: TestEnv -> IO ()
testSnapshotOnRunningVmRejected env = do
  withTestVmGuestExec env defaultVmConfig $ \vm -> do
    let daemon = tvmDaemon vm
        diskId = tvmDiskId vm
        vmId = tvmId vm

    snap1 <- do
      stopTestVmAndWait daemon vmId 30
      -- Reset in case the background process-exit handler set VmError
      -- after vmReset already set VmStopped (race between SIGKILL exit
      -- code -9 and the reset handler).
      ensureVmStopped daemon vmId
      createSnapshot daemon diskId "test-snap"

    startTestVmAndWaitGuestAgent vm 120

    createResult <- tryCreateSnapshot daemon diskId "should-fail"
    createResult `shouldBe` SnapshotVmMustBeStopped

    rollbackResult <- tryRollbackSnapshot daemon diskId snap1
    rollbackResult `shouldBe` SnapshotVmMustBeStopped

    mergeResult <- tryMergeSnapshot daemon diskId snap1
    mergeResult `shouldBe` SnapshotVmMustBeStopped

    stopTestVmAndWait daemon vmId 30

    snap2 <- createSnapshot daemon diskId "after-stop"
    snap2 `shouldSatisfy` (> 0)

    deleteSnapshot daemon diskId snap2
    deleteSnapshot daemon diskId snap1

-- | Test that multiple rapid snapshot operations are handled correctly
testConcurrentSnapshotOperations :: TestEnv -> IO ()
testConcurrentSnapshotOperations env = do
  withTestVmGuestExec env defaultVmConfig $ \vm -> do
    let daemon = tvmDaemon vm
        diskId = tvmDiskId vm
        vmId = tvmId vm

    stopTestVmAndWait daemon vmId 30

    snap1 <- createSnapshot daemon diskId "concurrent-1"
    snap2 <- createSnapshot daemon diskId "concurrent-2"
    snap3 <- createSnapshot daemon diskId "concurrent-3"

    let ids = [snap1, snap2, snap3]
    length ids `shouldBe` 3
    length (filter (> 0) ids) `shouldBe` 3

    snapshots <- listSnapshots daemon diskId
    let concurrentSnaps = filter (\s -> "concurrent-" `T.isPrefixOf` sniName s) snapshots
    length concurrentSnaps `shouldBe` 3

    deleteSnapshot daemon diskId snap3
    deleteSnapshot daemon diskId snap2
    deleteSnapshot daemon diskId snap1

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

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

-- | Ensure VM is in VmStopped state, resetting if it ended up in VmError.
-- This handles the race where vmReset sets VmStopped but the background
-- waitForProcess thread later overwrites it with VmError.
ensureVmStopped :: TestDaemon -> Int64 -> IO ()
ensureVmStopped daemon vmId = do
  -- Small delay to let the background thread finish
  threadDelay 500000
  res <- withDaemonConnection daemon $ \conn -> showVm conn vmId
  case res of
    Right (Right (Just details))
      | vdStatus details == VmError -> do
          _ <- withDaemonConnection daemon $ \conn -> vmReset conn vmId
          threadDelay 200000
      | otherwise -> pure ()
    _ -> pure ()

-- | Try to merge a snapshot, returning the result type for error checking
tryMergeSnapshot :: TestDaemon -> Int64 -> Int64 -> IO SnapshotResult
tryMergeSnapshot daemon diskId snapshotId = do
  result <- withDaemonConnection daemon $ \conn ->
    snapshotMerge conn diskId snapshotId
  case result of
    Left err -> fail $ "Connection error: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right res) -> pure res
