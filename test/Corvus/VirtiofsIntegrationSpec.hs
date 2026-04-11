{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration tests for virtiofs shared directories.
-- These tests verify that virtiofsd is properly managed by the daemon
-- and that shared directories are accessible from within VMs.
--
-- Requirements:
--   - QEMU with KVM support
--   - virtiofsd binary
--   - PostgreSQL for test database
--
-- Run with: stack test --test-arguments="--match Virtiofs"
module Corvus.VirtiofsIntegrationSpec (spec) where

import Control.Exception (bracket)
import Corvus.Client (vmStart)
import Corvus.Model (SharedDirCache (..), TaskResult (..), TaskSubsystem (..))
import Corvus.Protocol (TaskInfo (..))
import Data.List (find)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (getCanonicalTemporaryDirectory)
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (TestVm (..), VmConfig (..), defaultVmConfig, withTestDiskSetup, withTestVm)
import Test.VM.Daemon (TestDaemon (..), withDaemonConnection, withTestDaemon)
import Test.VM.Rpc (addVmDisk, addVmNetIf, addVmSharedDir, createTestVmWithGuestAgent, deleteTestVm, listSubtasks, listTasks)
import Test.VM.Ssh (runInTestVm)

spec :: Spec
spec = withTestDb $ do
  describe "Virtiofs integration" $ do
    it "can access shared directory from VM via virtiofs" $ \env -> do
      -- Create a temporary directory with a test file
      sysTmp <- getCanonicalTemporaryDirectory
      uuid <- nextRandom
      let testDir = sysTmp </> ("virtiofs-test-" <> T.unpack (T.take 8 (toText uuid)))
          testFile = testDir </> "testfile.txt"
          testContent = "UUID:" <> T.unpack (toText uuid)

      createDirectoryIfMissing True testDir
      writeFile testFile testContent

      bracket
        (pure ())
        (\_ -> removeDirectoryRecursive testDir)
        $ \_ ->
          withTestVm env (defaultVmConfig {vmcSharedDir = Just testDir, vmcWaitSshTimeout = 300}) $ \vm -> do
            -- Mount the shared directory (requires root via doas)
            (code2, _, _) <- runInTestVm vm "doas mkdir -p /mnt/share"
            code2 `shouldBe` ExitSuccess

            (code3, _, _) <- runInTestVm vm "doas mount -t virtiofs share /mnt/share"
            code3 `shouldBe` ExitSuccess

            -- Read the test file
            (code4, stdout4, _) <- runInTestVm vm "cat /mnt/share/testfile.txt"
            code4 `shouldBe` ExitSuccess
            T.strip stdout4 `shouldBe` T.pack testContent

            -- Write a file from the guest and verify on host
            let writeContent = "WRITTEN-BY-GUEST:" <> T.unpack (T.take 8 (toText uuid))
            (code5, _, _) <- runInTestVm vm $ "echo '" <> T.pack writeContent <> "' > /mnt/share/guest-file.txt"
            code5 `shouldBe` ExitSuccess

            hostContent <- readFile (testDir </> "guest-file.txt")
            T.strip (T.pack hostContent) `shouldBe` T.pack writeContent

    it "VM start fails when shared directory does not exist" $ \env ->
      withTestDaemon env $ \daemon ->
        withTestDiskSetup daemon defaultVmConfig $ \diskId cfg -> do
          -- Create a VM
          vmUuid <- nextRandom
          let vmName = "virtiofsd-fail-" <> T.take 8 (toText vmUuid)
          vmId <- createTestVmWithGuestAgent daemon vmName 1 512 Nothing True

          -- Add boot disk
          addVmDisk daemon vmId diskId (vmcDiskInterface cfg) (vmcDiskCache cfg) (vmcDiskDiscard cfg) False

          -- Add network interface
          addVmNetIf daemon vmId (vmcNetworkType cfg) "" Nothing

          -- Add shared directory pointing to non-existent path
          addVmSharedDir daemon vmId "/tmp/nonexistent-dir-that-does-not-exist-12345" "badshare" CacheNever

          -- Try to start the VM (sync) — should fail
          startResult <- withDaemonConnection daemon $ \conn ->
            vmStart conn (T.pack (show vmId)) True
          case startResult of
            Left _ -> pure () -- Expected: ServerError from virtiofsd failure
            Right (Left _) -> pure () -- RPC error
            Right (Right _) -> fail "Expected VM start to fail due to missing shared directory"

          -- Verify task history shows a failed start-virtiofsd subtask
          -- Find the VM start task (match by command and entity ID)
          allTasks <- listTasks daemon 50 Nothing Nothing True
          let startTask = find (\t -> tiCommand t == "start" && tiSubsystem t == SubVm && tiEntityId t == Just (fromIntegral vmId)) allTasks
          case startTask of
            Nothing -> fail "Expected to find a 'start' task for the VM"
            Just parentTask -> do
              -- Check subtasks
              subtasks <- listSubtasks daemon (tiId parentTask)
              let virtiofsdSubtask = find (\t -> tiCommand t == "start-virtiofsd") subtasks
              case virtiofsdSubtask of
                Nothing -> fail "Expected to find a 'start-virtiofsd' subtask"
                Just vt -> tiResult vt `shouldBe` TaskError

          -- Cleanup
          deleteTestVm daemon vmId
