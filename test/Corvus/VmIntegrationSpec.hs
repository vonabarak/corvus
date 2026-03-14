{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests that run commands inside a real VM via the Corvus daemon.
-- These tests require:
--   - QEMU with KVM support
--   - virtiofsd binary
--   - genisoimage or mkisofs
--   - SSH client
--   - PostgreSQL for test database
--   - Network access to download AlmaLinux cloud image (first run only)
--
-- Run with: cabal test --test-arguments="--match VmIntegration"
module Corvus.VmIntegrationSpec (spec) where

import Control.Exception (bracket)
import Corvus.Client (DiskResult (..), diskRegister)
import Corvus.Model (DriveFormat (..))
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (getCanonicalTemporaryDirectory)
import Test.DSL.Daemon
import Test.Daemon (startTestDaemon, stopTestDaemon, withDaemonConnection)
import Test.Database (TestEnv, withTestDb)
import Test.Hspec
import Test.VM.Image (createOverlay, defaultImageConfig, ensureBaseImage, removeOverlay)

spec :: Spec
spec = withTestDb $ do
  describe "VM integration through daemon (requires cloud-init support)" $ do
    -- These tests require a fully functioning cloud-init setup.
    -- They have been verified to work manually but are flaky in CI due to
    -- timing issues with cloud-init user creation.
    it "can run echo command in VM" $ \env -> do
      withTestVm env $ \vm -> do
        (code, stdout, _stderr) <- runInDaemonVm vm "echo hello"
        code `shouldBe` ExitSuccess
        T.strip stdout `shouldBe` "hello"

    it "can check OS release" $ \env -> do
      withTestVm env $ \vm -> do
        (code, stdout, _stderr) <- runInDaemonVm vm "cat /etc/os-release | grep -i alma"
        code `shouldBe` ExitSuccess
        T.isInfixOf "AlmaLinux" stdout `shouldBe` True

    it "can run multiple commands" $ \env -> do
      withTestVm env $ \vm -> do
        runInDaemonVm_ vm "echo 'test content' > /tmp/testfile"
        (code, stdout, _) <- runInDaemonVm vm "cat /tmp/testfile"
        code `shouldBe` ExitSuccess
        T.strip stdout `shouldBe` "test content"

    it "reports command failures correctly" $ \env -> do
      withTestVm env $ \vm -> do
        (code, _, stderr) <- runInDaemonVm vm "nonexistent_command_12345"
        code `shouldNotBe` ExitSuccess
        T.isInfixOf "not found" stderr `shouldBe` True

    it "can get system information" $ \env -> do
      withTestVm env $ \vm -> do
        (code1, stdout1, _) <- runInDaemonVm vm "uname -s"
        code1 `shouldBe` ExitSuccess
        T.strip stdout1 `shouldBe` "Linux"

        (code2, _, _) <- runInDaemonVm vm "free -m"
        code2 `shouldBe` ExitSuccess

        (code3, _, _) <- runInDaemonVm vm "df -h"
        code3 `shouldBe` ExitSuccess

    it "can use sudo without password" $ \env -> do
      withTestVm env $ \vm -> do
        (code, stdout, _) <- runInDaemonVm vm "sudo whoami"
        code `shouldBe` ExitSuccess
        T.strip stdout `shouldBe` "root"

-- | Run a test with a daemon-managed VM.
-- This handles all setup: daemon, disk image, VM creation, and cleanup.
withTestVm :: Test.Database.TestEnv -> (DaemonVm -> IO a) -> IO a
withTestVm env action = do
  daemon <- startTestDaemon env

  -- Ensure base image exists
  imageResult <- ensureBaseImage defaultImageConfig
  basePath <- case imageResult of
    Left err -> fail $ "Failed to get base image: " <> T.unpack err
    Right path -> pure path

  -- Create unique overlay for this test
  uuid <- nextRandom
  tmpDir <- getCanonicalTemporaryDirectory
  let overlayPath = tmpDir </> ("test-vm-" <> T.unpack (T.take 8 (toText uuid)) <> ".qcow2")

  overlayResult <- createOverlay basePath overlayPath
  case overlayResult of
    Left err -> do
      stopTestDaemon daemon
      fail $ "Failed to create overlay: " <> T.unpack err
    Right _ -> pure ()

  -- Use bracket to ensure cleanup
  bracket
    (pure ())
    ( \_ -> do
        stopTestDaemon daemon
        removeOverlay overlayPath
    )
    ( \_ -> do
        -- Register the overlay as a disk
        diskResult <- withDaemonConnection daemon $ \conn ->
          diskRegister conn "test-boot-disk" (T.pack overlayPath) FormatQcow2 Nothing

        diskId <- case diskResult of
          Left err -> fail $ "Connection error: " <> show err
          Right (Left err) -> fail $ "Failed to register disk: " <> show err
          Right (Right (DiskCreated dId)) -> pure dId
          Right (Right other) -> fail $ "Unexpected response: " <> show other

        withDaemonVm daemon diskId Nothing action
    )
