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
-- Run with: stack test --test-arguments="--match VmIntegration"
module Corvus.VmIntegrationSpec (spec) where

import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.DSL.Daemon
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (withTestVm)

spec :: Spec
spec = withTestDb $ do
  describe "VM integration through daemon (requires cloud-init support)" $ do
    -- These tests require a fully functioning cloud-init setup.
    -- They have been verified to work manually but are flaky in CI due to
    -- timing issues with cloud-init user creation.
    it "can run echo command in VM" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        (code, stdout, _stderr) <- runInDaemonVm vm "echo hello"
        code `shouldBe` ExitSuccess
        T.strip stdout `shouldBe` "hello"

    it "can check OS release" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        (code, stdout, _stderr) <- runInDaemonVm vm "cat /etc/os-release | grep -i alma"
        code `shouldBe` ExitSuccess
        T.isInfixOf "AlmaLinux" stdout `shouldBe` True

    it "can run multiple commands" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        runInDaemonVm_ vm "echo 'test content' > /tmp/testfile"
        (code, stdout, _) <- runInDaemonVm vm "cat /tmp/testfile"
        code `shouldBe` ExitSuccess
        T.strip stdout `shouldBe` "test content"

    it "reports command failures correctly" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        (code, _, stderr) <- runInDaemonVm vm "nonexistent_command_12345"
        code `shouldNotBe` ExitSuccess
        T.isInfixOf "not found" stderr `shouldBe` True

    it "can get system information" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        (code1, stdout1, _) <- runInDaemonVm vm "uname -s"
        code1 `shouldBe` ExitSuccess
        T.strip stdout1 `shouldBe` "Linux"

        (code2, _, _) <- runInDaemonVm vm "free -m"
        code2 `shouldBe` ExitSuccess

        (code3, _, _) <- runInDaemonVm vm "df -h"
        code3 `shouldBe` ExitSuccess

    it "can use sudo without password" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        (code, stdout, _) <- runInDaemonVm vm "sudo whoami"
        code `shouldBe` ExitSuccess
        T.strip stdout `shouldBe` "root"
