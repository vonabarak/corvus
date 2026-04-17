{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for cloud-init SSH key setup across multiple OS images.
-- Verifies that cloud-init correctly deploys SSH keys on each supported OS,
-- for both UEFI and BIOS boot modes. Reports boot-to-SSH timing for each image.
--
-- Requirements:
--   - QEMU with KVM support
--   - virtiofsd binary
--   - genisoimage or mkisofs
--   - SSH client
--   - PostgreSQL for test database
--   - Network access to download cloud images (first run only)
--
-- Run with: stack test --test-arguments="--match MultiOsIntegration"
module Corvus.CloudInitIntegrationSpec (spec) where

import Control.Monad (when)
import Corvus.Client.Rpc (CloudInitResult (..))
import Corvus.Protocol (CloudInitInfo (..))
import Data.Maybe (isJust)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (TestVm (..), VmConfig (..), cloudVmConfig, findFreePort, startTestVmAndWait, withTestDiskSetup, withTestVm, withTestVmConsole)
import Test.VM.Console (consoleDrain, consoleExpect, consoleSend)
import Test.VM.Daemon (withTestDaemon)
import Test.VM.Rpc (addVmDisk, addVmNetIf, cleanupSshKey, createTestVmWithOptions, deleteCloudInitConfig, deleteTestVm, getCloudInitConfig, setCloudInitConfig, setupVmSshKey, stopTestVmAndWait)
import Test.VM.Ssh (SshKeyPair (..), generateSshKeyPair, runInTestVm, runInTestVmWith, waitForTestVmSshWithKey)

-- | VM config for multi-OS cloud-image tests (uses cloud-init for SSH key deployment)
multiOsConfig :: VmConfig
multiOsConfig = cloudVmConfig {vmcWaitSshTimeout = 300}

-- | Verify SSH key auth works, key is deployed, user was created,
-- and optionally check privilege escalation and qemu-guest-agent.
verifyVm
  :: Bool
  -- ^ Check sudo/doas privilege escalation
  -> Bool
  -- ^ Check qemu-guest-agent is installed
  -> TestVm
  -> IO ()
verifyVm checkPrivEsc checkGuestAgent vm = do
  (code, stdout, _) <- runInTestVm vm "echo ssh-ok"
  code `shouldBe` ExitSuccess
  T.strip stdout `shouldBe` "ssh-ok"

  (code2, stdout2, _) <- runInTestVm vm "cat ~/.ssh/authorized_keys"
  code2 `shouldBe` ExitSuccess
  T.isInfixOf "ssh-" stdout2 `shouldBe` True

  (code3, stdout3, _) <- runInTestVm vm "whoami"
  code3 `shouldBe` ExitSuccess
  T.strip stdout3 `shouldBe` "corvus"

  -- Verify hostname is set (cloud-init sets local-hostname from VM name)
  (codeH, stdoutH, _) <- runInTestVm vm "hostname"
  codeH `shouldBe` ExitSuccess
  T.isPrefixOf "test-vm-" (T.strip stdoutH) `shouldBe` True

  -- Wait for cloud-init to finish. SSH becomes available before cloud-init
  -- completes modules-final (which runs runcmd and package installation),
  -- so doas config and qemu-guest-agent may not be ready yet.
  when (checkPrivEsc || checkGuestAgent) $ do
    (_, _, _) <- runInTestVm vm "n=0; while [ $n -lt 60 ]; do s=$(cloud-init status 2>/dev/null); case \"$s\" in *done*|*error*|*disabled*) break;; esac; n=$((n+1)); sleep 2; done"
    pure ()

  -- Verify privilege escalation to root (sudo on Linux, doas on Alpine)
  when checkPrivEsc $ do
    (code4, stdout4, _) <- runInTestVm vm "sudo whoami 2>/dev/null || doas whoami 2>/dev/null"
    code4 `shouldBe` ExitSuccess
    T.strip stdout4 `shouldBe` "root"

  -- Verify qemu-guest-agent is installed
  when checkGuestAgent $ do
    (code5, _, _) <- runInTestVm vm "which qemu-ga 2>/dev/null || command -v qemu-ga 2>/dev/null || test -x /usr/sbin/qemu-ga || test -x /usr/local/bin/qemu-ga"
    code5 `shouldBe` ExitSuccess

spec :: Spec
spec = withTestDb $ do
  describe "Multi-OS cloud-init integration" $ do
    describe "Alpine Linux" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "alpine-3.20-uefi"}) (verifyVm True False)

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "alpine-3.20-bios", vmcUefi = False}) (verifyVm True False)

    describe "AlmaLinux" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "almalinux-10"}) (verifyVm True False)

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "almalinux-10", vmcUefi = False}) (verifyVm True False)

      it "SSH key setup works with multiple SSH keys" $ \env -> do
        let config = multiOsConfig {vmcOsName = "almalinux-10"}
        withTestDaemon env $ \daemon ->
          withTestDiskSetup daemon config $ \diskId cfg ->
            withSystemTempDirectory "corvus-multi-key" $ \tmpDir -> do
              -- Create VM with cloud-init
              vmId <- createTestVmWithOptions daemon "test-vm-multikey" 2 2048 Nothing True False True

              -- Add boot disk
              addVmDisk daemon vmId diskId (vmcDiskInterface cfg) (vmcDiskCache cfg) (vmcDiskDiscard cfg) False

              -- Set up two SSH key pairs
              (keyId1, privKey1, _) <- setupVmSshKey daemon vmId tmpDir
              (keyId2, privKey2, _) <- setupVmSshKey daemon vmId tmpDir

              -- Add network with SSH port forwarding
              sshPort <- findFreePort
              let hostFwd = "hostfwd=tcp::" <> T.pack (show sshPort) <> "-:22"
              addVmNetIf daemon vmId (vmcNetworkType cfg) hostFwd Nothing

              -- Start VM and wait for SSH
              let vm = TestVm vmId diskId sshPort "localhost" daemon privKey1 keyId1 "corvus"
              startTestVmAndWait vm 300

              -- Verify both keys are in authorized_keys
              (code, stdout, _) <- runInTestVm vm "cat ~/.ssh/authorized_keys"
              code `shouldBe` ExitSuccess
              let keyCount = length $ filter (T.isPrefixOf "ssh-") (T.lines stdout)
              keyCount `shouldSatisfy` (>= 2)

              -- Verify SSH works with the second key too
              (code2, stdout2, _) <- runInTestVmWith "localhost" sshPort privKey2 "corvus" "echo key2-ok"
              code2 `shouldBe` ExitSuccess
              T.strip stdout2 `shouldBe` "key2-ok"

              -- Cleanup
              stopTestVmAndWait daemon vmId 30
              deleteTestVm daemon vmId
              cleanupSshKey daemon keyId1
              cleanupSshKey daemon keyId2

    describe "Ubuntu" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "ubuntu-24.04"}) (verifyVm True False)

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "ubuntu-24.04", vmcUefi = False}) (verifyVm True False)

    describe "Debian" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "debian-12"}) (verifyVm True False)

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "debian-12", vmcUefi = False}) (verifyVm True False)

    describe "Gentoo" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "gentoo"}) (verifyVm True False)

    describe "FreeBSD" $ do
      it "SSH key setup works with UEFI boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "freebsd-14"}) (verifyVm False False)

      it "SSH key setup works with BIOS boot" $ \env -> do
        withTestVm env (multiOsConfig {vmcOsName = "freebsd-14", vmcUefi = False}) (verifyVm False False)

      it "serial console login works" $ \env -> do
        withTestVmConsole env (multiOsConfig {vmcOsName = "freebsd-14", vmcHeadless = True}) $ \console -> do
          -- FreeBSD cloud-init reboots the VM after first boot, so the login
          -- prompt only appears after the second boot cycle (~60s total).
          -- Send Enter to trigger a fresh prompt once getty is ready.
          _ <- consoleExpect console "login:" 90
          _ <- consoleDrain console
          -- Log in as root (no password on FreeBSD console by default)
          consoleSend console "root"
          _ <- consoleExpect console "#" 30
          _ <- consoleDrain console
          -- Run whoami and verify
          consoleSend console "whoami"
          output <- consoleExpect console "root" 10
          T.isInfixOf "root" output `shouldBe` True

    describe "Custom cloud-init config" $ do
      it "deploys custom user-data with SSH key injection" $ \env -> do
        let config = multiOsConfig {vmcOsName = "alpine-3.20-bios", vmcUefi = False}
        withTestDaemon env $ \daemon ->
          withTestDiskSetup daemon config $ \diskId cfg ->
            withSystemTempDirectory "corvus-custom-ci" $ \tmpDir -> do
              -- Create VM with cloud-init enabled
              vmId <- createTestVmWithOptions daemon "test-vm-custom-ci" 2 2048 Nothing True False True

              -- Add boot disk
              addVmDisk daemon vmId diskId (vmcDiskInterface cfg) (vmcDiskCache cfg) (vmcDiskDiscard cfg) False

              -- Set custom cloud-init config with a custom user and curl package
              let customUserData =
                    T.unlines
                      [ "users:"
                      , "  - name: testadmin"
                      , "    sudo: ALL=(ALL) NOPASSWD:ALL"
                      , "    shell: /bin/sh"
                      , "    lock_passwd: false"
                      , "    plain_text_passwd: testpass"
                      , "ssh_pwauth: true"
                      , "package_update: false"
                      , "runcmd:"
                      , "  - rc-service sshd restart || systemctl restart ssh || true"
                      ]
              setCloudInitConfig daemon vmId (Just customUserData) Nothing True

              -- Verify config was stored
              ciResult <- getCloudInitConfig daemon vmId
              case ciResult of
                CloudInitConfig (Just ci) -> ciiInjectSshKeys ci `shouldBe` True
                other -> fail $ "Expected CloudInitConfig, got: " ++ show other

              -- Set up SSH key (will be injected into first user's ssh_authorized_keys)
              (keyId1, privKey1, _) <- setupVmSshKey daemon vmId tmpDir

              -- Add network with SSH port forwarding
              sshPort <- findFreePort
              let hostFwd = "hostfwd=tcp::" <> T.pack (show sshPort) <> "-:22"
              addVmNetIf daemon vmId (vmcNetworkType cfg) hostFwd Nothing

              -- Start VM and wait for SSH with custom user
              let vm = TestVm vmId diskId sshPort "localhost" daemon privKey1 keyId1 "testadmin"
              startTestVmAndWait vm 300

              -- Verify SSH works with the custom user
              (code, stdout, _) <- runInTestVmWith "localhost" sshPort privKey1 "testadmin" "echo custom-ci-ok"
              code `shouldBe` ExitSuccess
              T.strip stdout `shouldBe` "custom-ci-ok"

              -- Verify the custom user exists
              (code2, stdout2, _) <- runInTestVmWith "localhost" sshPort privKey1 "testadmin" "whoami"
              code2 `shouldBe` ExitSuccess
              T.strip stdout2 `shouldBe` "testadmin"

              -- Verify curl was installed
              (code3, _, _) <- runInTestVmWith "localhost" sshPort privKey1 "testadmin" "n=0; while [ $n -lt 60 ]; do s=$(cloud-init status 2>/dev/null); case \"$s\" in *done*|*error*|*disabled*) break;; esac; n=$((n+1)); sleep 2; done"
              code3 `shouldBe` ExitSuccess

              -- Cleanup
              stopTestVmAndWait daemon vmId 30
              deleteTestVm daemon vmId
              cleanupSshKey daemon keyId1

      it "cloud-init config CRUD operations work" $ \env -> do
        withTestDaemon env $ \daemon -> do
          -- Create VM with cloud-init enabled
          vmId <- createTestVmWithOptions daemon "test-vm-ci-crud" 1 512 Nothing True False True

          -- Initially no custom config
          r1 <- getCloudInitConfig daemon vmId
          case r1 of
            CloudInitConfig Nothing -> pure ()
            other -> fail $ "Expected no config, got: " ++ show other

          -- Set custom config
          setCloudInitConfig daemon vmId (Just "users:\n  - name: myuser\n") (Just "version: 2\n") False

          -- Verify it was stored
          r2 <- getCloudInitConfig daemon vmId
          case r2 of
            CloudInitConfig (Just ci) -> do
              ciiUserData ci `shouldSatisfy` isJust
              ciiNetworkConfig ci `shouldSatisfy` isJust
              ciiInjectSshKeys ci `shouldBe` False
            other -> fail $ "Expected config, got: " ++ show other

          -- Delete custom config
          deleteCloudInitConfig daemon vmId

          -- Verify it's gone
          r3 <- getCloudInitConfig daemon vmId
          case r3 of
            CloudInitConfig Nothing -> pure ()
            other -> fail $ "Expected no config after delete, got: " ++ show other

          -- Cleanup
          deleteTestVm daemon vmId
