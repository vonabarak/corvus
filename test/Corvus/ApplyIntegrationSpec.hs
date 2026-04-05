{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Integration tests for the `crv apply` declarative environment feature.
-- Tests that VMs created via YAML config can boot, communicate, and accept SSH keys.
--
-- Run with: stack test --test-arguments="--match ApplyIntegration"
module Corvus.ApplyIntegrationSpec (spec) where

import Control.Exception (bracket)
import Corvus.Client.Rpc (ApplyRpcResult (..), applyConfig)
import Corvus.Protocol (ApplyCreated (..), ApplyResult (..))
import Corvus.Utils.Yaml (yamlQQ)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (findFreePort)
import Test.VM.Daemon (withDaemonConnection, withTestDaemon)
import Test.VM.Image (ensureBaseImage)
import Test.VM.Rpc
  ( runViaGuestAgent
  , runViaGuestAgent_
  , startNetwork
  , startTestVm
  , startTestVmSync
  , stopNetwork
  , stopTestVmAndWait
  )
import Test.VM.Ssh (SshKeyPair (..), cleanupSshKeyPair, generateSshKeyPair, runInTestVmWith, waitForTestVmSshWithKey)

-- | Encode a value as YAML Text
encodeYaml :: (Yaml.ToJSON a) => a -> T.Text
encodeYaml = TE.decodeUtf8 . Yaml.encode

spec :: Spec
spec = withTestDb $ do
  describe "Apply integration" $ do
    it "creates two VMs with networking and guest agent, and they can communicate" $ \env -> do
      Right imagePath <- ensureBaseImage "corvus-test"
      withTestDaemon env $ \daemon -> do
        let yamlContent =
              encodeYaml
                [yamlQQ|
                  disks:
                    - name: apply-net-base
                      import: #{imagePath}
                    - name: apply-net-root1
                      overlay: apply-net-base
                    - name: apply-net-root2
                      overlay: apply-net-base
                  networks:
                    - name: apply-vde
                      subnet: ""
                  vms:
                    - name: apply-vm1
                      cpuCount: 2
                      ramMb: 2048
                      headless: true
                      guestAgent: true
                      drives:
                        - disk: apply-net-root1
                          interface: virtio
                      networkInterfaces:
                        - type: user
                        - type: managed
                          network: apply-vde
                    - name: apply-vm2
                      cpuCount: 2
                      ramMb: 2048
                      headless: true
                      guestAgent: true
                      drives:
                        - disk: apply-net-root2
                          interface: virtio
                      networkInterfaces:
                        - type: user
                        - type: managed
                          network: apply-vde
                |]

        -- Apply the config
        applyRes <- withDaemonConnection daemon $ \conn -> applyConfig conn yamlContent False True
        (vm1Id, vm2Id, nwId) <- case applyRes of
          Right (Right (ApplyOk r)) -> do
            length (arDisks r) `shouldBe` 3
            length (arNetworks r) `shouldBe` 1
            length (arVms r) `shouldBe` 2
            let [v1, v2] = arVms r
                [nw] = arNetworks r
            pure (acId v1, acId v2, acId nw)
          other -> fail $ "Apply failed: " ++ show other

        -- Start the network, then start both VMs
        bracket
          (startNetwork daemon nwId)
          (\_ -> stopNetwork daemon nwId)
          $ \_ -> do
            putStrLn "[test] Starting two VMs created via apply (sync)..."
            startTestVmSync daemon vm1Id
            startTestVmSync daemon vm2Id

            -- Configure static IPs on the VDE interface (eth1)
            runViaGuestAgent_ daemon vm1Id "ip addr add 10.0.0.1/24 dev eth1 && ip link set eth1 up"
            runViaGuestAgent_ daemon vm2Id "ip addr add 10.0.0.2/24 dev eth1 && ip link set eth1 up"

            -- Verify VM1 can ping VM2
            (codePing1, _, _) <- runViaGuestAgent daemon vm1Id "ping -c 3 -W 5 10.0.0.2"
            codePing1 `shouldBe` ExitSuccess

            -- Verify VM2 can ping VM1
            (codePing2, _, _) <- runViaGuestAgent daemon vm2Id "ping -c 3 -W 5 10.0.0.1"
            codePing2 `shouldBe` ExitSuccess

            -- Cleanup VMs
            stopTestVmAndWait daemon vm1Id 10
            stopTestVmAndWait daemon vm2Id 10

    it "deploys SSH key via cloud-init from a downloaded Alpine image" $ \env -> do
      withSystemTempDirectory "corvus-apply-ssh" $ \tmpDir -> do
        -- Generate SSH key pair
        keyResult <- generateSshKeyPair tmpDir
        keyPair <- case keyResult of
          Right kp -> pure kp
          Left err -> fail $ "SSH keygen failed: " ++ T.unpack err

        bracket (pure keyPair) cleanupSshKeyPair $ \kp -> do
          publicKeyContent <- TIO.readFile (skpPublicKey kp)
          sshPort <- findFreePort

          withTestDaemon env $ \daemon -> do
            let imageUrl = "https://dev.alpinelinux.org/~tomalok/alpine-cloud-images/v3.20/nocloud/x86_64/nocloud_alpine-3.20.9-x86_64-bios-cloudinit-r0.qcow2" :: T.Text
                hostFwd = "hostfwd=tcp::" <> T.pack (show sshPort) <> "-:22"
                yamlContent =
                  encodeYaml
                    [yamlQQ|
                      sshKeys:
                        - name: apply-test-key
                          publicKey: #{T.strip publicKeyContent}
                      disks:
                        - name: apply-alpine-base
                          import: #{imageUrl}
                        - name: apply-alpine-root
                          overlay: apply-alpine-base
                          sizeMb: 2048
                      vms:
                        - name: apply-ssh-vm
                          cpuCount: 2
                          ramMb: 2048
                          headless: true
                          guestAgent: true
                          cloudInit: true
                          drives:
                            - disk: apply-alpine-root
                              interface: virtio
                          networkInterfaces:
                            - type: user
                              hostDevice: #{hostFwd}
                          sshKeys:
                            - apply-test-key
                    |]

            -- Apply the config
            applyRes <- withDaemonConnection daemon $ \conn -> applyConfig conn yamlContent False True
            vmId <- case applyRes of
              Right (Right (ApplyOk r)) -> do
                length (arSshKeys r) `shouldBe` 1
                length (arDisks r) `shouldBe` 2
                length (arVms r) `shouldBe` 1
                pure $ acId (head (arVms r))
              other -> fail $ "Apply failed: " ++ show other

            -- Start VM and wait for SSH with the deployed key
            putStrLn "[test] Starting VM and waiting for SSH (cloud-init key deployment)..."
            startTestVm daemon vmId
            waitForTestVmSshWithKey "localhost" sshPort (skpPrivateKey kp) "corvus" 180

            -- Verify SSH works with the deployed key
            (code1, stdout1, _) <- runInTestVmWith "localhost" sshPort (skpPrivateKey kp) "corvus" "echo ssh-via-apply"
            code1 `shouldBe` ExitSuccess
            T.strip stdout1 `shouldBe` "ssh-via-apply"

            -- Verify the SSH key is in authorized_keys
            (code2, stdout2, _) <- runInTestVmWith "localhost" sshPort (skpPrivateKey kp) "corvus" "cat ~/.ssh/authorized_keys"
            code2 `shouldBe` ExitSuccess
            T.isInfixOf "corvus-test@localhost" stdout2 `shouldBe` True

            -- Cleanup
            stopTestVmAndWait daemon vmId 30

    it "deploys SSH key via apply with custom cloud-init config" $ \env -> do
      withSystemTempDirectory "corvus-apply-custom-ci" $ \tmpDir -> do
        -- Generate SSH key pair
        keyResult <- generateSshKeyPair tmpDir
        keyPair <- case keyResult of
          Right kp -> pure kp
          Left err -> fail $ "SSH keygen failed: " ++ T.unpack err

        bracket (pure keyPair) cleanupSshKeyPair $ \kp -> do
          publicKeyContent <- TIO.readFile (skpPublicKey kp)
          sshPort <- findFreePort

          withTestDaemon env $ \daemon -> do
            let imageUrl = "https://dev.alpinelinux.org/~tomalok/alpine-cloud-images/v3.20/nocloud/x86_64/nocloud_alpine-3.20.9-x86_64-bios-cloudinit-r0.qcow2" :: T.Text
                hostFwd = "hostfwd=tcp::" <> T.pack (show sshPort) <> "-:22"
                yamlContent =
                  encodeYaml
                    [yamlQQ|
                      sshKeys:
                        - name: apply-ci-key
                          publicKey: #{T.strip publicKeyContent}
                      disks:
                        - name: apply-ci-base
                          import: #{imageUrl}
                        - name: apply-ci-root
                          overlay: apply-ci-base
                          sizeMb: 2048
                      vms:
                        - name: apply-ci-vm
                          cpuCount: 2
                          ramMb: 2048
                          headless: true
                          guestAgent: true
                          cloudInit: true
                          cloudInitConfig:
                            userData:
                              users:
                                - name: deployer
                                  sudo: "ALL=(ALL) NOPASSWD:ALL"
                                  shell: /bin/sh
                                  lock_passwd: false
                                  plain_text_passwd: testpass
                              ssh_pwauth: true
                              package_update: true
                              packages:
                                - qemu-guest-agent
                              runcmd:
                                - "rc-service sshd restart || systemctl restart ssh || true"
                                - "rc-update add qemu-guest-agent default || true"
                                - "rc-service qemu-guest-agent start || true"
                            injectSshKeys: true
                          drives:
                            - disk: apply-ci-root
                              interface: virtio
                          networkInterfaces:
                            - type: user
                              hostDevice: #{hostFwd}
                          sshKeys:
                            - apply-ci-key
                    |]

            -- Apply the config
            applyRes <- withDaemonConnection daemon $ \conn -> applyConfig conn yamlContent False True
            vmId <- case applyRes of
              Right (Right (ApplyOk r)) -> do
                length (arVms r) `shouldBe` 1
                pure $ acId (head (arVms r))
              other -> fail $ "Apply failed: " ++ show other

            -- Start VM and wait for SSH with the custom user
            putStrLn "[test] Starting VM and waiting for SSH (custom cloud-init config)..."
            startTestVm daemon vmId
            waitForTestVmSshWithKey "localhost" sshPort (skpPrivateKey kp) "deployer" 180

            -- Verify SSH works with the custom user
            (code1, stdout1, _) <- runInTestVmWith "localhost" sshPort (skpPrivateKey kp) "deployer" "echo custom-apply-ok"
            code1 `shouldBe` ExitSuccess
            T.strip stdout1 `shouldBe` "custom-apply-ok"

            -- Verify the custom user was created
            (code2, stdout2, _) <- runInTestVmWith "localhost" sshPort (skpPrivateKey kp) "deployer" "whoami"
            code2 `shouldBe` ExitSuccess
            T.strip stdout2 `shouldBe` "deployer"

            -- Cleanup
            stopTestVmAndWait daemon vmId 30
