{-# LANGUAGE OverloadedStrings #-}

module Corvus.TemplateIntegrationSpec (spec) where

import Control.Monad (void)
import Corvus.Client
import Corvus.Model (NetInterfaceType (..))
import Corvus.Protocol
import Data.List (find)
import Data.Maybe (isJust)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.DSL.Daemon
import Test.Daemon (withDaemonConnection)
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (withTestVm)

-- | Find a disk name matching a prefix from a list of disk images
findDiskName :: T.Text -> [DiskImageInfo] -> T.Text
findDiskName prefix disks =
  case find (T.isPrefixOf prefix . diiName) disks of
    Just d -> diiName d
    Nothing -> error $ "No disk found with prefix: " <> T.unpack prefix

spec :: Spec
spec = withTestDb $ do
  describe "VM Template integration" $ do
    it "can create a template from YAML and instantiate it" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        let daemon = dvmDaemon vm
            sshKeyName = "template-key"
            privateKey = dvmSshPrivateKey vm

        -- Read the public key content from the existing VM's setup
        pubKeyContent <- T.pack <$> readFile (privateKey ++ ".pub")

        -- Create an SSH key to refer to in the template
        keyId <- withDaemonConnection daemon $ \conn -> sshKeyCreate conn sshKeyName pubKeyContent
        case keyId of
          Right (Right (SshKeyCreated _)) -> pure ()
          other -> fail $ "SSH key creation failed: " ++ show other

        -- Stop the VM so we can use its disk for cloning/overlay strategy
        stopDaemonVmAndWait daemon (dvmId vm) 10

        -- Discover actual disk names (they have unique suffixes)
        diskListRes <- withDaemonConnection daemon $ \conn -> diskList conn
        disks <- case diskListRes of
          Right (Right (DiskListResult ds)) -> pure ds
          other -> fail $ "Failed to list disks: " ++ show other
        let baseDiskName = findDiskName "base-image" disks
            ovmfCodeName = findDiskName "ovmf-code" disks
            ovmfVarsTemplateName = findDiskName "ovmf-vars-template" disks

        let templateYaml =
              T.unlines
                [ "name: \"test-template\""
                , "cpuCount: 2"
                , "ramMb: 2048"
                , "description: \"A test template\""
                , "drives:"
                , "  - diskImageName: \"" <> baseDiskName <> "\""
                , "    interface: \"virtio\""
                , "    strategy: \"overlay\""
                , "    newSizeMb: 1024"
                , "  - diskImageName: \"" <> ovmfCodeName <> "\""
                , "    interface: \"pflash\""
                , "    strategy: \"direct\""
                , "    readOnly: true"
                , "  - diskImageName: \"" <> ovmfVarsTemplateName <> "\""
                , "    interface: \"pflash\""
                , "    strategy: \"clone\""
                , "sshKeys:"
                , "  - name: \"" <> sshKeyName <> "\""
                ]

        -- 1. Create template
        resCreate <- withDaemonConnection daemon $ \conn -> templateCreate conn templateYaml
        templateId <- case resCreate of
          Right (Right (TemplateCreated tid)) -> pure tid
          other -> fail $ "Template creation failed: " ++ show other

        -- 2. List templates
        resList <- withDaemonConnection daemon $ \conn -> templateList conn
        case resList of
          Right (Right (TemplateListResult templates)) -> do
            let mTemplate = find (\t -> tviId t == templateId) templates
            mTemplate `shouldSatisfy` isJust
          other -> fail $ "Template list failed: " ++ show other

        -- 3. Show template
        resShow <- withDaemonConnection daemon $ \conn -> templateShow conn templateId
        case resShow of
          Right (Right (TemplateDetailsResult details)) -> do
            tvdName details `shouldBe` "test-template"
            length (tvdDrives details) `shouldBe` 3
            case tvdDrives details of
              (d : _) -> tvdiDiskImageName d `shouldBe` baseDiskName
              [] -> fail "Expected at least one drive in template"
            length (tvdSshKeys details) `shouldBe` 1
          other -> fail $ "Template show failed: " ++ show other

        -- 4. Instantiate template
        resInst <- withDaemonConnection daemon $ \conn -> templateInstantiate conn templateId "instantiated-vm"
        newVmId <- case resInst of
          Right (Right (TemplateInstantiated vmId)) -> pure vmId
          other -> fail $ "Template instantiation failed: " ++ show other

        -- 5. Verify instantiated VM
        resVm <- withDaemonConnection daemon $ \conn -> showVm conn newVmId
        case resVm of
          Right (Right (Just details)) -> do
            vdName details `shouldBe` "instantiated-vm"
            vdCpuCount details `shouldBe` 2
            vdRamMb details `shouldBe` 2048
            vdDescription details `shouldBe` Just "A test template"
            -- Verify we have 4 drives: the overlay, OVMF CODE, OVMF VARS clone, and the cloud-init ISO
            length (vdDrives details) `shouldBe` 4
          other -> fail $ "VM show failed: " ++ show other

        -- Verify SSH keys are attached
        resSshKeys <- withDaemonConnection daemon $ \conn -> sshKeyListForVm conn newVmId
        case resSshKeys of
          Right (Right (SshKeyListResult keys)) -> do
            length keys `shouldBe` 1
          other -> fail $ "SSH key list failed: " ++ show other

        -- 6. Start the VM and verify SSH access with the key
        sshPort <- findFreePort
        let hostFwd = "hostfwd=tcp::" <> T.pack (show sshPort) <> "-:22"
        addVmNetIf daemon newVmId NetUser hostFwd Nothing
        resStart <- withDaemonConnection daemon $ \conn -> vmStart conn newVmId
        case resStart of
          Right (Right (VmActionSuccess _)) -> pure ()
          other -> fail $ "VM start failed: " ++ show other

        -- Wait for SSH to be available and authenticate with the key
        putStrLn $ "[test] Waiting for SSH on port " ++ show sshPort ++ " with key authentication"
        waitForDaemonVmSshWithKey "127.0.0.1" sshPort privateKey "corvus" 90

        -- Verify we can run commands via SSH
        let testVm =
              DaemonVm
                { dvmId = newVmId
                , dvmDiskId = 0 -- Not needed for SSH operations
                , dvmSshHost = "127.0.0.1"
                , dvmSshPort = sshPort
                , dvmDaemon = daemon
                , dvmSshPrivateKey = privateKey
                , dvmSshKeyId = 0 -- Not needed for SSH operations
                , dvmSshUser = "corvus"
                }

        (exitCode, stdout, _) <- runInDaemonVm testVm "whoami"
        exitCode `shouldBe` ExitSuccess
        T.strip stdout `shouldBe` "corvus"

        -- Verify the SSH key is actually installed in authorized_keys
        (exitCode2, stdout2, _) <- runInDaemonVm testVm "grep -c 'ssh-' ~/.ssh/authorized_keys"
        exitCode2 `shouldBe` ExitSuccess
        read (T.unpack $ T.strip stdout2) `shouldSatisfy` (> (0 :: Int))

        -- Stop the instantiated VM
        stopDaemonVmAndWait daemon newVmId 10

        -- Cleanup: delete template
        void $ withDaemonConnection daemon $ \conn -> templateDelete conn templateId
