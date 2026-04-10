{-# LANGUAGE OverloadedStrings #-}

module Corvus.TemplateIntegrationSpec (spec) where

import Control.Monad (void)
import Corvus.Client
import Corvus.Client.Rpc (CloudInitResult (..))
import Corvus.Protocol
import Data.List (find)
import Data.Maybe (isJust)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (TestVm (..), defaultVmConfig, withTestVm)
import Test.VM.Daemon (withDaemonConnection)
import Test.VM.Rpc (getCloudInitConfig, runViaGuestAgent, startTestVmSync, stopTestVmAndWait)

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
        let daemon = tvmDaemon vm

        -- Stop the VM so we can use its disk for cloning/overlay strategy
        stopTestVmAndWait daemon (tvmId vm) 10

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
        resShow <- withDaemonConnection daemon $ \conn -> templateShow conn (T.pack (show templateId))
        case resShow of
          Right (Right (TemplateDetailsResult details)) -> do
            tvdName details `shouldBe` "test-template"
            length (tvdDrives details) `shouldBe` 3
            case tvdDrives details of
              (d : _) -> tvdiDiskImageName d `shouldBe` baseDiskName
              [] -> fail "Expected at least one drive in template"
          other -> fail $ "Template show failed: " ++ show other

        -- 4. Instantiate template
        resInst <- withDaemonConnection daemon $ \conn -> templateInstantiate conn (T.pack (show templateId)) "instantiated-vm"
        newVmId <- case resInst of
          Right (Right (TemplateInstantiated vmId)) -> pure vmId
          other -> fail $ "Template instantiation failed: " ++ show other

        -- 5. Verify instantiated VM
        resVm <- withDaemonConnection daemon $ \conn -> showVm conn (T.pack (show newVmId))
        case resVm of
          Right (Right (Just details)) -> do
            vdName details `shouldBe` "instantiated-vm"
            vdCpuCount details `shouldBe` 2
            vdRamMb details `shouldBe` 2048
            vdDescription details `shouldBe` Just "A test template"
            -- Verify we have 3 drives: the overlay, OVMF CODE, and OVMF VARS clone
            -- (no cloud-init ISO since cloudInit defaults to false)
            length (vdDrives details) `shouldBe` 3
          other -> fail $ "VM show failed: " ++ show other

        -- 6. Enable guest agent and start the VM
        resEdit <- withDaemonConnection daemon $ \conn -> vmEdit conn (T.pack (show newVmId)) Nothing Nothing Nothing Nothing (Just True) Nothing Nothing
        case resEdit of
          Right (Right VmEdited) -> pure ()
          other -> fail $ "VM edit failed: " ++ show other

        -- Start VM synchronously (blocks until guest agent responds)
        putStrLn "[test] Starting instantiated VM (sync)..."
        startTestVmSync daemon newVmId

        -- Verify we can run commands via guest agent
        (exitCode, stdout, _) <- runViaGuestAgent daemon newVmId "whoami"
        exitCode `shouldBe` ExitSuccess
        T.strip stdout `shouldBe` "root"

        -- Stop the instantiated VM
        stopTestVmAndWait daemon newVmId 10

        -- Cleanup: delete template
        void $ withDaemonConnection daemon $ \conn -> templateDelete conn (T.pack (show templateId))

    it "propagates cloud-init config from template to instantiated VM" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm

        -- Stop the VM so we can use its disk
        stopTestVmAndWait daemon (tvmId vm) 10

        -- Discover actual disk names
        diskListRes <- withDaemonConnection daemon $ \conn -> diskList conn
        disks <- case diskListRes of
          Right (Right (DiskListResult ds)) -> pure ds
          other -> fail $ "Failed to list disks: " ++ show other
        let baseDiskName = findDiskName "base-image" disks
            ovmfCodeName = findDiskName "ovmf-code" disks
            ovmfVarsTemplateName = findDiskName "ovmf-vars-template" disks

        -- Create template with cloud-init config
        let templateYaml =
              T.unlines
                [ "name: \"ci-template\""
                , "cpuCount: 2"
                , "ramMb: 2048"
                , "cloudInit: true"
                , "cloudInitConfig:"
                , "  userData:"
                , "    users:"
                , "      - name: custom-user"
                , "        sudo: \"ALL=(ALL) NOPASSWD:ALL\""
                , "    packages:"
                , "      - curl"
                , "  injectSshKeys: false"
                , "drives:"
                , "  - diskImageName: \"" <> baseDiskName <> "\""
                , "    interface: \"virtio\""
                , "    strategy: \"overlay\""
                , "  - diskImageName: \"" <> ovmfCodeName <> "\""
                , "    interface: \"pflash\""
                , "    strategy: \"direct\""
                , "    readOnly: true"
                , "  - diskImageName: \"" <> ovmfVarsTemplateName <> "\""
                , "    interface: \"pflash\""
                , "    strategy: \"clone\""
                ]

        resCreate <- withDaemonConnection daemon $ \conn -> templateCreate conn templateYaml
        templateId <- case resCreate of
          Right (Right (TemplateCreated tid)) -> pure tid
          other -> fail $ "Template creation failed: " ++ show other

        -- Verify template show includes cloud-init config
        resShow <- withDaemonConnection daemon $ \conn -> templateShow conn (T.pack (show templateId))
        case resShow of
          Right (Right (TemplateDetailsResult details)) -> do
            tvdCloudInit details `shouldBe` True
            tvdCloudInitConfig details `shouldSatisfy` isJust
          other -> fail $ "Template show failed: " ++ show other

        -- Instantiate template
        resInst <- withDaemonConnection daemon $ \conn -> templateInstantiate conn (T.pack (show templateId)) "ci-inst-vm"
        newVmId <- case resInst of
          Right (Right (TemplateInstantiated vmId)) -> pure vmId
          other -> fail $ "Template instantiation failed: " ++ show other

        -- Verify instantiated VM has the cloud-init config
        ciResult <- getCloudInitConfig daemon newVmId
        case ciResult of
          CloudInitConfig (Just ci) -> do
            ciiUserData ci `shouldSatisfy` isJust
            ciiInjectSshKeys ci `shouldBe` False
          other -> fail $ "Expected cloud-init config on instantiated VM, got: " ++ show other

        -- Verify VM details show the config
        resVm <- withDaemonConnection daemon $ \conn -> showVm conn (T.pack (show newVmId))
        case resVm of
          Right (Right (Just details)) -> do
            vdCloudInit details `shouldBe` True
            vdCloudInitConfig details `shouldSatisfy` isJust
          other -> fail $ "VM show failed: " ++ show other

        -- Cleanup
        void $ withDaemonConnection daemon $ \conn -> templateDelete conn (T.pack (show templateId))
