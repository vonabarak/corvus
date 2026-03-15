{-# LANGUAGE OverloadedStrings #-}

module Corvus.TemplateIntegrationSpec (spec) where

import Control.Monad (void)
import Corvus.Client
import Corvus.Protocol
import Data.List (find)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Test.DSL.Daemon
import Test.Daemon (withDaemonConnection)
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (withTestVm)

spec :: Spec
spec = withTestDb $ do
  describe "VM Template integration" $ do
    it "can create a template from YAML and instantiate it" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        let daemon = dvmDaemon vm
            sshKeyName = "template-key"
        
        -- Create an SSH key to refer to in the template
        void $ withDaemonConnection daemon $ \conn -> sshKeyCreate conn sshKeyName "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC..."
        
        -- Stop the VM so we can use its disk for cloning/overlay strategy
        -- withTestVm registers base image as "base-image"
        stopDaemonVmAndWait daemon (dvmId vm) 10
        
        let templateYaml = T.unlines
              [ "name: \"test-template\""
              , "cpuCount: 2"
              , "ramMb: 512"
              , "description: \"A test template\""
              , "drives:"
              , "  - diskImageName: \"base-image\""
              , "    interface: \"virtio\""
              , "    strategy: \"overlay\""
              , "    newSizeMb: 1024"
              , "networkInterfaces:"
              , "  - type: \"user\""
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
            length (tvdDrives details) `shouldBe` 1
            tvdiDiskImageName (head (tvdDrives details)) `shouldBe` "base-image"
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
            vdRamMb details `shouldBe` 512
            length (vdDrives details) `shouldBe` 1
            vdDescription details `shouldBe` Just "A test template"
          other -> fail $ "VM show failed: " ++ show other

        -- Cleanup: delete template
        void $ withDaemonConnection daemon $ \conn -> templateDelete conn templateId
