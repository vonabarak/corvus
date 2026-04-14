{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Corvus.TemplateSpec (spec) where

import Corvus.Client.Rpc (TemplateResult (..))
import Corvus.Model (Key, TemplateCloneStrategy (..), TemplateCloudInit (..), Unique (..), Vm (..))
import Corvus.Protocol (CloudInitInfo (..), TemplateDetails (..), TemplateDriveInfo (..))
import Corvus.Utils.Yaml (yaml)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Database.Persist (Entity (..), get, getBy)
import Database.Persist.Sql (toSqlKey)
import Test.DSL.Core (runDb)
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "template create" $ do
    testCase "creates template from valid YAML" $ do
      _ <- given $ insertDiskImage "base-disk" "base.qcow2" FormatQcow2
      let tplYaml =
            [yaml|
              name: test-template
              cpuCount: 2
              ramMb: 2048
              drives:
                - diskImageName: base-disk
                  interface: virtio
                  strategy: overlay
            |]
      result <- whenTemplateCreate tplYaml
      liftIO $ case result of
        TemplateCreated tId -> tId `shouldSatisfy` (> 0)
        other -> fail $ "Expected TemplateCreated, got: " ++ show other

    testCase "rejects invalid YAML" $ do
      result <- whenTemplateCreate "not: valid: yaml: [["
      liftIO $ case result of
        TemplateError _ -> pure ()
        other -> fail $ "Expected TemplateError, got: " ++ show other

    testCase "rejects template with missing required fields" $ do
      result <- whenTemplateCreate "name: incomplete-template\n"
      liftIO $ case result of
        TemplateError _ -> pure ()
        other -> fail $ "Expected TemplateError, got: " ++ show other

    testCase "creates template with create-strategy drive" $ do
      let tplYaml =
            [yaml|
              name: create-strategy-tpl
              cpuCount: 1
              ramMb: 512
              drives:
                - interface: virtio
                  strategy: create
                  format: qcow2
                  sizeMb: 10240
            |]
      result <- whenTemplateCreate tplYaml
      liftIO $ case result of
        TemplateCreated tId -> tId `shouldSatisfy` (> 0)
        other -> fail $ "Expected TemplateCreated, got: " ++ show other

    testCase "rejects create-strategy drive without format" $ do
      let tplYaml =
            [yaml|
              name: bad-create-tpl
              cpuCount: 1
              ramMb: 512
              drives:
                - interface: virtio
                  strategy: create
                  sizeMb: 10240
            |]
      result <- whenTemplateCreate tplYaml
      liftIO $ case result of
        TemplateError msg -> T.isInfixOf "format" msg `shouldBe` True
        other -> fail $ "Expected TemplateError, got: " ++ show other

    testCase "rejects create-strategy drive without sizeMb" $ do
      let tplYaml =
            [yaml|
              name: bad-create-tpl2
              cpuCount: 1
              ramMb: 512
              drives:
                - interface: virtio
                  strategy: create
                  format: qcow2
            |]
      result <- whenTemplateCreate tplYaml
      liftIO $ case result of
        TemplateError msg -> T.isInfixOf "sizeMb" msg `shouldBe` True
        other -> fail $ "Expected TemplateError, got: " ++ show other

    testCase "rejects duplicate template name" $ do
      _ <- given $ insertDiskImage "base-disk" "base.qcow2" FormatQcow2
      let tplYaml =
            [yaml|
              name: dup-template
              cpuCount: 2
              ramMb: 2048
              drives:
                - diskImageName: base-disk
                  interface: virtio
                  strategy: overlay
            |]
      result1 <- whenTemplateCreate tplYaml
      liftIO $ case result1 of
        TemplateCreated _ -> pure ()
        other -> fail $ "First create should succeed: " ++ show other
      result2 <- whenTemplateCreate tplYaml
      liftIO $ case result2 of
        TemplateError _ -> pure ()
        other -> fail $ "Expected TemplateError for duplicate, got: " ++ show other

  describe "template list" $ do
    testCase "returns empty list when no templates exist" $ do
      result <- whenTemplateList
      liftIO $ case result of
        TemplateListResult [] -> pure ()
        other -> fail $ "Expected empty list, got: " ++ show other

  describe "template show" $ do
    testCase "returns not found for non-existent template" $ do
      result <- whenTemplateShow 999
      liftIO $ result `shouldBe` TemplateNotFound

  describe "template with cloud-init config" $ do
    testCase "creates template with cloudInitConfig" $ do
      _ <- given $ insertDiskImage "ci-base-disk" "ci-base.qcow2" FormatQcow2
      let tplYaml =
            [yaml|
              name: ci-template
              cpuCount: 2
              ramMb: 2048
              cloudInit: true
              cloudInitConfig:
                userData:
                  users:
                    - name: deploy
                      sudo: "ALL=(ALL) NOPASSWD:ALL"
                  packages:
                    - nginx
                injectSshKeys: false
              drives:
                - diskImageName: ci-base-disk
                  interface: virtio
                  strategy: overlay
            |]
      result <- whenTemplateCreate tplYaml
      case result of
        TemplateCreated tId -> do
          -- Verify cloud-init config stored in DB
          mCi <- runDb $ getBy (UniqueTemplateCloudInitVm (toSqlKey tId))
          liftIO $ mCi `shouldSatisfy` isJust
          case mCi of
            Just (Entity _ tci) -> do
              liftIO $ templateCloudInitUserData tci `shouldSatisfy` isJust
              liftIO $ templateCloudInitInjectSshKeys tci `shouldBe` False
            Nothing -> liftIO $ expectationFailure "TemplateCloudInit row should exist"
          -- Verify template show includes cloud-init config
          showResult <- whenTemplateShow tId
          liftIO $ case showResult of
            TemplateDetailsResult details -> tvdCloudInitConfig details `shouldSatisfy` isJust
            other -> fail $ "Expected TemplateDetailsResult, got: " ++ show other
        other -> liftIO $ fail $ "Expected TemplateCreated, got: " ++ show other

    testCase "creates template with raw string cloudInit userData" $ do
      _ <- given $ insertDiskImage "raw-ci-base" "raw-ci-base.qcow2" FormatQcow2
      let tplYaml =
            [yaml|
              name: raw-ci-template
              cpuCount: 2
              ramMb: 2048
              cloudInit: true
              cloudInitConfig:
                userData: |
                  #ps1_sysnative
                  net user Administrator "WinPass123!" /y
                  Start-Service QEMU-GA
                injectSshKeys: false
              drives:
                - diskImageName: raw-ci-base
                  interface: virtio
                  strategy: overlay
            |]
      result <- whenTemplateCreate tplYaml
      case result of
        TemplateCreated tId -> do
          mCi <- runDb $ getBy (UniqueTemplateCloudInitVm (toSqlKey tId))
          liftIO $ mCi `shouldSatisfy` isJust
          case mCi of
            Just (Entity _ tci) -> do
              liftIO $ templateCloudInitUserData tci `shouldSatisfy` isJust
              case templateCloudInitUserData tci of
                Just ud -> liftIO $ T.isPrefixOf "#ps1_sysnative" ud `shouldBe` True
                Nothing -> liftIO $ expectationFailure "userData should be Just"
              liftIO $ templateCloudInitInjectSshKeys tci `shouldBe` False
            Nothing -> liftIO $ expectationFailure "TemplateCloudInit row should exist"
        other -> liftIO $ fail $ "Expected TemplateCreated, got: " ++ show other

  describe "template delete" $ do
    testCase "fails for non-existent template" $ do
      result <- whenTemplateDelete 999
      liftIO $ result `shouldBe` TemplateNotFound

  describe "template update" $ do
    testCase "replaces an existing template's fields" $ do
      _ <- given $ insertDiskImage "upd-disk" "upd.qcow2" FormatQcow2
      let initialYaml =
            [yaml|
              name: upd-tpl
              cpuCount: 1
              ramMb: 1024
              drives:
                - diskImageName: upd-disk
                  interface: virtio
                  strategy: overlay
            |]
      createRes <- whenTemplateCreate initialYaml
      tId <- case createRes of
        TemplateCreated i -> pure i
        other -> liftIO $ fail $ "Expected TemplateCreated, got: " ++ show other
      let updatedYaml =
            [yaml|
              name: upd-tpl
              cpuCount: 4
              ramMb: 4096
              drives:
                - diskImageName: upd-disk
                  interface: virtio
                  strategy: overlay
            |]
      updRes <- whenTemplateUpdate tId updatedYaml
      newId <- case updRes of
        TemplateUpdated i -> pure i
        other -> liftIO $ fail $ "Expected TemplateUpdated, got: " ++ show other
      showRes <- whenTemplateShow newId
      liftIO $ case showRes of
        TemplateDetailsResult d -> do
          tvdCpuCount d `shouldBe` 4
          tvdRamMb d `shouldBe` 4096
        other -> fail $ "Expected TemplateDetailsResult, got: " ++ show other

    testCase "rolls back when the new name already exists" $ do
      _ <- given $ insertDiskImage "conflict-disk" "c.qcow2" FormatQcow2
      let tplA =
            [yaml|
              name: tpl-a
              cpuCount: 1
              ramMb: 512
              drives:
                - diskImageName: conflict-disk
                  interface: virtio
                  strategy: overlay
            |]
      let tplB =
            [yaml|
              name: tpl-b
              cpuCount: 1
              ramMb: 512
              drives:
                - diskImageName: conflict-disk
                  interface: virtio
                  strategy: overlay
            |]
      _ <- whenTemplateCreate tplA
      bRes <- whenTemplateCreate tplB
      bId <- case bRes of
        TemplateCreated i -> pure i
        other -> liftIO $ fail $ "Expected TemplateCreated, got: " ++ show other
      -- Try to rename tpl-b to tpl-a — should fail and leave both intact.
      let collisionYaml =
            [yaml|
              name: tpl-a
              cpuCount: 2
              ramMb: 2048
              drives:
                - diskImageName: conflict-disk
                  interface: virtio
                  strategy: overlay
            |]
      updRes <- whenTemplateUpdate bId collisionYaml
      liftIO $ case updRes of
        TemplateError _ -> pure ()
        other -> fail $ "Expected TemplateError, got: " ++ show other
      -- tpl-b should still exist with ramMb 512.
      showRes <- whenTemplateShow bId
      liftIO $ case showRes of
        TemplateDetailsResult d -> tvdRamMb d `shouldBe` 512
        other -> fail $ "Expected TemplateDetailsResult, got: " ++ show other

    testCase "fails for non-existent template" $ do
      let y =
            [yaml|
              name: doesnt-matter
              cpuCount: 1
              ramMb: 512
              drives: []
            |]
      result <- whenTemplateUpdate 9999 y
      liftIO $ result `shouldBe` TemplateNotFound

  describe "template create strategy" $ do
    testCase "stores format and sizeMb for create-strategy drive" $ do
      let tplYaml =
            [yaml|
              name: create-show-tpl
              cpuCount: 1
              ramMb: 512
              drives:
                - diskImageName: data
                  interface: virtio
                  strategy: create
                  format: qcow2
                  sizeMb: 2048
            |]
      createRes <- whenTemplateCreate tplYaml
      tId <- case createRes of
        TemplateCreated i -> pure i
        other -> liftIO $ fail $ "Expected TemplateCreated, got: " ++ show other
      showRes <- whenTemplateShow tId
      liftIO $ case showRes of
        TemplateDetailsResult d -> do
          length (tvdDrives d) `shouldBe` 1
          let [drv] = tvdDrives d
          tvdiCloneStrategy drv `shouldBe` StrategyCreate
          tvdiFormat drv `shouldBe` Just FormatQcow2
          tvdiSizeMb drv `shouldBe` Just 2048
          tvdiDiskImageId drv `shouldBe` Nothing
          tvdiDiskImageName drv `shouldBe` Just "data"
        other -> fail $ "Expected TemplateDetailsResult, got: " ++ show other

    testCase "stores drive without diskImageName for create strategy" $ do
      let tplYaml =
            [yaml|
              name: create-noname-tpl
              cpuCount: 1
              ramMb: 256
              drives:
                - interface: virtio
                  strategy: create
                  format: raw
                  sizeMb: 1024
            |]
      createRes <- whenTemplateCreate tplYaml
      tId <- case createRes of
        TemplateCreated i -> pure i
        other -> liftIO $ fail $ "Expected TemplateCreated, got: " ++ show other
      showRes <- whenTemplateShow tId
      liftIO $ case showRes of
        TemplateDetailsResult d -> do
          let [drv] = tvdDrives d
          tvdiDiskImageName drv `shouldBe` Nothing
          tvdiFormat drv `shouldBe` Just FormatRaw
          tvdiSizeMb drv `shouldBe` Just 1024
        other -> fail $ "Expected TemplateDetailsResult, got: " ++ show other

    testCase "stores mixed create and overlay strategies in one template" $ do
      _ <- given $ insertDiskImage "base" "base.qcow2" FormatQcow2
      let tplYaml =
            [yaml|
              name: mixed-strat-tpl
              cpuCount: 2
              ramMb: 1024
              drives:
                - diskImageName: base
                  interface: virtio
                  strategy: overlay
                - diskImageName: extra-data
                  interface: virtio
                  strategy: create
                  format: qcow2
                  sizeMb: 5120
            |]
      createRes <- whenTemplateCreate tplYaml
      tId <- case createRes of
        TemplateCreated i -> pure i
        other -> liftIO $ fail $ "Expected TemplateCreated, got: " ++ show other
      showRes <- whenTemplateShow tId
      liftIO $ case showRes of
        TemplateDetailsResult d -> do
          length (tvdDrives d) `shouldBe` 2
          let [overlayDrv, createDrv] = tvdDrives d
          -- Overlay drive has a backing disk
          tvdiCloneStrategy overlayDrv `shouldBe` StrategyOverlay
          tvdiDiskImageName overlayDrv `shouldBe` Just "base"
          tvdiDiskImageId overlayDrv `shouldSatisfy` isJust
          -- Create drive has no backing disk but has format/size
          tvdiCloneStrategy createDrv `shouldBe` StrategyCreate
          tvdiDiskImageName createDrv `shouldBe` Just "extra-data"
          tvdiDiskImageId createDrv `shouldBe` Nothing
          tvdiFormat createDrv `shouldBe` Just FormatQcow2
          tvdiSizeMb createDrv `shouldBe` Just 5120
        other -> fail $ "Expected TemplateDetailsResult, got: " ++ show other

  describe "template instantiate field inheritance" $ do
    testCase "instantiated VM inherits all template fields" $ do
      let tplYaml =
            [yaml|
              name: inherit-all-tpl
              cpuCount: 8
              ramMb: 16384
              description: "full inheritance test"
              headless: true
              cloudInit: true
              guestAgent: true
              autostart: true
              drives:
                - interface: virtio
                  strategy: create
                  format: qcow2
                  sizeMb: 1024
            |]
      createRes <- whenTemplateCreate tplYaml
      tId <- case createRes of
        TemplateCreated i -> pure i
        other -> liftIO $ fail $ "Expected TemplateCreated, got: " ++ show other
      instRes <- whenTemplateInstantiate tId "inherit-vm"
      vmId <- case instRes of
        TemplateInstantiated i -> pure i
        other -> liftIO $ fail $ "Expected TemplateInstantiated, got: " ++ show other
      mVm <- runDb $ get (toSqlKey vmId :: Key Vm)
      liftIO $ case mVm of
        Nothing -> fail "Instantiated VM not found in DB"
        Just vm -> do
          vmCpuCount vm `shouldBe` 8
          vmRamMb vm `shouldBe` 16384
          vmDescription vm `shouldBe` Just "full inheritance test"
          vmHeadless vm `shouldBe` True
          vmCloudInit vm `shouldBe` True
          vmGuestAgent vm `shouldBe` True
          vmAutostart vm `shouldBe` True

    testCase "instantiated VM defaults guestAgent and autostart to false" $ do
      let tplYaml =
            [yaml|
              name: defaults-tpl
              cpuCount: 1
              ramMb: 512
              drives:
                - interface: virtio
                  strategy: create
                  format: qcow2
                  sizeMb: 512
            |]
      createRes <- whenTemplateCreate tplYaml
      tId <- case createRes of
        TemplateCreated i -> pure i
        other -> liftIO $ fail $ "Expected TemplateCreated, got: " ++ show other
      instRes <- whenTemplateInstantiate tId "defaults-vm"
      vmId <- case instRes of
        TemplateInstantiated i -> pure i
        other -> liftIO $ fail $ "Expected TemplateInstantiated, got: " ++ show other
      mVm <- runDb $ get (toSqlKey vmId :: Key Vm)
      liftIO $ case mVm of
        Nothing -> fail "Instantiated VM not found in DB"
        Just vm -> do
          vmGuestAgent vm `shouldBe` False
          vmAutostart vm `shouldBe` False
          vmHeadless vm `shouldBe` False
          vmCloudInit vm `shouldBe` False
