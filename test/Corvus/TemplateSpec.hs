{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Corvus.TemplateSpec (spec) where

import Corvus.Client.Rpc (TemplateResult (..))
import Corvus.Model (TemplateCloudInit (..), Unique (..))
import Corvus.Protocol (CloudInitInfo (..), TemplateDetails (..))
import Corvus.Utils.Yaml (yaml)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Database.Persist (Entity (..), getBy)
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
