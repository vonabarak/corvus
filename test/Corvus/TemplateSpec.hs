{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Corvus.TemplateSpec (spec) where

import Corvus.Client.Rpc (TemplateResult (..))
import Corvus.Utils.Yaml (yaml)
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

  describe "template delete" $ do
    testCase "fails for non-existent template" $ do
      result <- whenTemplateDelete 999
      liftIO $ result `shouldBe` TemplateNotFound
