{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the 'Corvus.Schema.Build' YAML schema.
--
-- These exercise parsing of @builds:@ pipelines without touching a
-- daemon: they verify that defaults fire, polymorphic forms (e.g.
-- @shell:@ as a string vs an object) work, and obviously-wrong shapes
-- are rejected.
module Corvus.BuildSchemaSpec (spec) where

import Corvus.Model (DriveFormat (..))
import Corvus.Schema.Build
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Yaml as Yaml
import Test.Hspec

decodeBuilds :: BS8.ByteString -> Either String BuildConfig
decodeBuilds bs = case Yaml.decodeEither' bs of
  Left e -> Left (show e)
  Right v -> Right v

spec :: Spec
spec = describe "Schema.Build" $ do
  describe "BuildConfig" $ do
    it "accepts an empty document" $
      decodeBuilds "{}" `shouldSatisfy` \case
        Right c -> null (bcBuilds c)
        _ -> False

    it "rejects a document with non-list builds" $
      decodeBuilds "builds: notalist" `shouldSatisfy` \case
        Left _ -> True
        _ -> False

  describe "Build" $ do
    it "applies defaults to optional fields" $ do
      let yaml =
            BS8.unlines
              [ "builds:"
              , "  - name: x"
              , "    template: tpl"
              , "    target: { name: out }"
              ]
      case decodeBuilds yaml of
        Right c -> do
          let [b] = bcBuilds c
          buildName b `shouldBe` "x"
          buildTemplate b `shouldBe` "tpl"
          btName (buildTarget b) `shouldBe` "out"
          btFormat (buildTarget b) `shouldBe` FormatQcow2
          btCompact (buildTarget b) `shouldBe` True
          buildStrategy b `shouldBe` BuildStrategyOverlay
          buildCleanup b `shouldBe` CleanupAlways
          bvmCpuCount (buildVm b) `shouldBe` 4
          bvmRamMb (buildVm b) `shouldBe` 4096
        Left e -> expectationFailure e

    it "rejects unknown strategy" $
      decodeBuilds
        ( BS8.unlines
            [ "builds:"
            , "  - name: x"
            , "    template: tpl"
            , "    target: { name: out }"
            , "    strategy: maybe"
            ]
        )
        `shouldSatisfy` \case
          Left _ -> True
          _ -> False

    it "rejects missing template" $
      decodeBuilds
        ( BS8.unlines
            [ "builds:"
            , "  - name: x"
            , "    target: { name: out }"
            ]
        )
        `shouldSatisfy` \case
          Left _ -> True
          _ -> False

  describe "Provisioner: shell" $ do
    it "accepts shell as a bare string (sets inline)" $ do
      let yaml =
            BS8.unlines
              [ "builds:"
              , "  - name: x"
              , "    template: t"
              , "    target: { name: o }"
              , "    provisioners:"
              , "      - shell: echo hi"
              ]
      case decodeBuilds yaml of
        Right c -> do
          let [b] = bcBuilds c
          case buildProvisioners b of
            [ProvShell sh] -> do
              shellInline sh `shouldBe` Just "echo hi"
              shellScript sh `shouldBe` Nothing
            other -> expectationFailure $ "unexpected: " ++ show other
        Left e -> expectationFailure e

    it "accepts shell as an object with inline + env + workdir + timeout" $ do
      let yaml =
            BS8.unlines
              [ "builds:"
              , "  - name: x"
              , "    template: t"
              , "    target: { name: o }"
              , "    provisioners:"
              , "      - shell:"
              , "          inline: \"echo hi\""
              , "          workdir: /tmp"
              , "          env: { FOO: bar }"
              , "          timeoutSec: 60"
              ]
      case decodeBuilds yaml of
        Right c -> do
          let [b] = bcBuilds c
          case buildProvisioners b of
            [ProvShell sh] -> do
              shellInline sh `shouldBe` Just "echo hi"
              shellWorkdir sh `shouldBe` Just "/tmp"
              shellTimeoutSec sh `shouldBe` Just 60
              shellEnv sh `shouldBe` [("FOO", "bar")]
            other -> expectationFailure $ "unexpected: " ++ show other
        Left e -> expectationFailure e

  describe "Provisioner: file" $ do
    it "accepts content (post-client preprocess form)" $ do
      let yaml =
            BS8.unlines
              [ "builds:"
              , "  - name: x"
              , "    template: t"
              , "    target: { name: o }"
              , "    provisioners:"
              , "      - file:"
              , "          content: ZXhhbXBsZQo="
              , "          to: /etc/foo"
              ]
      case decodeBuilds yaml of
        Right c -> do
          let [b] = bcBuilds c
          case buildProvisioners b of
            [ProvFile fp] -> do
              fileContentBase64 fp `shouldBe` Just "ZXhhbXBsZQo="
              fileTo fp `shouldBe` "/etc/foo"
            other -> expectationFailure $ "unexpected: " ++ show other
        Left e -> expectationFailure e

  describe "Provisioner: wait-for" $ do
    it "parses wait-for: { ping }" $ do
      let yaml =
            BS8.unlines
              [ "builds:"
              , "  - name: x"
              , "    template: t"
              , "    target: { name: o }"
              , "    provisioners:"
              , "      - wait-for: { ping: true, timeoutSec: 30 }"
              ]
      case decodeBuilds yaml of
        Right c -> do
          let [b] = bcBuilds c
          case buildProvisioners b of
            [ProvWaitFor (WaitForPing 30)] -> pure ()
            other -> expectationFailure $ "unexpected: " ++ show other
        Left e -> expectationFailure e

    it "rejects wait-for with multiple keys" $
      decodeBuilds
        ( BS8.unlines
            [ "builds:"
            , "  - name: x"
            , "    template: t"
            , "    target: { name: o }"
            , "    provisioners:"
            , "      - wait-for: { ping: true, file: /tmp/x }"
            ]
        )
        `shouldSatisfy` \case
          Left _ -> True
          _ -> False

  describe "Provisioner: dispatch" $ do
    it "rejects a provisioner with two kinds set" $
      decodeBuilds
        ( BS8.unlines
            [ "builds:"
            , "  - name: x"
            , "    template: t"
            , "    target: { name: o }"
            , "    provisioners:"
            , "      - shell: \"echo\""
            , "        file: { content: \"\", to: /x }"
            ]
        )
        `shouldSatisfy` \case
          Left _ -> True
          _ -> False

    it "rejects a provisioner with no kinds" $
      decodeBuilds
        ( BS8.unlines
            [ "builds:"
            , "  - name: x"
            , "    template: t"
            , "    target: { name: o }"
            , "    provisioners:"
            , "      - {}"
            ]
        )
        `shouldSatisfy` \case
          Left _ -> True
          _ -> False
