{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Corvus.CloudInit.renderUserData' — the pure function
-- that decides what to write to @user-data@ inside a NoCloud ISO.
--
-- This distinguishes raw scripts (shebang, @#ps1_sysnative@, @#include@,
-- already-@#cloud-config@) from structured YAML we need to wrap, and
-- also handles the SSH-key injection path. Historically this logic was
-- covered only by the slow multi-OS 'CloudInitIntegrationSpec'; a
-- regression in the raw-script detection broke Windows VMs silently
-- until someone booted one.
module Corvus.CloudInitSpec (spec) where

import Corvus.CloudInit
import qualified Data.Text as T
import Test.Hspec

-- | Minimal config; tests override fields as needed.
baseConfig :: CloudInitConfig
baseConfig = defaultCloudInitConfig

spec :: Spec
spec = do
  describe "isRawUserDataScript" $ do
    it "detects shebang scripts" $
      isRawUserDataScript "#!/bin/bash\necho hi" `shouldBe` True

    it "detects #cloud-config as raw" $
      isRawUserDataScript "#cloud-config\npackages: [vim]" `shouldBe` True

    it "detects #ps1_sysnative (Windows cloudbase-init)" $
      isRawUserDataScript "#ps1_sysnative\nWrite-Host hi" `shouldBe` True

    it "detects #include directive" $
      isRawUserDataScript "#include\nhttps://example.com/u.sh" `shouldBe` True

    it "treats leading-whitespace-then-# as raw" $
      isRawUserDataScript "  \n\t#!/bin/sh" `shouldBe` True

    it "treats plain YAML as structured (not raw)" $
      isRawUserDataScript "packages:\n  - vim\n" `shouldBe` False

    it "treats empty input as structured" $
      isRawUserDataScript "" `shouldBe` False

  describe "renderUserData" $ do
    describe "when ciCustomUserData is Nothing (default path)" $ do
      it "produces content starting with #cloud-config" $ do
        let rendered = renderUserData baseConfig ["ssh-rsa AAAAB3..."]
        T.take 14 rendered `shouldBe` "#cloud-config\n"

      it "includes the supplied SSH key in the rendered YAML" $ do
        let key = "ssh-rsa AAAABBBCCC user@host"
            rendered = renderUserData baseConfig [key]
        rendered `shouldSatisfy` T.isInfixOf key

    describe "when ciCustomUserData is a raw shebang script" $ do
      it "passes the script through verbatim" $ do
        let script = "#!/bin/bash\nset -eu\necho configured"
            config = baseConfig {ciCustomUserData = Just script}
        renderUserData config [] `shouldBe` script

      it "does not prepend #cloud-config" $ do
        let script = "#!/bin/sh\ntrue"
            config = baseConfig {ciCustomUserData = Just script}
        renderUserData config [] `shouldSatisfy` not . T.isPrefixOf "#cloud-config"

      it "does not inject SSH keys into a shebang script even when requested" $ do
        let script = "#!/bin/bash\nexec /usr/bin/agent"
            config = baseConfig {ciCustomUserData = Just script, ciInjectSshKeys = True}
            rendered = renderUserData config ["ssh-rsa AAAA-the-key user@host"]
        rendered `shouldBe` script
        rendered `shouldNotSatisfy` T.isInfixOf "ssh-rsa"

    describe "when ciCustomUserData is #ps1_sysnative (Windows)" $ do
      it "passes the PowerShell script through verbatim" $ do
        let script = "#ps1_sysnative\nNew-LocalUser -Name corvus"
            config = baseConfig {ciCustomUserData = Just script}
        renderUserData config [] `shouldBe` script

      it "preserves the #ps1_sysnative header exactly" $ do
        let script = "#ps1_sysnative\nWrite-Host configured"
            config = baseConfig {ciCustomUserData = Just script}
        T.take 14 (renderUserData config []) `shouldBe` "#ps1_sysnative"

    describe "when ciCustomUserData is already #cloud-config" $ do
      it "returns the input verbatim (no double header)" $ do
        let custom = "#cloud-config\npackages:\n  - vim\n"
            config = baseConfig {ciCustomUserData = Just custom, ciInjectSshKeys = False}
        renderUserData config [] `shouldBe` custom

      it "has exactly one #cloud-config line" $ do
        let custom = "#cloud-config\nwrite_files:\n  - path: /etc/a\n"
            config = baseConfig {ciCustomUserData = Just custom, ciInjectSshKeys = False}
            rendered = renderUserData config []
        length (filter ("#cloud-config" `T.isPrefixOf`) (T.lines rendered)) `shouldBe` 1

    describe "when ciCustomUserData is structured YAML (no raw header)" $ do
      it "prepends #cloud-config" $ do
        let custom = "packages:\n  - curl\n"
            config = baseConfig {ciCustomUserData = Just custom, ciInjectSshKeys = False}
            rendered = renderUserData config []
        T.take 14 rendered `shouldBe` "#cloud-config\n"

      it "preserves the original YAML body after the header" $ do
        let custom = "packages:\n  - curl\n"
            config = baseConfig {ciCustomUserData = Just custom, ciInjectSshKeys = False}
            rendered = renderUserData config []
        rendered `shouldSatisfy` T.isSuffixOf custom

    describe "SSH key injection into structured YAML" $ do
      it "injects the key when ciInjectSshKeys is True and keys are supplied" $ do
        let custom = "packages:\n  - openssh-server\n"
            config = baseConfig {ciCustomUserData = Just custom, ciInjectSshKeys = True}
            rendered = renderUserData config ["ssh-rsa AAA-inject-me user@host"]
        -- Header is present
        T.take 14 rendered `shouldBe` "#cloud-config\n"
        -- Key ended up somewhere in the body
        rendered `shouldSatisfy` T.isInfixOf "AAA-inject-me"

      it "does not inject when ciInjectSshKeys is False" $ do
        let custom = "packages:\n  - openssh-server\n"
            config = baseConfig {ciCustomUserData = Just custom, ciInjectSshKeys = False}
            rendered = renderUserData config ["ssh-rsa AAA-do-not-inject user@host"]
        rendered `shouldNotSatisfy` T.isInfixOf "AAA-do-not-inject"

      it "does not inject when there are no keys to inject" $ do
        let custom = "packages:\n  - openssh-server\n"
            config = baseConfig {ciCustomUserData = Just custom, ciInjectSshKeys = True}
            rendered = renderUserData config []
        -- No keys supplied, so the YAML body is unchanged (just #cloud-config prepended).
        rendered `shouldSatisfy` T.isSuffixOf custom
