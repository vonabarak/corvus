{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the 'Corvus.Schema.Build' YAML schema.
--
-- These exercise parsing of @builds:@ pipelines without touching a
-- daemon: they verify that defaults fire, polymorphic forms (e.g.
-- @shell:@ as a string vs an object) work, and obviously-wrong shapes
-- are rejected.
module Corvus.BuildSchemaSpec (spec) where

import Corvus.Handlers.Build (buildShellCommand)
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
          buildBootKeys b `shouldBe` []
          buildWaitForShutdownSec b `shouldBe` 3600
        Left e -> expectationFailure e

    it "parses installer strategy with bootKeys + waitForShutdownSec" $ do
      let yaml =
            BS8.unlines
              [ "builds:"
              , "  - name: win"
              , "    template: tpl"
              , "    target: { name: out }"
              , "    strategy: installer"
              , "    waitForShutdownSec: 1800"
              , "    bootKeys:"
              , "      - keys: ret"
              , "        delaySec: 3"
              , "        repeat: 5"
              , "        intervalSec: 1"
              , "      - keys: esc"
              ]
      case decodeBuilds yaml of
        Right c -> do
          let [b] = bcBuilds c
          buildStrategy b `shouldBe` BuildStrategyInstaller
          buildWaitForShutdownSec b `shouldBe` 1800
          map bkKeys (buildBootKeys b) `shouldBe` ["ret", "esc"]
          map bkDelaySec (buildBootKeys b) `shouldBe` [3, 0]
          map bkRepeat (buildBootKeys b) `shouldBe` [5, 1]
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

  describe "Floppy" $ do
    it "parses the post-preprocess form (contentBase64 + filename)" $ do
      let yaml =
            BS8.unlines
              [ "builds:"
              , "  - name: x"
              , "    template: t"
              , "    target: { name: o }"
              , "    strategy: installer"
              , "    floppy:"
              , "      contentBase64: ZXhhbXBsZQo="
              , "      filename: autounattend.xml"
              ]
      case decodeBuilds yaml of
        Right c -> do
          let [b] = bcBuilds c
          case buildFloppy b of
            Just f -> do
              floppyContentBase64 f `shouldBe` Just "ZXhhbXBsZQo="
              floppyFilename f `shouldBe` Just "autounattend.xml"
              floppyFrom f `shouldBe` Nothing
            Nothing -> expectationFailure "floppy parsed as Nothing"
        Left e -> expectationFailure e

    it "parses target.path" $ do
      let yaml =
            BS8.unlines
              [ "builds:"
              , "  - name: x"
              , "    template: t"
              , "    target:"
              , "      name: o"
              , "      path: alpine-test/"
              ]
      case decodeBuilds yaml of
        Right c -> do
          let [b] = bcBuilds c
          btPath (buildTarget b) `shouldBe` Just "alpine-test/"
        Left e -> expectationFailure e

    it "leaves buildFloppy=Nothing when no floppy: key is present" $ do
      let yaml =
            BS8.unlines
              [ "builds:"
              , "  - name: x"
              , "    template: t"
              , "    target: { name: o }"
              ]
      case decodeBuilds yaml of
        Right c -> buildFloppy (head (bcBuilds c)) `shouldBe` Nothing
        Left e -> expectationFailure e

  describe "ShellDefaults" $ do
    let parseSd yaml = case decodeBuilds yaml of
          Right c -> Right (buildShellDefaults (head (bcBuilds c)))
          Left e -> Left e
        wrap inner =
          BS8.unlines
            ( [ "builds:"
              , "  - name: x"
              , "    template: t"
              , "    target: { name: o }"
              ]
                ++ inner
            )

    it "defaults to empty when shellDefaults is absent" $ do
      case parseSd (wrap []) of
        Right sd -> do
          sdPreamble sd `shouldBe` Nothing
          sdEnv sd `shouldBe` []
        Left e -> expectationFailure e

    it "parses shellDefaults with only preamble" $ do
      let yaml =
            wrap
              [ "    shellDefaults:"
              , "      preamble: |"
              , "        set -eux"
              ]
      case parseSd yaml of
        Right sd -> do
          sdPreamble sd `shouldBe` Just "set -eux\n"
          sdEnv sd `shouldBe` []
        Left e -> expectationFailure e

    it "parses shellDefaults with only env" $ do
      let yaml =
            wrap
              [ "    shellDefaults:"
              , "      env:"
              , "        SYSROOT: /mnt/sysroot"
              ]
      case parseSd yaml of
        Right sd -> do
          sdPreamble sd `shouldBe` Nothing
          sdEnv sd `shouldBe` [("SYSROOT", "/mnt/sysroot")]
        Left e -> expectationFailure e

    it "parses shellDefaults with both preamble and env" $ do
      let yaml =
            wrap
              [ "    shellDefaults:"
              , "      preamble: \"set -eux\""
              , "      env:"
              , "        SYSROOT: /mnt/sysroot"
              ]
      case parseSd yaml of
        Right sd -> do
          sdPreamble sd `shouldBe` Just "set -eux"
          sdEnv sd `shouldBe` [("SYSROOT", "/mnt/sysroot")]
        Left e -> expectationFailure e

    it "rejects non-string env values" $
      decodeBuilds
        ( wrap
            [ "    shellDefaults:"
            , "      env:"
            , "        FOO: 1"
            ]
        )
        `shouldSatisfy` \case
          Left _ -> True
          _ -> False

  describe "buildShellCommand" $ do
    let bareShell =
          Shell
            { shellInline = Just "true"
            , shellScript = Nothing
            , shellWorkdir = Nothing
            , shellEnv = []
            , shellTimeoutSec = Nothing
            }

    it "returns the body unchanged when defaults + step are empty" $
      buildShellCommand emptyShellDefaults bareShell "echo hi"
        `shouldBe` "echo hi"

    it "prepends sdPreamble before the body" $
      buildShellCommand
        emptyShellDefaults {sdPreamble = Just "set -eux"}
        bareShell
        "echo hi"
        `shouldBe` "set -eux\necho hi"

    it "exports sdEnv before the body" $
      buildShellCommand
        emptyShellDefaults {sdEnv = [("SYSROOT", "/mnt/sysroot")]}
        bareShell
        "echo hi"
        `shouldBe` "export SYSROOT='/mnt/sysroot'\necho hi"

    it "orders preamble before sdEnv before stepEnv before workdir before body" $
      let sd =
            ShellDefaults
              { sdPreamble = Just "set -eux"
              , sdEnv = [("SHARED", "1")]
              }
          sh = bareShell {shellEnv = [("STEP", "2")], shellWorkdir = Just "/tmp"}
       in buildShellCommand sd sh "echo hi"
            `shouldBe` "set -eux\nexport SHARED='1'\nexport STEP='2'\ncd '/tmp'\necho hi"

    it "lets the step override a defaults env key (later export wins in shell)" $
      let sd = emptyShellDefaults {sdEnv = [("X", "default")]}
          sh = bareShell {shellEnv = [("X", "step")]}
       in buildShellCommand sd sh "echo $X"
            `shouldBe` "export X='default'\nexport X='step'\necho $X"

    it "single-quotes env values that contain spaces" $
      buildShellCommand
        emptyShellDefaults {sdEnv = [("MSG", "hello world")]}
        bareShell
        "echo $MSG"
        `shouldBe` "export MSG='hello world'\necho $MSG"
