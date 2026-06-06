{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the 'Corvus.Schema.Build' YAML schema.
--
-- These exercise parsing of @pipeline:@ pipelines without touching a
-- daemon: they verify that defaults fire, polymorphic forms (e.g.
-- @shell:@ as a string vs an object) work, and obviously-wrong shapes
-- are rejected.
module Corvus.BuildSchemaSpec (spec) where

import Corvus.Handlers.Build (buildShellCommand)
import Corvus.Model (DriveFormat (..))
import Corvus.Schema.Apply (ApplyConfig (..))
import Corvus.Schema.Build
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Yaml as Yaml
import Test.Hspec

decodePipeline :: BS8.ByteString -> Either String PipelineConfig
decodePipeline bs = case Yaml.decodeEither' bs of
  Left e -> Left (show e)
  Right v -> Right v

-- | Extract the first 'PipelineBuild' from a parsed pipeline, failing
-- the test if the pipeline is empty or its first step is an apply.
firstBuild :: PipelineConfig -> Build
firstBuild cfg = case pcSteps cfg of
  (PipelineBuild b : _) -> b
  (PipelineApply _ : _) -> error "expected a build step, got apply"
  [] -> error "expected a non-empty pipeline"

spec :: Spec
spec = describe "Schema.Build" $ do
  describe "PipelineConfig" $ do
    it "accepts an empty document" $
      decodePipeline "{}" `shouldSatisfy` \case
        Right c -> null (pcSteps c)
        _ -> False

    it "rejects a document with non-list pipeline" $
      decodePipeline "pipeline: notalist" `shouldSatisfy` \case
        Left _ -> True
        _ -> False

    it "parses a build-only pipeline step" $ do
      let yaml =
            BS8.unlines
              [ "pipeline:"
              , "  - build:"
              , "      name: x"
              , "      template: tpl"
              , "      target: {}"
              ]
      case decodePipeline yaml of
        Right c -> case pcSteps c of
          [PipelineBuild b] -> buildName b `shouldBe` "x"
          other -> expectationFailure $ "unexpected: " ++ show other
        Left e -> expectationFailure e

    it "parses an apply-only pipeline step" $ do
      let yaml =
            BS8.unlines
              [ "pipeline:"
              , "  - apply:"
              , "      sshKeys:"
              , "        - name: k"
              , "          publicKey: \"ssh-ed25519 AAAA\""
              ]
      case decodePipeline yaml of
        Right c -> case pcSteps c of
          [PipelineApply ac] -> length (acSshKeys ac) `shouldBe` 1
          other -> expectationFailure $ "unexpected: " ++ show other
        Left e -> expectationFailure e

    it "parses a mixed build → apply → build pipeline" $ do
      let yaml =
            BS8.unlines
              [ "pipeline:"
              , "  - build:"
              , "      name: a"
              , "      template: t"
              , "      target: {}"
              , "  - apply:"
              , "      templates:"
              , "        - name: tpl"
              , "          cpuCount: 1"
              , "          ramMb: 512"
              , "          drives: []"
              , "  - build:"
              , "      name: b"
              , "      template: t"
              , "      target: {}"
              ]
      case decodePipeline yaml of
        Right c -> case pcSteps c of
          [PipelineBuild a, PipelineApply _, PipelineBuild b] -> do
            buildName a `shouldBe` "a"
            buildName b `shouldBe` "b"
          other -> expectationFailure $ "unexpected: " ++ show other
        Left e -> expectationFailure e

    it "rejects a step with both build and apply set" $
      decodePipeline
        ( BS8.unlines
            [ "pipeline:"
            , "  - build:"
            , "      name: x"
            , "      template: t"
            , "      target: {}"
            , "    apply: {}"
            ]
        )
        `shouldSatisfy` \case
          Left _ -> True
          _ -> False

    it "rejects a step with neither build nor apply" $
      decodePipeline "pipeline:\n  - {}\n" `shouldSatisfy` \case
        Left _ -> True
        _ -> False

  describe "Build" $ do
    it "applies defaults to optional fields" $ do
      let yaml =
            BS8.unlines
              [ "pipeline:"
              , "  - build:"
              , "      name: x"
              , "      template: tpl"
              , "      target: {}"
              ]
      case decodePipeline yaml of
        Right c -> do
          let b = firstBuild c
          buildName b `shouldBe` "x"
          buildTemplate b `shouldBe` "tpl"
          btFormat (buildTarget b) `shouldBe` FormatQcow2
          btCompact (buildTarget b) `shouldBe` True
          btIfExists (buildTarget b) `shouldBe` IfExistsError
          buildStrategy b `shouldBe` BuildStrategyOverlay
          buildCleanup b `shouldBe` CleanupAlways
          bvmCpuCount (buildVm b) `shouldBe` 4
          bvmRamMb (buildVm b) `shouldBe` 4096
          buildBootKeys b `shouldBe` []
          buildWaitForShutdownSec b `shouldBe` 3600
          buildUseCache b `shouldBe` False
          buildBuildCache b `shouldBe` False
        Left e -> expectationFailure e

    it "parses installer strategy with bootKeys + waitForShutdownSec" $ do
      let yaml =
            BS8.unlines
              [ "pipeline:"
              , "  - build:"
              , "      name: win"
              , "      template: tpl"
              , "      target: {}"
              , "      strategy: installer"
              , "      waitForShutdownSec: 1800"
              , "      bootKeys:"
              , "        - keys: ret"
              , "          delaySec: 3"
              , "          repeat: 5"
              , "          intervalSec: 1"
              , "        - keys: esc"
              ]
      case decodePipeline yaml of
        Right c -> do
          let b = firstBuild c
          buildStrategy b `shouldBe` BuildStrategyInstaller
          buildWaitForShutdownSec b `shouldBe` 1800
          map bkKeys (buildBootKeys b) `shouldBe` ["ret", "esc"]
          map bkDelaySec (buildBootKeys b) `shouldBe` [3, 0]
          map bkRepeat (buildBootKeys b) `shouldBe` [5, 1]
        Left e -> expectationFailure e

    it "rejects unknown strategy" $
      decodePipeline
        ( BS8.unlines
            [ "pipeline:"
            , "  - build:"
            , "      name: x"
            , "      template: tpl"
            , "      target: {}"
            , "      strategy: maybe"
            ]
        )
        `shouldSatisfy` \case
          Left _ -> True
          _ -> False

    it "rejects missing template" $
      decodePipeline
        ( BS8.unlines
            [ "pipeline:"
            , "  - build:"
            , "      name: x"
            , "      target: {}"
            ]
        )
        `shouldSatisfy` \case
          Left _ -> True
          _ -> False

  describe "BuildTarget ifExists" $ do
    let parseIfExists yaml = case decodePipeline yaml of
          Right c -> Right (btIfExists (buildTarget (firstBuild c)))
          Left e -> Left e
        wrapTarget v =
          BS8.unlines
            [ "pipeline:"
            , "  - build:"
            , "      name: x"
            , "      template: t"
            , "      target:"
            , "        ifExists: " <> v
            ]

    it "defaults to IfExistsError when ifExists is absent" $ do
      let yaml =
            BS8.unlines
              [ "pipeline:"
              , "  - build:"
              , "      name: x"
              , "      template: t"
              , "      target: {}"
              ]
      case decodePipeline yaml of
        Right c -> btIfExists (buildTarget (firstBuild c)) `shouldBe` IfExistsError
        Left e -> expectationFailure e

    it "parses ifExists: error" $
      parseIfExists (wrapTarget "error") `shouldBe` Right IfExistsError

    it "parses ifExists: skip" $
      parseIfExists (wrapTarget "skip") `shouldBe` Right IfExistsSkip

    it "parses ifExists: overwrite" $
      parseIfExists (wrapTarget "overwrite") `shouldBe` Right IfExistsOverwrite

    it "rejects unknown ifExists values" $
      decodePipeline (wrapTarget "maybe") `shouldSatisfy` \case
        Left _ -> True
        _ -> False

  describe "Provisioner: shell" $ do
    it "accepts shell as a bare string (sets inline)" $ do
      let yaml =
            BS8.unlines
              [ "pipeline:"
              , "  - build:"
              , "      name: x"
              , "      template: t"
              , "      target: {}"
              , "      provisioners:"
              , "        - shell: echo hi"
              ]
      case decodePipeline yaml of
        Right c -> do
          let b = firstBuild c
          case buildProvisioners b of
            [ProvShell sh] -> do
              shellInline sh `shouldBe` Just "echo hi"
              shellScript sh `shouldBe` Nothing
            other -> expectationFailure $ "unexpected: " ++ show other
        Left e -> expectationFailure e

    it "accepts shell as an object with inline + env + workdir + timeout" $ do
      let yaml =
            BS8.unlines
              [ "pipeline:"
              , "  - build:"
              , "      name: x"
              , "      template: t"
              , "      target: {}"
              , "      provisioners:"
              , "        - shell:"
              , "            inline: \"echo hi\""
              , "            workdir: /tmp"
              , "            env: { FOO: bar }"
              , "            timeoutSec: 60"
              ]
      case decodePipeline yaml of
        Right c -> do
          let b = firstBuild c
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
              [ "pipeline:"
              , "  - build:"
              , "      name: x"
              , "      template: t"
              , "      target: {}"
              , "      provisioners:"
              , "        - file:"
              , "            content: ZXhhbXBsZQo="
              , "            to: /etc/foo"
              ]
      case decodePipeline yaml of
        Right c -> do
          let b = firstBuild c
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
              [ "pipeline:"
              , "  - build:"
              , "      name: x"
              , "      template: t"
              , "      target: {}"
              , "      provisioners:"
              , "        - wait-for: { ping: true, timeoutSec: 30 }"
              ]
      case decodePipeline yaml of
        Right c -> do
          let b = firstBuild c
          case buildProvisioners b of
            [ProvWaitFor (WaitForPing 30)] -> pure ()
            other -> expectationFailure $ "unexpected: " ++ show other
        Left e -> expectationFailure e

    it "rejects wait-for with multiple keys" $
      decodePipeline
        ( BS8.unlines
            [ "pipeline:"
            , "  - build:"
            , "      name: x"
            , "      template: t"
            , "      target: {}"
            , "      provisioners:"
            , "        - wait-for: { ping: true, file: /tmp/x }"
            ]
        )
        `shouldSatisfy` \case
          Left _ -> True
          _ -> False

  describe "Provisioner: dispatch" $ do
    it "rejects a provisioner with two kinds set" $
      decodePipeline
        ( BS8.unlines
            [ "pipeline:"
            , "  - build:"
            , "      name: x"
            , "      template: t"
            , "      target: {}"
            , "      provisioners:"
            , "        - shell: \"echo\""
            , "          file: { content: \"\", to: /x }"
            ]
        )
        `shouldSatisfy` \case
          Left _ -> True
          _ -> False

    it "rejects a provisioner with no kinds" $
      decodePipeline
        ( BS8.unlines
            [ "pipeline:"
            , "  - build:"
            , "      name: x"
            , "      template: t"
            , "      target: {}"
            , "      provisioners:"
            , "        - {}"
            ]
        )
        `shouldSatisfy` \case
          Left _ -> True
          _ -> False

  describe "Floppy" $ do
    it "parses the post-preprocess form (contentBase64 + filename)" $ do
      let yaml =
            BS8.unlines
              [ "pipeline:"
              , "  - build:"
              , "      name: x"
              , "      template: t"
              , "      target: {}"
              , "      strategy: installer"
              , "      floppy:"
              , "        contentBase64: ZXhhbXBsZQo="
              , "        filename: autounattend.xml"
              ]
      case decodePipeline yaml of
        Right c -> do
          let b = firstBuild c
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
              [ "pipeline:"
              , "  - build:"
              , "      name: x"
              , "      template: t"
              , "      target:"
              , "        path: alpine-test/"
              ]
      case decodePipeline yaml of
        Right c -> btPath (buildTarget (firstBuild c)) `shouldBe` Just "alpine-test/"
        Left e -> expectationFailure e

    it "leaves buildFloppy=Nothing when no floppy: key is present" $ do
      let yaml =
            BS8.unlines
              [ "pipeline:"
              , "  - build:"
              , "      name: x"
              , "      template: t"
              , "      target: {}"
              ]
      case decodePipeline yaml of
        Right c -> buildFloppy (firstBuild c) `shouldBe` Nothing
        Left e -> expectationFailure e

  describe "ShellDefaults" $ do
    let parseSd yaml = case decodePipeline yaml of
          Right c -> Right (buildShellDefaults (firstBuild c))
          Left e -> Left e
        wrap inner =
          BS8.unlines
            ( [ "pipeline:"
              , "  - build:"
              , "      name: x"
              , "      template: t"
              , "      target: {}"
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
              [ "      shellDefaults:"
              , "        preamble: |"
              , "          set -eux"
              ]
      case parseSd yaml of
        Right sd -> do
          sdPreamble sd `shouldBe` Just "set -eux\n"
          sdEnv sd `shouldBe` []
        Left e -> expectationFailure e

    it "parses shellDefaults with only env" $ do
      let yaml =
            wrap
              [ "      shellDefaults:"
              , "        env:"
              , "          SYSROOT: /mnt/sysroot"
              ]
      case parseSd yaml of
        Right sd -> do
          sdPreamble sd `shouldBe` Nothing
          sdEnv sd `shouldBe` [("SYSROOT", "/mnt/sysroot")]
        Left e -> expectationFailure e

    it "parses shellDefaults with both preamble and env" $ do
      let yaml =
            wrap
              [ "      shellDefaults:"
              , "        preamble: \"set -eux\""
              , "        env:"
              , "          SYSROOT: /mnt/sysroot"
              ]
      case parseSd yaml of
        Right sd -> do
          sdPreamble sd `shouldBe` Just "set -eux"
          sdEnv sd `shouldBe` [("SYSROOT", "/mnt/sysroot")]
        Left e -> expectationFailure e

    it "rejects non-string env values" $
      decodePipeline
        ( wrap
            [ "      shellDefaults:"
            , "        env:"
            , "          FOO: 1"
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
        -- Auto-injected env (Corvus + shellDefaults.env) is wrapped in
        -- a POSIX save/restore of @set -x@ so a user preamble like
        -- @set -eux@ doesn't flood the build log with two trace lines
        -- per Corvus variable. The wrapper is a no-op when @-x@ wasn't
        -- on. The exact strings live here as test constants so the
        -- buildShellCommand expectations below stay readable.
        xtraceOff = "{ __corvus_xs=$-; set +x; } 2>/dev/null\n"
        xtraceOn = "{ case $__corvus_xs in *x*) set -x;; esac; unset __corvus_xs; } 2>/dev/null\n"

    it "returns the body unchanged when defaults + corvus + step are empty" $
      buildShellCommand emptyShellDefaults [] bareShell "echo hi"
        `shouldBe` "echo hi"

    it "prepends sdPreamble before the body" $
      buildShellCommand
        emptyShellDefaults {sdPreamble = Just "set -eux"}
        []
        bareShell
        "echo hi"
        `shouldBe` "set -eux\necho hi"

    it "exports sdEnv before the body, wrapped in xtrace save/restore" $
      buildShellCommand
        emptyShellDefaults {sdEnv = [("SYSROOT", "/mnt/sysroot")]}
        []
        bareShell
        "echo hi"
        `shouldBe` xtraceOff <> "export SYSROOT='/mnt/sysroot'\n" <> xtraceOn <> "echo hi"

    it "exports the corvusEnv layer between preamble and shellDefaults env" $
      buildShellCommand
        emptyShellDefaults {sdPreamble = Just "set -eux", sdEnv = [("SD", "sd")]}
        [("CORVUS_VERSION", "0.9.0.0")]
        bareShell
        "echo hi"
        `shouldBe` "set -eux\n"
          <> xtraceOff
          <> "export CORVUS_VERSION='0.9.0.0'\nexport SD='sd'\n"
          <> xtraceOn
          <> "echo hi"

    it "orders preamble → (auto env wrapped) → stepEnv → workdir → body" $
      let sd =
            ShellDefaults
              { sdPreamble = Just "set -eux"
              , sdEnv = [("SHARED", "1")]
              }
          sh = bareShell {shellEnv = [("STEP", "2")], shellWorkdir = Just "/tmp"}
       in buildShellCommand sd [("CORVUS_VERSION", "v")] sh "echo hi"
            `shouldBe` "set -eux\n"
              <> xtraceOff
              <> "export CORVUS_VERSION='v'\nexport SHARED='1'\n"
              <> xtraceOn
              <> "export STEP='2'\ncd '/tmp'\necho hi"

    it "leaves step env outside the xtrace wrapper" $
      let sh = bareShell {shellEnv = [("STEP_VAR", "v")]}
       in buildShellCommand emptyShellDefaults [] sh "echo $STEP_VAR"
            `shouldBe` "export STEP_VAR='v'\necho $STEP_VAR"

    it "lets the step override a defaults env key (later export wins in shell)" $
      let sd = emptyShellDefaults {sdEnv = [("X", "default")]}
          sh = bareShell {shellEnv = [("X", "step")]}
       in buildShellCommand sd [] sh "echo $X"
            `shouldBe` xtraceOff
              <> "export X='default'\n"
              <> xtraceOn
              <> "export X='step'\necho $X"

    it "lets shellDefaults.env override a CORVUS_* var" $
      buildShellCommand
        emptyShellDefaults {sdEnv = [("CORVUS_VERSION", "override")]}
        [("CORVUS_VERSION", "real")]
        bareShell
        "echo $CORVUS_VERSION"
        `shouldBe` xtraceOff
          <> "export CORVUS_VERSION='real'\nexport CORVUS_VERSION='override'\n"
          <> xtraceOn
          <> "echo $CORVUS_VERSION"

    it "single-quotes env values that contain spaces" $
      buildShellCommand
        emptyShellDefaults {sdEnv = [("MSG", "hello world")]}
        []
        bareShell
        "echo $MSG"
        `shouldBe` xtraceOff
          <> "export MSG='hello world'\n"
          <> xtraceOn
          <> "echo $MSG"
