{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Corvus.ClientFilesSpec (spec) where

import qualified Capnp as C
import qualified Capnp.Gen.Cloudinit as GCloudinit
import qualified Capnp.Gen.Corvus as GCorvus
import qualified Capnp.Gen.Disk as GDisk
import qualified Capnp.Gen.Streams as GStreams
import qualified Capnp.Gen.Template as GTemplate
import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Corvus.Client.Types
import Corvus.Model (TaskResult (..))
import Corvus.Protocol.Apply (ApplyEvent (..))
import Corvus.Protocol.Build (BuildEvent (..), BuildOne (..), BuildResult (..))
import Corvus.Wire.Apply (toCapnpApplyEvent)
import Corvus.Wire.Build (toCapnpBuildEvent)
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString.Char8 as BS
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import Test.ClientCommand
import Test.ClientRpc
import Test.Hspec

spec :: Spec
spec = sequential $ describe "CLI files, editors and event streams" $ do
  describe "file errors" $ do
    forM_ [TemplateCreate (Just "/nonexistent-corvus.yml"), CloudInitSet "vm" (Just "/nonexistent-corvus.yml"), Apply "/nonexistent-corvus.yml" False noWait] $ \cmd ->
      it (show cmd) $ withMock $ \f -> do
        expectCalls f []
        out <- checkCli f JsonOutput cmd (ExitFailure 1)
        out `shouldSatisfy` isInfixOf "file_not_found"
    forM_ [("[", "yaml_parse"), ("pipeline: [{build: {provisioners: [{shell: {script: missing}}]}}]", "preprocess_error"), ("pipeline: [{build: {}}, {upload: {}}]", "upload_error"), ("pipeline: [{upload: {}}]", "upload_error"), ("pipeline: [{upload: null}]", "upload_error"), ("pipeline: [{upload: {name: n, from: x, format: bad}}]", "upload_error"), ("pipeline: [{upload: {name: n, from: x, format: raw, ifExists: bad}}]", "upload_error"), ("pipeline: [{upload: {name: n, from: x, format: raw, ephemeral: x}}]", "upload_error"), ("pipeline: [{upload: {name: n, from: x, format: raw, path: false}}]", "upload_error"), ("vars: {required: null}", "build_vars")] $ \(yaml, errorCode) ->
      it ("rejects build input: " ++ yaml) $ withFile yaml $ \path -> withMock $ \f -> do
        expectCalls f []
        out <- checkCli f JsonOutput (Build path noVars noWait) (ExitFailure 1)
        out `shouldSatisfy` isInfixOf errorCode
    it "reports unreadable build files" $ withMock $ \f -> do
      expectCalls f []
      out <- checkCli f JsonOutput (Build "/nonexistent-corvus.yml" noVars noWait) (ExitFailure 1)
      out `shouldSatisfy` isInfixOf "yaml_parse"
    it "rejects malformed cloud-init YAML locally" $ withFile "[" $ \path -> withMock $ \f -> do
      expectCalls f []
      out <- checkCli f JsonOutput (CloudInitSet "vm" (Just path)) (ExitFailure 1)
      out `shouldSatisfy` isInfixOf "parse_error"
  describe "local documents reach typed RPC requests" $ do
    forM_ [TextOutput, JsonOutput, YamlOutput] $ \fmt -> do
      forM_ [False, True] $ \failRpc -> do
        it ("creates a template from file: " ++ show (fmt, failRpc)) $ withFile "name: new\n" $ \path -> withMock $ \f -> do
          expectCalls f ["Daemon.templates", "TemplateManager.create", "Template.show"]
          setFailure f (if failRpc then Just "Template.show" else Nothing)
          _ <- checkCli f fmt (TemplateCreate (Just path)) (exitFor failRpc)
          [GTemplate.TemplateManager'create'params yaml] <- requests f "TemplateManager.create"
          yaml `shouldBe` "name: new\n"
        it ("sets cloud-init from file: " ++ show (fmt, failRpc)) $ withFile "userData: hello\nnetworkConfig: net\ninjectSshKeys: false\n" $ \path -> withMock $ \f -> do
          expectCalls f ["Daemon.cloudInit", "CloudInitManager.set"]
          setFailure f (if failRpc then Just "CloudInitManager.set" else Nothing)
          _ <- checkCli f fmt (CloudInitSet "vm" (Just path)) (exitFor failRpc)
          [GCloudinit.CloudInitManager'set'params (GCloudinit.CloudInitSetParams _ config)] <- requests f "CloudInitManager.set"
          config `shouldBe` GCloudinit.CloudInitInfo True "hello" True "net" False
        it ("starts apply from file: " ++ show (fmt, failRpc)) $ withFile "vms: []\n" $ \path -> withMock $ \f -> do
          expectCalls f ["Daemon.apply"]
          setFailure f (if failRpc then Just "Daemon.apply" else Nothing)
          _ <- checkCli f fmt (Apply path True noWait) (exitFor failRpc)
          [GCorvus.Daemon'apply'params yaml skip wait _] <- requests f "Daemon.apply"
          (yaml, skip, wait) `shouldBe` ("vms: []\n", True, False)
        it ("starts build from file: " ++ show (fmt, failRpc)) $ withFile "pipeline: []\n" $ \path -> withMock $ \f -> do
          expectCalls f ["Daemon.build"]
          setFailure f (if failRpc then Just "Daemon.build" else Nothing)
          void $ checkCli f fmt (Build path noVars noWait) (exitFor failRpc)
  forM_ [JsonOutput, YamlOutput] $ \fmt -> it ("returns the completed apply result as " ++ show fmt) $ withFile "vms: []" $ \path -> withMock $ \f -> do
    expectCalls f ["Daemon.apply"]
    out <- checkCli f fmt (Apply path False (WaitOptions True Nothing)) ExitSuccess
    out `shouldSatisfy` isInfixOf "fixture"
    [GCorvus.Daemon'apply'params yaml skip wait _] <- requests f "Daemon.apply"
    (yaml, skip, wait) `shouldBe` ("vms: []", False, True)
  it "reports a streaming apply RPC failure" $ withFile "vms: []" $ \path -> withMock $ \f -> do
    expectCalls f ["Daemon.apply"]
    setFailure f (Just "Daemon.apply")
    out <- checkCli f TextOutput (Apply path False (WaitOptions True Nothing)) (ExitFailure 1)
    out `shouldSatisfy` isInfixOf "Apply failed"
  it "inlines relative scripts and binary files, preserving unrelated provisioners" $ withSystemTempDirectory "cli-build" $ \dir -> do
    let path = dir </> "build.yml"
    writeFile (dir </> "script.sh") "echo hello\n"
    BS.writeFile (dir </> "binary") (BS.pack ['\0', '\255'])
    writeFile path "pipeline:\n- apply: {}\n- build:\n    provisioners:\n    - shell: {script: script.sh}\n    - file: {from: binary}\n    - shell: {inline: existing}\n    - file: {content: existing}\n    - wait-for: {}\n"
    withMock $ \f -> do
      expectCalls f ["Daemon.build"]
      _ <- checkCli f JsonOutput (Build path noVars noWait) ExitSuccess
      [GCorvus.Daemon'build'params yaml _] <- requests f "Daemon.build"
      parsed <- either (fail . show) pure (Yaml.decodeEither' (TE.encodeUtf8 yaml) :: Either Yaml.ParseException Value)
      parsed `shouldBe` object ["pipeline" .= [object ["apply" .= object []], object ["build" .= object ["provisioners" .= [object ["shell" .= object ["inline" .= ("echo hello\n" :: T.Text)]], object ["file" .= object ["content" .= ("AP8=" :: T.Text)]], object ["shell" .= object ["inline" .= ("existing" :: T.Text)]], object ["file" .= object ["content" .= ("existing" :: T.Text)]], object ["wait-for" .= object []]]]]]]
  describe "build uploads and variables" $ do
    forM_ [False, True] $ \failRpc -> it ("uploads local media before submitting only daemon steps: " ++ show failRpc) $ withSystemTempDirectory "cli-build-upload" $ \dir -> withMock $ \f -> do
      let path = dir </> "build.yml"
      BS.writeFile (dir </> "media.raw") "binary"
      writeFile path "pipeline: [{upload: {name: media, from: media.raw, format: raw, node: host, path: /media, ephemeral: false, ifExists: overwrite}}, {build: {}}]"
      expectCalls f (["Daemon.disks", "DiskManager.beginUpload", "DiskUpload.write"] ++ if failRpc then ["DiskUpload.abort"] else ["DiskUpload.finish", "Disk.show", "Daemon.build"])
      setFailure f (if failRpc then Just "DiskUpload.write" else Nothing)
      _ <- checkCli f JsonOutput (Build path noVars noWait) (exitFor failRpc)
      if failRpc
        then pure ()
        else do
          [GCorvus.Daemon'build'params yaml _] <- requests f "Daemon.build"
          parsed <- either (fail . show) pure (Yaml.decodeEither' (TE.encodeUtf8 yaml) :: Either Yaml.ParseException Value)
          parsed `shouldBe` object ["pipeline" .= [object ["build" .= object []]]]
          [GDisk.DiskUpload'write'params bytes] <- requests f "DiskUpload.write"
          bytes `shouldBe` "binary"
    it "substitutes CLI variable overrides before forwarding YAML" $ withFile "vars: {name: default}\npipeline: [{build: {name: '{{name}}'}}]" $ \path -> withMock $ \f -> do
      expectCalls f ["Daemon.build"]
      _ <- checkCli f JsonOutput (Build path (BuildClientOptions [("name", "overridden")] []) noWait) ExitSuccess
      [GCorvus.Daemon'build'params yaml _] <- requests f "Daemon.build"
      yaml `shouldSatisfy` T.isInfixOf "overridden"
      yaml `shouldSatisfy` (not . T.isInfixOf "{{name}}")
    forM_ ["[]", "pipeline: null", "pipeline: [1, {build: null}, {build: {provisioners: [1, {shell: null}, {file: null}]}}]", "pipeline: [{upload: {name: n, from: absent, format: raw}}]"] $ \yaml ->
      it ("handles preprocessing shape: " ++ yaml) $ withFile yaml $ \path -> withMock $ \f -> do
        let missing = "absent" `isInfixOf` yaml
        expectCalls f (["Daemon.build" | not missing])
        void $ checkCli f JsonOutput (Build path noVars noWait) (exitFor missing)
  describe "temporary editor" $ do
    forM_ [TemplateCreate Nothing, CloudInitSet "vm" Nothing] $ \cmd -> do
      it ("reports editor failure for " ++ show cmd) $ withEditor "exit 2" $ withMock $ \f -> do
        expectCalls f []
        out <- checkCli f JsonOutput cmd (ExitFailure 1)
        out `shouldSatisfy` isInfixOf "editor"
    forM_ [False, True] $ \changed -> do
      forM_ [False, True] $ \failRpc -> do
        it ("edits template: " ++ show (changed, failRpc)) $ withEditor (if changed then "printf '\\n# change\\n' >> \"$1\"" else "exit 0") $ withMock $ \f -> do
          let calls = ["Daemon.templates", "TemplateManager.get", "Template.show"] ++ if changed then ["Daemon.templates", "TemplateManager.get", "Template.update"] else []
          expectCalls f calls
          setFailure f (if failRpc && changed then Just "Template.update" else Nothing)
          out <- checkCli f TextOutput (TemplateEdit "fixture") (exitFor (failRpc && changed))
          out `shouldSatisfy` isInfixOf (if failRpc && changed then "scripted RPC failure" else if changed then "updated" else "No changes")
        it ("edits cloud-init: " ++ show (changed, failRpc)) $ withEditor (if changed then "printf '\\n# change\\n' >> \"$1\"" else "exit 0") $ withMock $ \f -> do
          let calls = ["Daemon.cloudInit", "CloudInitManager.get"] ++ if changed then ["Daemon.cloudInit", "CloudInitManager.set"] else []
          expectCalls f calls
          setFailure f (if failRpc && changed then Just "CloudInitManager.set" else Nothing)
          void $ checkCli f TextOutput (CloudInitEdit "fixture") (exitFor (failRpc && changed))
    forM_ [(TemplateEdit "fixture", ["Daemon.templates", "TemplateManager.get", "Template.show"]), (CloudInitEdit "fixture", ["Daemon.cloudInit", "CloudInitManager.get"])] $ \(cmd, calls) -> do
      it ("reports editor failure after fetching " ++ show cmd) $ withEditor "exit 2" $ withMock $ \f -> do
        expectCalls f calls
        _ <- checkCli f JsonOutput cmd (ExitFailure 1)
        pure ()
      it ("reports fetch failure before opening editor for " ++ show cmd) $ withMock $ \f -> do
        expectCalls f calls
        setFailure f (Just (last calls))
        _ <- checkCli f JsonOutput cmd (ExitFailure 1)
        pure ()
  describe "build event outcomes" $ do
    let successful = BuildOne "image" (Just 42) Nothing
        failed = BuildOne "image" Nothing (Just "provisioning failed")
    forM_ [("success", [BuildLogLine "hello", StepStart 1 "shell" "running", StepOutput 1 "output", StepEnd 1 TaskSuccess (Just "done"), BuildEnd (Right 42), PipelineEnd (BuildResult [successful])], ExitSuccess), ("failed build", [BuildEnd (Left "provisioning failed"), PipelineEnd (BuildResult [failed])], ExitFailure 1), ("failed step", [StepEnd 1 TaskError Nothing, PipelineEnd (BuildResult [successful])], ExitFailure 1), ("cancelled step", [StepEnd 1 TaskCancelled Nothing, PipelineEnd (BuildResult [successful])], ExitFailure 1), ("mixed results", [PipelineEnd (BuildResult [successful, failed])], ExitFailure 1), ("apply-only pipeline", [BuildEnd (Right 0), PipelineEnd (BuildResult [BuildOne "apply" Nothing Nothing])], ExitSuccess), ("mixed apply and build pipeline", [PipelineEnd (BuildResult [BuildOne "apply" Nothing Nothing, successful])], ExitSuccess), ("premature end", [], ExitFailure 1)] $ \(label, events, exit) ->
      it label $ withFile "pipeline: []" $ \path -> withMock $ \f -> do
        expectCalls f ["Daemon.build"]
        setReply f "Daemon.build" $ \(GCorvus.Daemon'build'params _ sink) -> do
          forM_ events $ \ev -> void (C.callP #push (GStreams.BuildEventSink'push'params (toCapnpBuildEvent ev)) sink >>= C.waitPipeline)
          void (C.callP #end GStreams.BuildEventSink'end'params sink >>= C.waitPipeline)
          pure (GCorvus.Daemon'build'results 42)
        out <- checkCli f TextOutput (Build path noVars (WaitOptions True Nothing)) exit
        if null events then out `shouldBe` "" else out `shouldSatisfy` (not . null)
  describe "apply streaming" $ forM_ [TaskSuccess, TaskError, TaskCancelled] $ \result ->
    it (show result) $ withFile "vms: []" $ \path -> withMock $ \f -> do
      expectCalls f ["Daemon.apply"]
      setReply f "Daemon.apply" $ \(GCorvus.Daemon'apply'params _ _ _ sink) -> do
        let events = [ApplyLogLine "hello", PhaseStart "disks" 2, EntityStart "disks" "image" "disk-import", DownloadStart "image" "https://fixture/image", DownloadProgress "image" 10 100, DownloadProgress "image" 20 0, DownloadEnd "image" True "done", DownloadEnd "other" False "failure", EntityEnd "disks" "image" TaskSuccess "created" 42, EntityEnd "disks" "skip" TaskSuccess "existing" 0, EntityEnd "disks" "bad" TaskError "failed" 0, ApplyEnd result "done" 42]
        forM_ events $ \ev -> void (C.callP #push (GStreams.ApplyEventSink'push'params (toCapnpApplyEvent ev)) sink >>= C.waitPipeline)
        void (C.callP #end GStreams.ApplyEventSink'end'params sink >>= C.waitPipeline)
        pure (GCorvus.Daemon'apply'results (GCorvus.ApplyResult [] [] [] [] []) 42)
      out <- checkCli f TextOutput (Apply path False (WaitOptions True Nothing)) (exitFor (result /= TaskSuccess))
      out `shouldSatisfy` isInfixOf "hello"

noWait :: WaitOptions
noWait = WaitOptions False Nothing

noVars :: BuildClientOptions
noVars = BuildClientOptions [] []

exitFor :: Bool -> ExitCode
exitFor True = ExitFailure 1
exitFor False = ExitSuccess

withFile :: String -> (FilePath -> IO a) -> IO a
withFile content action = withSystemTempDirectory "cli-file" $ \dir -> do
  let path = dir </> "input.yml"
  writeFile path content
  action path

withEditor :: String -> IO a -> IO a
withEditor body action = withSystemTempDirectory "cli-editor" $ \dir -> do
  let path = dir </> "editor"
  writeFile path ("#!/bin/sh\n" ++ body ++ "\n")
  setFileMode path 0o700
  withEnv "EDITOR" (Just path) action

withEnv :: String -> Maybe String -> IO a -> IO a
withEnv name value action = bracket (lookupEnv name) restore $ \_ -> restore value >> action
  where
    restore Nothing = unsetEnv name
    restore (Just text) = setEnv name text
