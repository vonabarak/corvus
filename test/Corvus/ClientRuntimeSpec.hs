{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Corvus.ClientRuntimeSpec (spec) where

import qualified Capnp as C
import qualified Capnp.Gen.Common as GCommon
import qualified Capnp.Gen.Disk as GDisk
import qualified Capnp.Gen.Enums as GEnums
import qualified Capnp.Gen.Streams as GStreams
import qualified Capnp.Gen.Task as GTask
import qualified Capnp.Gen.Vm as GVm
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Exception (bracket)
import Control.Monad (forM_, void, when)
import Corvus.Client.Commands.Vm (RawSessionKind (..), runRawTerminalSession, runRemoteViewer)
import Corvus.Client.Completion
import Corvus.Client.Config (ClientConfig (..))
import Corvus.Client.Types
import Corvus.Wire.Common (ViewGrant (..))
import qualified Data.ByteString as BS
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import qualified Data.Text as T
import Options.Applicative.Types (Completer (..))
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import System.Posix.IO (closeFd, dup, dupTo, fdWrite, stdInput)
import System.Posix.Terminal (TerminalMode (..), getTerminalAttributes, openPseudoTerminal, terminalMode)
import System.Posix.Types (Fd)
import System.Timeout (timeout)
import Test.ClientCommand
import Test.ClientRpc
import Test.Hspec
import Prelude hiding (id)

spec :: Spec
spec = sequential $ describe "CLI runtime and transport" $ do
  describe "task polling" $ do
    forM_ [TextOutput, JsonOutput, YamlOutput] $ \fmt -> do
      forM_ [GEnums.TaskResult'success, GEnums.TaskResult'error, GEnums.TaskResult'cancelled] $ \result ->
        it (show (fmt, result)) $ withMock $ \f -> do
          states <- newIORef [GEnums.TaskResult'notStarted, GEnums.TaskResult'running, result]
          setReply f "Task.show" $ \(_ :: C.Parsed GTask.Task'show'params) -> do
            state <- atomicModifyIORef' states $ \case x : rest -> (rest, x); [] -> ([], result)
            pure (GTask.Task'show'results (sampleTaskInfo {GTask.result = state, GTask.finishedAt = if state == GEnums.TaskResult'running || state == GEnums.TaskResult'notStarted then 0 else 61000000000}))
          expectCalls f (concat (replicate 3 taskShow))
          (code, out, err) <- runCli (cliOptions f fmt (TaskWait 42 (Just 4)))
          code `shouldBe` if result == GEnums.TaskResult'success then ExitSuccess else ExitFailure 1
          out `shouldSatisfy` (not . null)
          if fmt == TextOutput then err `shouldSatisfy` isInfixOf "Waiting" else err `shouldBe` ""
          assertCalls f
      it ("times out without polling: " ++ show fmt) $ withMock $ \f -> do
        expectCalls f taskShow
        setReply f "Task.show" (\(_ :: C.Parsed GTask.Task'show'params) -> pure (GTask.Task'show'results (sampleTaskInfo {GTask.result = GEnums.TaskResult'running, GTask.finishedAt = 0})))
        out <- checkCli f fmt (TaskWait 42 (Just 0)) (ExitFailure 1)
        out `shouldSatisfy` isInfixOf (if fmt == TextOutput then "Timeout" else "timeout")
      it ("reports a polling RPC failure: " ++ show fmt) $ withMock $ \f -> do
        expectCalls f (taskShow ++ taskShow)
        setReply f "Task.show" $ \(_ :: C.Parsed GTask.Task'show'params) -> do
          setFailure f (Just "Task.show")
          pure (GTask.Task'show'results (sampleTaskInfo {GTask.result = GEnums.TaskResult'running, GTask.finishedAt = 0}))
        out <- checkCli f fmt (TaskWait 42 Nothing) (ExitFailure 1)
        out `shouldSatisfy` isInfixOf "scripted RPC failure"
  describe "disk upload" $ do
    forM_ ["error", "update", "overwrite", "skip"] $ \policy ->
      it ("streams binary data with policy " ++ T.unpack policy) $ withSystemTempDirectory "cli-upload" $ \dir -> withMock $ \f -> do
        let path = dir </> "disk.raw"
            bytes = BS.pack [0, 255, 42]
        BS.writeFile path bytes
        expectCalls f ["Daemon.disks", "DiskManager.beginUpload", "DiskUpload.write", "DiskUpload.finish", "Disk.show"]
        _ <- checkCli f JsonOutput (DiskUpload "new" path "raw" (Just "/destination") True "node" policy) ExitSuccess
        [GDisk.DiskUpload'write'params chunk] <- requests f "DiskUpload.write"
        chunk `shouldBe` bytes
        [GDisk.DiskManager'beginUpload'params params] <- requests f "DiskManager.beginUpload"
        GDisk.sourcePath (params :: C.Parsed GDisk.DiskUploadParams) `shouldBe` T.pack path
        GDisk.expectedSha256 params `shouldBe` if policy == "update" then "c3e2d6ce9638c39fdf3c92460f115d9e428e217022ab2b5eb8852ada85526a51" else ""
        let GDisk.DiskUploadParams {GDisk.node = node} = params
        node `shouldBe` GCommon.EntityRef (GCommon.EntityRef'name "node")
    forM_ ["DiskUpload.write", "DiskUpload.finish"] $ \method ->
      it ("aborts after " ++ method ++ " failure") $ withSystemTempDirectory "cli-upload" $ \dir -> withMock $ \f -> do
        let path = dir </> "disk.raw"
        BS.writeFile path "data"
        expectCalls f (["Daemon.disks", "DiskManager.beginUpload", "DiskUpload.write"] ++ ["DiskUpload.finish" | method == "DiskUpload.finish"] ++ ["DiskUpload.abort"])
        setFailure f (Just method)
        _ <- checkCli f JsonOutput (DiskUpload "new" path "raw" Nothing False "" "overwrite") (ExitFailure 1)
        pure ()
    it "reuses an existing disk without opening or streaming the source" $ withMock $ \f -> do
      disk <- mockClient f
      setReply f "DiskManager.beginUpload" (\(_ :: C.Parsed GDisk.DiskManager'beginUpload'params) -> pure (GDisk.DiskManager'beginUpload'results (GDisk.DiskUploadResult (GDisk.DiskUploadResult'existing disk))))
      expectCalls f ["Daemon.disks", "DiskManager.beginUpload", "Disk.show"]
      out <- checkCli f TextOutput (DiskUpload "new" "/nonexistent-corvus-disk" "raw" Nothing False "" "skip") ExitSuccess
      out `shouldSatisfy` isInfixOf "reused"
  describe "shell completion" $ do
    forM_ [(vmCompleter, "vms", "Vm", ["fixture"]), (diskCompleter, "disks", "Disk", ["42", "fixture:fixture"]), (networkCompleter, "networks", "Network", ["fixture"]), (nodeCompleter, "nodes", "Node", ["fixture"]), (sshKeyCompleter, "sshKeys", "SshKey", ["fixture"]), (templateCompleter, "templates", "Template", ["fixture"])] $ \(completer, getter, kind, labels) -> do
      it ("completes " ++ getter ++ " and filters prefixes") $ withMock $ \f -> withEnv "CORVUS_SOCKET" (Just (mockSocket f)) $ do
        forM_ [("", labels), ("fixture", filter (isInfixOf "fixture") labels), ("absent", [])] $ \(prefix, expected) -> do
          expectCalls f ["Daemon." ++ getter, kind ++ "Manager.list"]
          runCompleter completer prefix `shouldReturn` expected
          assertCalls f
      it ("silently handles " ++ getter ++ " RPC errors") $ withMock $ \f -> withEnv "CORVUS_SOCKET" (Just (mockSocket f)) $ do
        expectCalls f ["Daemon." ++ getter, kind ++ "Manager.list"]
        setFailure f (Just (kind ++ "Manager.list"))
        runCompleter completer "" `shouldReturn` []
        assertCalls f
    it "silently handles a missing socket" $ withEnv "CORVUS_SOCKET" (Just "/nonexistent-corvus.sock") $ runCompleter vmCompleter "" `shouldReturn` []
    forM_ ["bash", "zsh", "fish", "BASH", "unknown"] $ \shell ->
      it ("generates completion for " ++ T.unpack shell ++ " before connecting") $ withMock $ \f -> do
        expectCalls f []
        (code, out, err) <- runCli ((cliOptions f TextOutput (Completion shell)) {optSocket = Just "/nonexistent-corvus.sock"})
        code `shouldBe` ExitSuccess
        if shell == "unknown" then (out, "Unknown shell" `isInfixOf` err) `shouldBe` ("", True) else ("crv" `isInfixOf` out, err) `shouldBe` (True, "")
        assertCalls f
  describe "connections and graphical view" $ do
    it "reports connection errors in structured output" $ withMock $ \f -> do
      (code, out, err) <- runCli ((cliOptions f JsonOutput Ping) {optSocket = Just "/nonexistent-corvus.sock"})
      (code, err) `shouldBe` (ExitFailure 1, "")
      out `shouldSatisfy` isInfixOf "connection_error"
    forM_ [JsonOutput, YamlOutput] $ \fmt ->
      it ("renders a SPICE grant as " ++ show fmt) $ withMock $ \f -> do
        expectCalls f (vm "show" ++ vm "viewGrant")
        setReply f "Vm.show" (\(_ :: C.Parsed GVm.Vm'show'params) -> pure (GVm.Vm'show'results (case sampleVmDetails of GVm.VmDetails {..} -> GVm.VmDetails {GVm.headless = False, ..})))
        out <- checkCli f fmt (VmView "fixture") ExitSuccess
        out `shouldSatisfy` isInfixOf "password"
    it "rejects viewing a stopped VM" $ withMock $ \f -> do
      expectCalls f (vm "show")
      setReply f "Vm.show" (\(_ :: C.Parsed GVm.Vm'show'params) -> pure (GVm.Vm'show'results (case sampleVmDetails of GVm.VmDetails {..} -> GVm.VmDetails {GVm.status = GEnums.VmStatus'stopped, ..})))
      out <- checkCli f JsonOutput (VmView "fixture") (ExitFailure 1)
      out `shouldSatisfy` isInfixOf "vm_not_running"
    forM_ ["Vm.show", "Vm.viewGrant"] $ \method ->
      it ("reports view failure at " ++ method) $ withMock $ \f -> do
        expectCalls f (vm "show" ++ if method == "Vm.viewGrant" then vm "viewGrant" else [])
        setReply f "Vm.show" (\(_ :: C.Parsed GVm.Vm'show'params) -> pure (GVm.Vm'show'results (case sampleVmDetails of GVm.VmDetails {..} -> GVm.VmDetails {GVm.headless = False, ..})))
        setFailure f (Just method)
        _ <- checkCli f JsonOutput (VmView "fixture") (ExitFailure 1)
        pure ()
  describe "TCP and TLS selection" $ do
    it "connects to the scripted TCP peer with TLS disabled" $ withMockTcp $ \f -> do
      port <- readIORef (mockPort f)
      expectCalls f ["Daemon.ping"]
      (code, out, err) <- runCli ((cliOptions f JsonOutput Ping) {optSocket = Nothing, optPort = port, optNoTls = True})
      (code, err) `shouldBe` (ExitSuccess, "")
      out `shouldSatisfy` isInfixOf "ok"
      assertCalls f
    it "reports missing TLS material before connecting" $ withSystemTempDirectory "cli-tls" $ \dir -> withMock $ \f -> do
      expectCalls f []
      (code, out, err) <- runCli ((cliOptions f JsonOutput Ping) {optSocket = Nothing, optTlsCertDir = Just dir})
      (code, out) `shouldBe` (ExitFailure 1, "")
      err `shouldSatisfy` isInfixOf "failed to load TLS material"
      assertCalls f
    it "uses a TCP address for completion from host and port" $ withMockTcp $ \f -> do
      port <- readIORef (mockPort f)
      withEnv "CORVUS_SOCKET" Nothing $ withEnv "CORVUS_HOST" (Just "127.0.0.1") $ withEnv "CORVUS_PORT" (Just (show port)) $ do
        expectCalls f ["Daemon.vms", "VmManager.list"]
        runCompleter vmCompleter "" `shouldReturn` ["fixture"]
        assertCalls f
    it "falls back to the default Unix path for an invalid completion port" $ withSystemTempDirectory "cli-default" $ \dir ->
      withEnv "CORVUS_SOCKET" Nothing $
        withEnv "CORVUS_HOST" (Just "127.0.0.1") $
          withEnv "CORVUS_PORT" (Just "bad") $
            withEnv "XDG_RUNTIME_DIR" (Just dir) $
              runCompleter vmCompleter "" `shouldReturn` []
    it "resolves --unix without an explicit socket" $ withSystemTempDirectory "cli-default" $ \dir -> withEnv "XDG_RUNTIME_DIR" (Just dir) $ withMock $ \f -> do
      (code, out, err) <- runCli ((cliOptions f TextOutput Ping) {optSocket = Nothing, optUnix = True})
      (code, err) `shouldBe` (ExitFailure 1, "")
      out `shouldSatisfy` isInfixOf "Connection error"
  describe "console command dispatch" $ do
    forM_ [(VmView "fixture", "serialConsole", vm "show"), (VmMonitor "fixture", "hmpMonitor", [])] $ \(cmd, method, prefix) -> do
      it ("forwards remote output and local input for " ++ method) $ withPty $ \master -> withMock $ \f -> do
        sink <- mockClient f
        let send :: C.Client GStreams.ByteSink -> IO ()
            send output = void (C.callP #write (GStreams.ByteSink'write'params "remote output") output >>= C.waitPipeline)
        if method == "serialConsole"
          then setReply f "Vm.serialConsole" $ \(GVm.Vm'serialConsole'params output) -> send output >> pure (GVm.Vm'serialConsole'results sink)
          else setReply f "Vm.hmpMonitor" $ \(GVm.Vm'hmpMonitor'params output) -> send output >> pure (GVm.Vm'hmpMonitor'results sink)
        expectCalls f (prefix ++ vm method ++ ["ByteSink.write", "ByteSink.end"])
        (code, out, err) <- withAsync (awaitRaw >> void (fdWrite master "a\GSq")) $ \_ -> runCli (cliOptions f TextOutput cmd)
        (code, err) `shouldBe` (ExitSuccess, "")
        out `shouldSatisfy` isInfixOf "remote output"
        [GStreams.ByteSink'write'params bytes] <- requests f "ByteSink.write"
        bytes `shouldBe` "a"
        assertCalls f
      it ("reports attach failure for " ++ method) $ withMock $ \f -> do
        expectCalls f (prefix ++ vm method)
        setFailure f (Just ("Vm." ++ method))
        out <- checkCli f JsonOutput cmd (ExitFailure 1)
        out `shouldSatisfy` isInfixOf "scripted RPC failure"
  describe "viewer credential file" $ forM_ [False, True] $ \failViewer ->
    it ("passes one private temporary file and removes it: " ++ show failViewer) $ withSystemTempDirectory "cli-viewer" $ \dir -> do
      let viewer = dir </> "viewer"
          record = dir </> "record"
      writeFile viewer ("#!/bin/sh\ntest \"$#\" -eq 1 || exit 3\nprintf '%s\\n' \"$1\" > '" ++ record ++ "'\nstat -c %a \"$1\" >> '" ++ record ++ "'\ncat \"$1\" >> '" ++ record ++ "'\nexit " ++ if failViewer then "2\n" else "0\n")
      setFileMode viewer 0o700
      (out, ok) <- hCapture [stdout] $ runRemoteViewer (ClientConfig viewer) (ViewGrant "host" 5900 "secret" 30)
      ok `shouldBe` not failViewer
      if failViewer then out `shouldSatisfy` isInfixOf "Failed" else out `shouldBe` ""
      saved <- lines <$> readFile record
      case saved of
        path : mode : body -> do
          mode `shouldBe` "600"
          body `shouldBe` ["[virt-viewer]", "type=spice", "host=host", "port=5900", "password=secret"]
          doesFileExist path `shouldReturn` False
        _ -> expectationFailure "Viewer did not record its single file argument"
  describe "raw terminal restoration" $ do
    forM_ [SerialSession, MonitorSession] $ \kind ->
      forM_ [Nothing, Just False, Just True] $ \failure ->
        it ("forwards bytes, handles escapes and restores terminal: " ++ show (kind, failure)) $ withPty $ \master -> do
          writes <- newIORef []
          closed <- newIORef (0 :: Int)
          end <- newEmptyMVar
          attrs <- getTerminalAttributes stdInput
          let action = case failure of Nothing -> Nothing; Just False -> Just (pure ()); Just True -> Just (fail "scripted action failure")
              feed = do
                awaitRaw
                void (fdWrite master "a\GS\GS\GS?\GSd\GSf\GSx\GSq")
          (out, result) <- hCapture [stdout, stderr] $ withAsync feed $ \_ -> timeout 3000000 (runRawTerminalSession (\chunk -> modifyIORef' writes (++ [chunk])) (modifyIORef' closed (+ 1)) end kind action action)
          result `shouldBe` Just True
          readIORef writes `shouldReturn` ["a", BS.singleton 29]
          readIORef closed `shouldReturn` 1
          out `shouldSatisfy` isInfixOf "Escape commands"
          restored <- getTerminalAttributes stdInput
          forM_ [EnableEcho, ProcessInput, KeyboardInterrupts, ProcessOutput] $ \mode -> terminalMode mode restored `shouldBe` terminalMode mode attrs
    it "closes input and restores the terminal when the server ends the session" $ withPty $ \_ -> do
      end <- newEmptyMVar
      closed <- newIORef False
      (_, result) <- hCapture [stdout] $ withAsync (awaitRaw >> putMVar end ()) $ \_ -> timeout 3000000 (runRawTerminalSession (const (pure ())) (modifyIORef' closed (const True)) end SerialSession Nothing Nothing)
      result `shouldBe` Just True
      readIORef closed `shouldReturn` True

taskShow :: [String]
taskShow = ["Daemon.tasks", "TaskManager.get", "Task.show"]

vm :: String -> [String]
vm method = ["Daemon.vms", "VmManager.get", "Vm." ++ method]

withEnv :: String -> Maybe String -> IO a -> IO a
withEnv name value action = bracket (lookupEnv name) restore $ \_ -> restore value >> action
  where
    restore Nothing = unsetEnv name
    restore (Just text) = setEnv name text

withPty :: (Fd -> IO a) -> IO a
withPty action = bracket openPseudoTerminal (\(master, slave) -> closeFd master >> closeFd slave) $ \(master, slave) ->
  bracket (dup stdInput) (\saved -> void (dupTo saved stdInput) >> closeFd saved) $ \_ -> do
    void (dupTo slave stdInput)
    action master

awaitRaw :: IO ()
awaitRaw = do
  attrs <- getTerminalAttributes stdInput
  when (terminalMode ProcessInput attrs) $ threadDelay 1000 >> awaitRaw
