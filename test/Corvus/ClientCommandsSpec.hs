{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Corvus.ClientCommandsSpec (spec) where

import qualified Capnp as C
import qualified Capnp.Gen.Common as GCommon
import qualified Capnp.Gen.Disk as GDisk
import qualified Capnp.Gen.Network as GNetwork
import qualified Capnp.Gen.Node as GNode
import qualified Capnp.Gen.Sshkey as GSshkey
import qualified Capnp.Gen.Task as GTask
import qualified Capnp.Gen.Template as GTemplate
import qualified Capnp.Gen.Vm as GVm
import Control.Monad (forM_)
import Corvus.Client.Types
import Corvus.Model (GraphicsAdapter (..))
import Data.Aeson (Value, eitherDecodeStrict', object, toJSON, (.=))
import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Yaml as Yaml
import System.Exit (ExitCode (..))
import Test.ClientCommand
import Test.ClientRpc
import Test.Hspec

spec :: Spec
spec = sequential $ describe "CLI commands over typed fake RPC" $ do
  forM_ cases $ \(cmd, calls, text) -> describe (show cmd) $ do
    forM_ [TextOutput, JsonOutput, YamlOutput] $ \fmt -> do
      it ("renders a successful response as " ++ show fmt) $ withMock $ \f -> do
        expectCalls f (forFormat fmt cmd calls)
        out <- checkCli f fmt cmd ExitSuccess
        case fmt of
          TextOutput -> out `shouldSatisfy` isInfixOf text
          JsonOutput -> (eitherDecodeStrict' (BS.pack out) :: Either String Value) `shouldSatisfy` isRight
          YamlOutput -> (Yaml.decodeEither' (BS.pack out) :: Either Yaml.ParseException Value) `shouldSatisfy` isRight
      it ("reports RPC failure as " ++ show fmt) $ withMock $ \f -> do
        expectCalls f calls
        setFailure f (Just (last calls))
        out <- checkCli f fmt cmd (ExitFailure 1)
        out `shouldSatisfy` isInfixOf "scripted RPC failure"
        case fmt of
          TextOutput -> pure ()
          _ -> decoded fmt out `shouldBe` Right (object ["status" .= ("error" :: String), "error" .= ("rpc_error" :: String), "message" .= ("scripted RPC failure" :: String)])
  describe "local validation rejects invalid input without RPC" $
    forM_ invalid $ \(cmd, message) -> forM_ [TextOutput, JsonOutput, YamlOutput] $ \fmt -> it (show (cmd, fmt)) $ withMock $ \f -> do
      expectCalls f []
      out <- checkCli f fmt cmd (ExitFailure 1)
      if fmt == TextOutput then out `shouldSatisfy` (not . null) else out `shouldSatisfy` isInfixOf message
  describe "empty list rendering" $ do
    -- Each response has its own generated type; the fixture still rejects unexpected RPCs.
    it "renders no VMs" $ emptyList VmList ["Daemon.vms", "VmManager.list"] "No VMs" $ \f -> setReply f "VmManager.list" (\(_ :: C.Parsed GVm.VmManager'list'params) -> pure (GVm.VmManager'list'results []))
    it "renders no disks" $ emptyList DiskList ["Daemon.disks", "DiskManager.list"] "No disk" $ \f -> setReply f "DiskManager.list" (\(_ :: C.Parsed GDisk.DiskManager'list'params) -> pure (GDisk.DiskManager'list'results []))
    it "renders no nodes" $ emptyList NodeList ["Daemon.nodes", "NodeManager.list"] "No nodes" $ \f -> setReply f "NodeManager.list" (\(_ :: C.Parsed GNode.NodeManager'list'params) -> pure (GNode.NodeManager'list'results []))
    it "renders no networks" $ emptyList NetworkList ["Daemon.networks", "NetworkManager.list"] "No networks" $ \f -> setReply f "NetworkManager.list" (\(_ :: C.Parsed GNetwork.NetworkManager'list'params) -> pure (GNetwork.NetworkManager'list'results []))
    it "renders no keys" $ emptyList SshKeyList ["Daemon.sshKeys", "SshKeyManager.list"] "No SSH" $ \f -> setReply f "SshKeyManager.list" (\(_ :: C.Parsed GSshkey.SshKeyManager'list'params) -> pure (GSshkey.SshKeyManager'list'results []))
    it "renders no templates" $ emptyList TemplateList ["Daemon.templates", "TemplateManager.list"] "No templates" $ \f -> setReply f "TemplateManager.list" (\(_ :: C.Parsed GTemplate.TemplateManager'list'params) -> pure (GTemplate.TemplateManager'list'results []))
    it "renders no tasks" $ emptyList (TaskList 20 Nothing Nothing False) ["Daemon.tasks", "TaskManager.list"] "No tasks" $ \f -> setReply f "TaskManager.list" (\(_ :: C.Parsed GTask.TaskManager'list'params) -> pure (GTask.TaskManager'list'results []))
    it "renders no directories" $ emptyList (SharedDirList "fixture") (vm "listSharedDirs") "No shared" $ \f -> setReply f "Vm.listSharedDirs" (\(_ :: C.Parsed GVm.Vm'listSharedDirs'params) -> pure (GVm.Vm'listSharedDirs'results []))
    it "renders no audio devices" $ emptyList (AudioDeviceList "fixture") (vm "listAudioDevices") "No audio" $ \f -> setReply f "Vm.listAudioDevices" (\(_ :: C.Parsed GVm.Vm'listAudioDevices'params) -> pure (GVm.Vm'listAudioDevices'results []))
    it "renders no interfaces" $ emptyList (NetIfList "fixture") (vm "listNetIfs") "No network" $ \f -> setReply f "Vm.listNetIfs" (\(_ :: C.Parsed GVm.Vm'listNetIfs'params) -> pure (GVm.Vm'listNetIfs'results []))
    it "renders no disk snapshots" $ emptyList (SnapshotList "fixture") (disk "snapshotList") "No snapshots" $ \f -> setReply f "Disk.snapshotList" (\(_ :: C.Parsed GDisk.Disk'snapshotList'params) -> pure (GDisk.Disk'snapshotList'results []))
    it "renders no VM snapshots" $ emptyList (VmSnapshotList "fixture") (vm "snapshotList") "No VM-scoped snapshots" $ \f -> setReply f "Vm.snapshotList" (\(_ :: C.Parsed GVm.Vm'snapshotList'params) -> pure (GVm.Vm'snapshotList'results []))
  it "preserves numeric and named entity references on the wire" $ withMock $ \f -> do
    forM_ [("42", GCommon.EntityRef (GCommon.EntityRef'id 42)), ("fixture", GCommon.EntityRef (GCommon.EntityRef'name "fixture"))] $ \(ref, expected) -> do
      expectCalls f (vm "reset")
      _ <- checkCli f JsonOutput (VmReset ref) ExitSuccess
      params <- requests @(C.Parsed GVm.VmManager'get'params) f "VmManager.get"
      params `shouldBe` [GVm.VmManager'get'params expected]
  forM_ [Nothing, Just False, Just True] $ \flag -> do
    it ("preserves optional VM booleans on the wire: " ++ show flag) $ withMock $ \f -> do
      expectCalls f (vm "edit")
      _ <- checkCli f JsonOutput (VmEdit "fixture" Nothing Nothing Nothing flag flag flag flag flag flag Nothing Nothing flag flag flag) ExitSuccess
      [GVm.Vm'edit'params params@GVm.VmEditParams {GVm.headless = headless, GVm.guestAgent = guestAgent, GVm.vsock = vsock, GVm.balloon = balloon, GVm.rng = rng, GVm.cpuCount = cpuCount, GVm.ram = ram}] <- requests f "Vm.edit"
      let present = isJust flag
          value fallback = fromMaybe fallback flag
      (GVm.hasHeadless (params :: C.Parsed GVm.VmEditParams), headless) `shouldBe` (present, value False)
      (GVm.hasGuestAgent params, guestAgent) `shouldBe` (present, value False)
      (GVm.hasVsock params, vsock) `shouldBe` (present, value True)
      (GVm.hasBalloon params, balloon) `shouldBe` (present, value True)
      (GVm.hasRng params, rng) `shouldBe` (present, value True)
      (GVm.hasCpuCount params, cpuCount, GVm.hasRam params, ram) `shouldBe` (False, 0, False, 0)
    it ("preserves optional network booleans on the wire: " ++ show flag) $ withMock $ \f -> do
      expectCalls f (resource "networks" "Network" "edit")
      _ <- checkCli f JsonOutput (NetworkEdit "fixture" Nothing flag flag flag Nothing Nothing flag) ExitSuccess
      [GNetwork.Network'edit'params params@GNetwork.NetworkEditParams {GNetwork.dhcp = dhcp, GNetwork.nat = nat, GNetwork.hostDns = hostDns, GNetwork.dnsServers = dnsServers}] <- requests f "Network.edit"
      let present = isJust flag
          value = fromMaybe False flag
      (GNetwork.hasDhcp (params :: C.Parsed GNetwork.NetworkEditParams), dhcp) `shouldBe` (present, value)
      (GNetwork.hasNat params, nat) `shouldBe` (present, value)
      (GNetwork.hasHostDns params, hostDns) `shouldBe` (present, value)
      (GNetwork.hasDnsServers params, dnsServers) `shouldBe` (False, [])
  it "does not report success for a guest command with a nonzero exit code" $ withMock $ \f -> do
    expectCalls f (vm "guestExec")
    setReply f "Vm.guestExec" (\(_ :: C.Parsed GVm.Vm'guestExec'params) -> pure (GVm.Vm'guestExec'results (GVm.GuestExecResult 3 "guest stdout" "guest stderr")))
    (code, out, err) <- runCli (cliOptions f TextOutput (VmExec "fixture" "false"))
    code `shouldBe` ExitFailure 1
    out `shouldBe` "guest stdout"
    err `shouldBe` "guest stderr"
    assertCalls f

  forM_ [TextOutput, JsonOutput, YamlOutput] $ \fmt -> it ("reports guest execution RPC failure: " ++ show fmt) $ withMock $ \f -> do
    expectCalls f (vm "guestExec")
    setFailure f (Just "Vm.guestExec")
    out <- checkCli f fmt (VmExec "fixture" "echo hello") (ExitFailure 1)
    out `shouldSatisfy` isInfixOf "scripted RPC failure"
  forM_ [TextOutput, JsonOutput, YamlOutput] $ \fmt -> it ("reports successful guest execution and preserves its payload: " ++ show fmt) $ withMock $ \f -> do
    expectCalls f (vm "guestExec")
    setReply f "Vm.guestExec" (\(_ :: C.Parsed GVm.Vm'guestExec'params) -> pure (GVm.Vm'guestExec'results (GVm.GuestExecResult 0 "hello" "")))
    out <- checkCli f fmt (VmExec "fixture" "echo hello") ExitSuccess
    if fmt == TextOutput then out `shouldBe` "hello" else decoded fmt out `shouldBe` Right (object ["status" .= ("ok" :: String), "exitcode" .= (0 :: Int), "stdout" .= ("hello" :: String), "stderr" .= ("" :: String)])

decoded :: OutputFormat -> String -> Either String Value
decoded JsonOutput output = eitherDecodeStrict' (BS.pack output)
decoded _ output = either (Left . show) Right (Yaml.decodeEither' (BS.pack output))

emptyList :: Command -> [String] -> String -> (Fixture -> IO ()) -> IO ()
emptyList cmd calls message setup = withMock $ \f -> do
  setup f
  forM_ [TextOutput, JsonOutput, YamlOutput] $ \fmt -> do
    expectCalls f calls
    out <- checkCli f fmt cmd ExitSuccess
    if fmt == TextOutput then out `shouldSatisfy` isInfixOf message else decoded fmt out `shouldBe` Right (toJSON ([] :: [Value]))

forFormat :: OutputFormat -> Command -> [String] -> [String]
forFormat TextOutput (TaskShow _) calls = calls ++ ["Daemon.tasks", "TaskManager.listChildren"]
forFormat _ _ calls = calls

vm :: String -> [String]
vm method = ["Daemon.vms", "VmManager.get", "Vm." ++ method]

disk :: String -> [String]
disk method = ["Daemon.disks", "DiskManager.get", "Disk." ++ method]

resource :: String -> String -> String -> [String]
resource getter kind method = ["Daemon." ++ getter, kind ++ "Manager.get", kind ++ "." ++ method]

created :: String -> String -> String -> [String]
created getter kind method = ["Daemon." ++ getter, kind ++ "Manager." ++ method, kind ++ ".show"]

cases :: [(Command, [String], String)]
cases =
  [ (Ping, ["Daemon.ping"], "pong")
  , (Status, ["Daemon.status"], "Uptime:")
  , (Shutdown, ["Daemon.shutdown"], "acknowledged")
  , (VmList, ["Daemon.vms", "VmManager.list"], "fixture")
  , (VmShow "fixture", vm "show", "fixture")
  , (VmCreate "new" "node" 2 1073741824 (Just "description") False True True True True False "host" GraphicsVirtioVga True True True, created "vms" "Vm" "create", "42")
  , (VmStop "fixture" (WaitOptions False Nothing), vm "stop", "stop")
  , (VmSave "fixture" (WaitOptions True Nothing), vm "save", "save")
  , (VmDelete "fixture" True True, vm "delete", "deleted")
  , (VmStart "fixture" (WaitOptions True Nothing), vm "start", "start")
  , (VmStop "fixture" (WaitOptions True (Just 7)), vm "stop", "stop")
  , (VmSave "fixture" (WaitOptions False Nothing), vm "save", "save")
  , (VmPause "fixture", vm "pause", "pause")
  , (VmReset "fixture", vm "reset", "reset")
  , (VmSetBalloon "fixture" 100, vm "setBalloon", "balloon")
  , (VmEdit "fixture" (Just 4) (Just 1024) (Just "desc") (Just False) (Just True) (Just True) (Just False) (Just False) (Just True) (Just "host") (Just GraphicsVirtioVga) (Just True) (Just False) (Just True), vm "edit", "updated")
  , (VmMigrate "fixture" "node", vm "migrate", "42")
  , (DiskCreate "new" "qcow2" 1024 (Just "/disk") True "node", created "disks" "Disk" "create", "42")
  , (DiskRegisterCmd "new" "/disk" (Just "raw") (Just "base") True "node", created "disks" "Disk" "register", "42")
  , (DiskImport "new" "/source" (Just "/disk") (Just "raw") False "node" (WaitOptions False Nothing), ["Daemon.disks", "DiskManager.import"], "42")
  , (DiskCreateOverlay "new" "base" (Just "/disk") True, created "disks" "Disk" "createOverlay", "42")
  , (DiskRegisterCmd "new" "/disk" Nothing Nothing False "", created "disks" "Disk" "register", "42")
  , (DiskImport "new" "/source" Nothing Nothing True "" (WaitOptions True (Just 0)), ["Daemon.disks", "DiskManager.import", "Daemon.tasks", "TaskManager.get", "Task.show"], "success")
  , (DiskRebase "fixture" Nothing False, ["Daemon.disks", "DiskManager.flatten"], "flattened")
  , (DiskAttach "fixture" "disk" "virtio" Nothing False False "none", vm "attachDisk", "42")
  , (DiskRefresh "fixture", disk "refresh", "refreshed")
  , (DiskCleanup (Just "old") "node" True True, ["Daemon.disks", "DiskManager.cleanup"], "fixture")
  , (DiskDelete "fixture", disk "delete", "deleted")
  , (DiskResize "fixture" 4096, disk "resize", "resized")
  , (DiskRegisterPlacement "fixture" "node" "/disk", disk "registerPlacement", "registered")
  , (DiskTag "fixture" "stable", disk "tag", "updated")
  , (DiskUntag "fixture" "stable", disk "untag", "updated")
  , (DiskList, ["Daemon.disks", "DiskManager.list"], "fixture")
  , (DiskShow "fixture", disk "show", "fixture")
  , (DiskClone "new" "base" (Just "/disk") True, created "disks" "Disk" "clone", "42")
  , (DiskRebase "fixture" (Just "base") True, ["Daemon.disks", "DiskManager.rebase"], "rebased")
  , (DiskAttach "fixture" "disk" "virtio" (Just "disk") True True "writeback", vm "attachDisk", "42")
  , (DiskDetach "fixture" "fixture", vm "show" ++ vm "detachDisk", "detached")
  , (DiskMediaEject 42, ["Daemon.disks", "DiskManager.mediaEject"], "ejected")
  , (DiskMediaChange 42 "disk", ["Daemon.disks", "DiskManager.mediaChange"], "changed")
  , (DiskCopy "fixture" "node" (Just "/copy") True, ["Daemon.disks", "DiskManager.copy"], "42")
  , (DiskMove "fixture" "node" (Just "/move") False, ["Daemon.disks", "DiskManager.move"], "42")
  , (SharedDirAdd "fixture" "/shared" "tag" "auto" True, vm "addSharedDir", "42")
  , (SharedDirRemove "fixture" "42", vm "removeSharedDir", "removed")
  , (SharedDirList "fixture", vm "listSharedDirs", "fixture")
  , (AudioDeviceAdd "fixture" "spice" "" "virtio-sound", vm "addAudioDevice", "42")
  , (AudioDeviceEdit "fixture" 42 "spice" "opts" (Just "intel-hda"), vm "editAudioDevice", "updated")
  , (AudioDeviceRemove "fixture" 42, vm "removeAudioDevice", "removed")
  , (AudioDeviceList "fixture", vm "listAudioDevices", "fixture")
  , (NetIfAdd "fixture" "tap" "tap0" (Just "52:54:00:11:22:33") (Just "lan") "virtio-net-pci", vm "addNetIf", "42")
  , (NetIfEdit "fixture" 42 "virtio-net-pci", vm "editNetIf", "updated")
  , (NetIfRemove "fixture" 42, vm "removeNetIf", "removed")
  , (NetIfList "fixture", vm "listNetIfs", "fixture")
  , (SnapshotCreate "fixture" "snap" QuiesceFlagRequire True, disk "snapshotCreate", "42")
  , (SnapshotDelete "fixture" "snap", disk "snapshotGet" ++ ["Snapshot.delete"], "deleted")
  , (SnapshotRollback "fixture" "snap" True, disk "snapshotGet" ++ ["Snapshot.rollback"], "Rollback")
  , (SnapshotMerge "fixture" "snap", disk "snapshotGet" ++ ["Snapshot.merge"], "merged")
  , (SnapshotList "fixture", disk "snapshotList", "fixture")
  , (VmSnapshotCreate "fixture" "snap", vm "snapshotCreate", "42")
  , (VmSnapshotList "fixture", vm "snapshotList", "fixture")
  , (VmSnapshotRollback "fixture" "snap", vm "snapshotRollback", "Rolled back")
  , (VmSnapshotDelete "fixture" "snap", vm "snapshotDelete", "deleted")
  , (SshKeyCreate "new" "ssh-ed25519 key", created "sshKeys" "SshKey" "create", "42")
  , (SshKeyDelete "fixture", resource "sshKeys" "SshKey" "delete", "deleted")
  , (SshKeyList, ["Daemon.sshKeys", "SshKeyManager.list"], "fixture")
  , (SshKeyAttach "fixture" "key", vm "attachSshKey", "attached")
  , (SshKeyDetach "fixture" "key", vm "detachSshKey", "detached")
  , (SshKeyListForVm "fixture", vm "listSshKeys", "fixture")
  , (TemplateDelete "fixture", resource "templates" "Template" "delete", "deleted")
  , (TemplateShow "fixture", resource "templates" "Template" "show", "fixture")
  , (TemplateList, ["Daemon.templates", "TemplateManager.list"], "fixture")
  , (TemplateInstantiate "fixture" "new" "node", resource "templates" "Template" "instantiate" ++ ["Vm.show"], "42")
  , (NetworkCreate "lan" "node" "10.0.0.0/24" True True True ["1.1.1.1"] "lan" True, created "networks" "Network" "create", "42")
  , (NetworkDelete "fixture", resource "networks" "Network" "delete", "deleted")
  , (NetworkStart "fixture", resource "networks" "Network" "start", "started")
  , (NetworkStop "fixture" True, resource "networks" "Network" "stop", "stopped")
  , (NetworkList, ["Daemon.networks", "NetworkManager.list"], "fixture")
  , (NetworkShow "fixture", resource "networks" "Network" "show", "fixture")
  , (NetworkEdit "fixture" (Just "10.1.0.0/24") (Just False) (Just True) (Just False) (Just []) (Just "") (Just False), resource "networks" "Network" "edit", "updated")
  , (NetworkAttachNode "fixture" "node", resource "networks" "Network" "attachNode", "attached")
  , (NetworkDetachNode "fixture" "node", resource "networks" "Network" "detachNode", "detached")
  , (NodeAdd "new" "host" 1234 1235 (Just "/vms") (Just "desc") "online" True, created "nodes" "Node" "create", "42")
  , (NodeList, ["Daemon.nodes", "NodeManager.list"], "fixture")
  , (NodeShow "fixture", resource "nodes" "Node" "show", "fixture")
  , (NodeEdit "fixture" (Just "new") (Just "host") (Just 1234) (Just 1235) (Just "/vms") (Just Nothing) (Just "maintenance") (Just False), resource "nodes" "Node" "edit", "updated")
  , (NodeDrain "fixture", resource "nodes" "Node" "drain", "draining")
  , (NodeDelete "fixture", resource "nodes" "Node" "delete", "deleted")
  , (CloudInitGenerate "fixture", vm "cloudInit", "generated")
  , (CloudInitShow "fixture", ["Daemon.cloudInit", "CloudInitManager.get"], "fixture")
  , (CloudInitDelete "fixture", ["Daemon.cloudInit", "CloudInitManager.delete"], "deleted")
  , (TaskList 20 (Just "vm") (Just "success") True, ["Daemon.tasks", "TaskManager.list"], "fixture")
  , (TaskShow 42, ["Daemon.tasks", "TaskManager.get", "Task.show"], "fixture")
  , (TaskWait 42 Nothing, ["Daemon.tasks", "TaskManager.get", "Task.show"], "success")
  , (TaskCancel 42, ["Daemon.tasks", "TaskManager.cancel"], "42")
  ]

invalid :: [(Command, String)]
invalid =
  [ (DiskCreate "new" "bad" 1 Nothing False "", "invalid_format")
  , (DiskRegisterCmd "new" "/disk" (Just "bad") Nothing False "", "invalid_format")
  , (DiskImport "new" "/source" Nothing (Just "bad") False "" (WaitOptions False Nothing), "invalid_format")
  , (DiskUpload "new" "/source" "bad" Nothing False "" "error", "invalid_format")
  , (DiskUpload "new" "/source" "raw" Nothing False "" "bad", "invalid_format")
  , (DiskAttach "vm" "disk" "bad" Nothing False False "writeback", "invalid_interface")
  , (DiskAttach "vm" "disk" "virtio" Nothing False False "bad", "invalid_cache_type")
  , (DiskAttach "vm" "disk" "virtio" (Just "bad") False False "writeback", "invalid_media")
  , (SharedDirAdd "vm" "/shared" "tag" "bad" False, "invalid_cache")
  , (SharedDirRemove "vm" "bad", "bad_id")
  , (AudioDeviceAdd "vm" "bad" "" "virtio-sound", "invalid_audio_backend")
  , (AudioDeviceAdd "vm" "spice" "" "bad", "invalid_audio_model")
  , (AudioDeviceEdit "vm" 42 "spice" "" (Just "bad"), "invalid_audio_model")
  , (NetIfAdd "vm" "bad" "" Nothing Nothing "virtio-net-pci", "invalid_interface_type")
  , (NetIfAdd "vm" "tap" "" Nothing Nothing "bad", "invalid_network_model")
  , (NetIfEdit "vm" 42 "bad", "invalid_network_model")
  , (NodeAdd "node" "host" 1 2 Nothing Nothing "bad" False, "bad_arg")
  , (NodeEdit "node" Nothing Nothing Nothing Nothing Nothing Nothing (Just "bad") Nothing, "bad_arg")
  , (TaskList 10 (Just "bad") Nothing False, "invalid_argument")
  , (TaskList 10 Nothing (Just "bad") False, "invalid_argument")
  ]
