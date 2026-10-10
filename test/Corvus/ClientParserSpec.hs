{-# LANGUAGE OverloadedStrings #-}

module Corvus.ClientParserSpec (spec) where

import Control.Exception (bracket)
import Corvus.Client.Parser
import Corvus.Client.Types
import Corvus.Model (GraphicsAdapter (..))
import Data.List (isInfixOf, nub)
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, prefs, renderFailure, showHelpOnError)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "CLI command arguments" $ mapM_ commandCase commands
  describe "CLI global options" $ do
    it "uses the default connection and display settings" $ parsed defaults ["ping"] $ \o -> do
      (optSocket o, optUnix o, optHost o, optPort o) `shouldBe` (Nothing, False, "127.0.0.1", 9876)
      (optOutput o, optBorders o, optTruncate o, optColumns o, optFitWidth o) `shouldBe` (TextOutput, BordersUnicodeOpt, True, [], True)
      (optNoTls o, optTlsCertDir o) `shouldBe` (False, Nothing)
    it "overrides supplied defaults and parses display and TLS options"
      $ parsed
        (ClientDefaults (Just "/env.sock") "env-host" 1234)
        ["--socket", "/cli.sock", "--unix", "-H", "cli-host", "-p", "4321", "-o", "JSON", "--borders", "ASCII", "--no-truncate", "--columns", ",id,,name,", "--no-fit", "--no-tls", "--tls-cert-dir", "/certs", "ping"]
      $ \o -> do
        (optSocket o, optUnix o, optHost o, optPort o) `shouldBe` (Just "/cli.sock", True, "cli-host", 4321)
        (optOutput o, optBorders o, optTruncate o, optColumns o, optFitWidth o) `shouldBe` (JsonOutput, BordersAsciiOpt, False, ["id", "name"], False)
        (optNoTls o, optTlsCertDir o) `shouldBe` (True, Just "/certs")
    it "preserves supplied defaults when flags are absent" $ parsed (ClientDefaults (Just "/env.sock") "env-host" 1234) ["ping"] $ \o ->
      (optSocket o, optHost o, optPort o) `shouldBe` (Just "/env.sock", "env-host", 1234)
    mapM_
      (\(raw, expected) -> it ("parses output " <> raw) $ parsed defaults ["--output", raw, "ping"] $ \o -> optOutput o `shouldBe` expected)
      [("text", TextOutput), ("yaml", YamlOutput), ("json", JsonOutput)]
    mapM_
      (\(args, expected) -> it ("parses borders " <> unwords args) $ parsed defaults (args <> ["ping"]) $ \o -> optBorders o `shouldBe` expected)
      [(["--borders", "unicode"], BordersUnicodeOpt), (["--borders", "none"], BordersNoneOpt), (["--no-borders"], BordersNoneOpt)]
    it "accepts an empty columns selection" $ parsed defaults ["--columns", "", "ping"] $ \o -> optColumns o `shouldBe` []
  describe "CLI argument rejection" $ mapM_ (\(args, message) -> it ("rejects " <> unwords args) $ rejected args message) failures
  describe "CLI help" $
    mapM_
      ( \group -> it ("renders help for " <> unwords group) $
          case execParserPure defaultPrefs (optsInfo defaults) (group <> ["--help"]) of
            Failure failure -> do
              let (message, code) = renderFailure failure "crv"
              code `shouldBe` (if null group then ExitSuccess else ExitFailure 1)
              message `shouldSatisfy` isInfixOf "Usage:"
            _ -> expectationFailure "expected help"
      )
      [[], ["vm"], ["disk"], ["disk", "snapshot"], ["network"], ["node"], ["build"]]
  describe "CLI command usage diagnostics" $ mapM_ usageCase (nub (map (commandPath . fst) commands))
  describe "CLI environment defaults" $ sequential $ do
    it "uses defaults without environment variables" $ withEnvironment [] $ do
      d <- readClientDefaults
      (cdSocket d, cdHost d, cdPort d) `shouldBe` (Nothing, "127.0.0.1", 9876)
    it "reads socket, host and port" $ withEnvironment [("CORVUS_SOCKET", "/env.sock"), ("CORVUS_HOST", "remote"), ("CORVUS_PORT", "1234")] $ do
      d <- readClientDefaults
      (cdSocket d, cdHost d, cdPort d) `shouldBe` (Just "/env.sock", "remote", 1234)
    it "falls back for an invalid port" $ withEnvironment [("CORVUS_PORT", "bad")] $ do
      d <- readClientDefaults
      cdPort d `shouldBe` 9876
  describe "CLI boolean spellings" $
    mapM_
      (\(raw, expected) -> commandCase (["network", "edit", "net", "--dhcp", raw], NetworkEdit "net" Nothing (Just expected) Nothing Nothing Nothing Nothing Nothing))
      [("TRUE", True), ("false", False), ("yes", True), ("NO", False), ("1", True), ("0", False)]

-- Command has no Eq instance. Comparing its derived representation checks
-- every field without changing the production API just for tests.
commandCase :: ([String], Command) -> Spec
commandCase (args, expected) = it (unwords args) $ parsed defaults args $ \o -> show (optCommand o) `shouldBe` show expected

-- Rendering the complete contextual help exercises argument names and option
-- descriptions as well as parsing; checking only the leading "Usage:" is lazy.
commandPath :: [String] -> [String]
commandPath args = case args of
  family : sub : _ | (family, sub) `elem` [("vm", "snapshot"), ("disk", "snapshot"), ("disk", "media")] -> take 3 args
  family : _ | family `elem` ["vm", "disk", "network", "node", "ssh-key", "net-if", "shared-dir", "audio-device", "template", "cloud-init", "task"] -> take 2 args
  _ -> take 1 args

usageCase :: [String] -> Spec
usageCase args = it ("renders complete usage for " <> unwords args) $
  case execParserPure (prefs showHelpOnError) (optsInfo defaults) (args <> ["--invalid-test-option"]) of
    Failure failure -> do
      let (message, code) = renderFailure failure "crv"
      code `shouldBe` ExitFailure 1
      message `shouldSatisfy` isInfixOf "--invalid-test-option"
      message `shouldSatisfy` isInfixOf "Usage:"
      length message `shouldSatisfy` (> 80)
    _ -> expectationFailure "expected contextual usage diagnostic"

parsed :: ClientDefaults -> [String] -> (Options -> Expectation) -> Expectation
parsed defs args check = case execParserPure defaultPrefs (optsInfo defs) args of
  Success options -> check options
  Failure failure -> expectationFailure (fst (renderFailure failure "crv"))
  CompletionInvoked _ -> expectationFailure "unexpected completion"

rejected :: [String] -> String -> Expectation
rejected args expected = case execParserPure defaultPrefs (optsInfo defaults) args of
  Failure failure -> do
    let (message, code) = renderFailure failure "crv"
    code `shouldBe` ExitFailure 1
    message `shouldSatisfy` isInfixOf expected
  _ -> expectationFailure "expected argument rejection"

defaults :: ClientDefaults
defaults = ClientDefaults Nothing "127.0.0.1" 9876

withEnvironment :: [(String, String)] -> IO a -> IO a
withEnvironment values action =
  bracket
    (mapM (\name -> (,) name <$> lookupEnv name) names)
    (mapM_ (\(name, value) -> maybe (unsetEnv name) (setEnv name) value))
    (\_ -> mapM_ unsetEnv names >> mapM_ (uncurry setEnv) values >> action)
  where
    names = ["CORVUS_SOCKET", "CORVUS_HOST", "CORVUS_PORT"]

commands :: [([String], Command)]
commands =
  [ (["ping"], Ping)
  , (["status"], Status)
  , (["shutdown"], Shutdown)
  , (["completion", "bash"], Completion "bash")
  , (["vm", "list"], VmList)
  , (["vm", "show", "web"], VmShow "web")
  , (["vm", "create", "web"], VmCreate "web" "" 1 1073741824 Nothing False False False False False False "host" GraphicsVirtioVga True True True)
  , (["vm", "create", "web", "--node", "host", "--cpus", "4", "--ram", "2G", "--description", "server", "--headless", "--guest-agent", "--tpm", "--cloud-init", "--autostart", "--reboot-quirk", "--cpu-model", "qemu64", "--graphics-adapter", "qxl-vga", "--no-vsock", "--no-balloon", "--no-rng"], VmCreate "web" "host" 4 2147483648 (Just "server") True True True True True True "qemu64" GraphicsQxlVga False False False)
  , (["vm", "delete", "web"], VmDelete "web" False False)
  , (["vm", "delete", "web", "--keep-disks", "--force"], VmDelete "web" True True)
  , (["vm", "start", "web"], VmStart "web" (WaitOptions False Nothing))
  , (["vm", "start", "web", "--wait"], VmStart "web" (WaitOptions True Nothing))
  , (["vm", "stop", "web"], VmStop "web" (WaitOptions False Nothing))
  , (["vm", "stop", "web", "-w", "-t", "0"], VmStop "web" (WaitOptions True (Just 0)))
  , (["vm", "pause", "web"], VmPause "web")
  , (["vm", "balloon", "web", "768M"], VmSetBalloon "web" 805306368)
  , (["vm", "balloon", "web", "18446744073709551615B"], VmSetBalloon "web" 18446744073709551615)
  , (["vm", "reset", "web"], VmReset "web")
  , (["vm", "save", "web"], VmSave "web" (WaitOptions False Nothing))
  , (["vm", "save", "web", "-w"], VmSave "web" (WaitOptions True Nothing))
  , (["vm", "edit", "web"], VmEdit "web" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  , (["vm", "edit", "web", "--cpus", "2", "--ram", "512M", "--description", "new", "--headless", "true", "--guest-agent", "false", "--tpm", "yes", "--cloud-init", "no", "--autostart", "1", "--reboot-quirk", "0", "--cpu-model", "qemu64", "--graphics-adapter", "vga", "--vsock", "false", "--balloon", "true", "--rng", "false"], VmEdit "web" (Just 2) (Just 536870912) (Just "new") (Just True) (Just False) (Just True) (Just False) (Just True) (Just False) (Just "qemu64") (Just GraphicsVga) (Just False) (Just True) (Just False))
  , (["vm", "view", "web"], VmView "web")
  , (["vm", "monitor", "web"], VmMonitor "web")
  , (["vm", "exec", "web", "uname -a"], VmExec "web" "uname -a")
  , (["vm", "migrate", "web", "--to-node", "host"], VmMigrate "web" "host")
  , (["vm", "snapshot", "create", "web", "snap"], VmSnapshotCreate "web" "snap")
  , (["vm", "snapshot", "list", "web"], VmSnapshotList "web")
  , (["vm", "snapshot", "rollback", "web", "snap"], VmSnapshotRollback "web" "snap")
  , (["vm", "snapshot", "delete", "web", "snap"], VmSnapshotDelete "web" "snap")
  , (["disk", "create", "image", "--size", "1G"], DiskCreate "image" "qcow2" 1073741824 Nothing False "")
  , (["disk", "create", "image", "-s", "9223372036854775807B", "-f", "raw", "--path", "/images/", "--ephemeral", "--node", "host"], DiskCreate "image" "raw" 9223372036854775807 (Just "/images/") True "host")
  , (["disk", "cleanup", "image"], DiskCleanup (Just "image") "" False False)
  , (["disk", "cleanup", "--all", "--node", "host", "--include-tagged", "--dry-run"], DiskCleanup Nothing "host" True True)
  , (["disk", "tag", "image", "stable"], DiskTag "image" "stable")
  , (["disk", "untag", "image", "stable"], DiskUntag "image" "stable")
  , (["disk", "register", "image", "/disk"], DiskRegisterCmd "image" "/disk" Nothing Nothing False "")
  , (["disk", "register", "image", "/disk", "--format", "qcow2", "--backing", "base", "--ephemeral", "--node", "host"], DiskRegisterCmd "image" "/disk" (Just "qcow2") (Just "base") True "host")
  , (["disk", "register-placement", "image", "--node", "host", "/disk"], DiskRegisterPlacement "image" "host" "/disk")
  , (["disk", "import", "image", "https://example.test/disk"], DiskImport "image" "https://example.test/disk" Nothing Nothing False "" (WaitOptions False Nothing))
  , (["disk", "import", "image", "/source", "--path", "/dest", "--format", "raw", "--ephemeral", "--node", "host", "--wait"], DiskImport "image" "/source" (Just "/dest") (Just "raw") True "host" (WaitOptions True Nothing))
  , (["disk", "upload", "image", "/local", "--format", "raw"], DiskUpload "image" "/local" "raw" Nothing False "" "overwrite")
  , (["disk", "upload", "image", "/local", "--format", "qcow2", "--path", "/dest", "--ephemeral", "--node", "host", "--if-exists", "skip"], DiskUpload "image" "/local" "qcow2" (Just "/dest") True "host" "skip")
  , (["disk", "overlay", "image", "base"], DiskCreateOverlay "image" "base" Nothing False)
  , (["disk", "overlay", "image", "base", "--path", "/dest", "--ephemeral"], DiskCreateOverlay "image" "base" (Just "/dest") True)
  , (["disk", "clone", "image", "base"], DiskClone "image" "base" Nothing False)
  , (["disk", "clone", "image", "base", "--path", "/dest", "--ephemeral"], DiskClone "image" "base" (Just "/dest") True)
  , (["disk", "delete", "image"], DiskDelete "image")
  , (["disk", "refresh", "image"], DiskRefresh "image")
  , (["disk", "resize", "image", "--size", "2G"], DiskResize "image" 2147483648)
  , (["disk", "list"], DiskList)
  , (["disk", "show", "image"], DiskShow "image")
  , (["disk", "rebase", "image"], DiskRebase "image" Nothing False)
  , (["disk", "rebase", "image", "--backing", "base", "--unsafe"], DiskRebase "image" (Just "base") True)
  , (["disk", "attach", "web", "image"], DiskAttach "web" "image" "virtio" Nothing False False "writeback")
  , (["disk", "attach", "web", "image", "-i", "ide", "-m", "cdrom", "--read-only", "--discard", "--cache", "none"], DiskAttach "web" "image" "ide" (Just "cdrom") True True "none")
  , (["disk", "detach", "web", "image"], DiskDetach "web" "image")
  , (["disk", "media", "eject", "7"], DiskMediaEject 7)
  , (["disk", "media", "change", "7", "image"], DiskMediaChange 7 "image")
  , (["disk", "copy", "image", "--to-node", "host"], DiskCopy "image" "host" Nothing False)
  , (["disk", "copy", "image", "--to-node", "host", "--to-path", "/dest", "--with-backing-chain"], DiskCopy "image" "host" (Just "/dest") True)
  , (["disk", "move", "image", "--to-node", "host"], DiskMove "image" "host" Nothing False)
  , (["disk", "move", "image", "--to-node", "host", "--to-path", "/dest", "--with-backing-chain"], DiskMove "image" "host" (Just "/dest") True)
  , (["disk", "snapshot", "create", "image", "snap"], SnapshotCreate "image" "snap" QuiesceFlagAuto False)
  , (["disk", "snapshot", "create", "image", "snap", "--quiesce", "require", "--with-ram"], SnapshotCreate "image" "snap" QuiesceFlagRequire True)
  , (["disk", "snapshot", "create", "image", "snap", "--quiesce", "skip"], SnapshotCreate "image" "snap" QuiesceFlagSkip False)
  , (["disk", "snapshot", "create", "image", "snap", "--quiesce", "auto"], SnapshotCreate "image" "snap" QuiesceFlagAuto False)
  , (["disk", "snapshot", "delete", "image", "snap"], SnapshotDelete "image" "snap")
  , (["disk", "snapshot", "rollback", "image", "snap"], SnapshotRollback "image" "snap" False)
  , (["disk", "snapshot", "rollback", "image", "snap", "--auto-stop"], SnapshotRollback "image" "snap" True)
  , (["disk", "snapshot", "merge", "image", "snap"], SnapshotMerge "image" "snap")
  , (["disk", "snapshot", "list", "image"], SnapshotList "image")
  , (["shared-dir", "add", "web", "/data", "data"], SharedDirAdd "web" "/data" "data" "auto" False)
  , (["shared-dir", "add", "web", "/data", "data", "--cache", "never", "--read-only"], SharedDirAdd "web" "/data" "data" "never" True)
  , (["shared-dir", "remove", "web", "data"], SharedDirRemove "web" "data")
  , (["shared-dir", "list", "web"], SharedDirList "web")
  , (["audio-device", "add", "web", "pulse"], AudioDeviceAdd "web" "pulse" "" "virtio-sound")
  , (["audio-device", "add", "web", "spice", "--options", "in=on", "--model", "intel-hda"], AudioDeviceAdd "web" "spice" "in=on" "intel-hda")
  , (["audio-device", "edit", "web", "7", "pipewire"], AudioDeviceEdit "web" 7 "pipewire" "" Nothing)
  , (["audio-device", "edit", "web", "7", "pulse", "--options", "in=on", "--model", "AC97"], AudioDeviceEdit "web" 7 "pulse" "in=on" (Just "AC97"))
  , (["audio-device", "remove", "web", "7"], AudioDeviceRemove "web" 7)
  , (["audio-device", "list", "web"], AudioDeviceList "web")
  , (["net-if", "add", "web"], NetIfAdd "web" "user" "" Nothing Nothing "virtio-net-pci")
  , (["net-if", "add", "web", "--type", "managed", "--host-device", "br0", "--mac", "52:54:00:12:34:56", "--network", "net", "--model", "e1000"], NetIfAdd "web" "managed" "br0" (Just "52:54:00:12:34:56") (Just "net") "e1000")
  , (["net-if", "edit", "web", "7", "--model", "e1000"], NetIfEdit "web" 7 "e1000")
  , (["net-if", "remove", "web", "7"], NetIfRemove "web" 7)
  , (["net-if", "list", "web"], NetIfList "web")
  , (["ssh-key", "create", "key", "ssh-ed25519 abc"], SshKeyCreate "key" "ssh-ed25519 abc")
  , (["ssh-key", "delete", "key"], SshKeyDelete "key")
  , (["ssh-key", "list"], SshKeyList)
  , (["ssh-key", "attach", "web", "key"], SshKeyAttach "web" "key")
  , (["ssh-key", "detach", "web", "key"], SshKeyDetach "web" "key")
  , (["ssh-key", "list-vm", "web"], SshKeyListForVm "web")
  , (["template", "create"], TemplateCreate Nothing)
  , (["template", "create", "template.yaml"], TemplateCreate (Just "template.yaml"))
  , (["template", "edit", "tpl"], TemplateEdit "tpl")
  , (["template", "delete", "tpl"], TemplateDelete "tpl")
  , (["template", "list"], TemplateList)
  , (["template", "show", "tpl"], TemplateShow "tpl")
  , (["template", "instantiate", "tpl", "web"], TemplateInstantiate "tpl" "web" "")
  , (["template", "instantiate", "tpl", "web", "--node", "host"], TemplateInstantiate "tpl" "web" "host")
  , (["network", "create", "net"], NetworkCreate "net" "" "" False False False [] "" True)
  , (["network", "create", "net", "--node", "host", "--subnet", "10.0.0.0/24", "--dhcp", "--nat", "--autostart", "--dns-server", "1.1.1.1", "--dns-server", "8.8.8.8", "--domain", "test", "--no-host-dns"], NetworkCreate "net" "host" "10.0.0.0/24" True True True ["1.1.1.1", "8.8.8.8"] "test" False)
  , (["network", "delete", "net"], NetworkDelete "net")
  , (["network", "start", "net"], NetworkStart "net")
  , (["network", "stop", "net"], NetworkStop "net" False)
  , (["network", "stop", "net", "--force"], NetworkStop "net" True)
  , (["network", "list"], NetworkList)
  , (["network", "show", "net"], NetworkShow "net")
  , (["network", "edit", "net"], NetworkEdit "net" Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  , (["network", "edit", "net", "--subnet", "10.0.1.0/24", "--dhcp", "true", "--nat", "false", "--autostart", "yes", "--dns-server", "1.1.1.1", "--domain", "test", "--host-dns", "no"], NetworkEdit "net" (Just "10.0.1.0/24") (Just True) (Just False) (Just True) (Just ["1.1.1.1"]) (Just "test") (Just False))
  , (["network", "attach-node", "net", "host"], NetworkAttachNode "net" "host")
  , (["network", "detach-node", "net", "host"], NetworkDetachNode "net" "host")
  , (["node", "add", "host", "--host", "10.0.0.1"], NodeAdd "host" "10.0.0.1" 9878 9877 Nothing Nothing "online" False)
  , (["node", "add", "host", "--host", "10.0.0.1", "--node-agent-port", "1111", "--net-agent-port", "2222", "--base-path", "/images", "--description", "server", "--admin-state", "draining", "--netd-disabled"], NodeAdd "host" "10.0.0.1" 1111 2222 (Just "/images") (Just "server") "draining" True)
  , (["node", "list"], NodeList)
  , (["node", "show", "host"], NodeShow "host")
  , (["node", "edit", "host"], NodeEdit "host" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  , (["node", "edit", "host", "--name", "new", "--host", "10.0.0.2", "--node-agent-port", "1111", "--net-agent-port", "2222", "--base-path", "/images", "--description", "server", "--admin-state", "maintenance", "--netd-disabled", "true"], NodeEdit "host" (Just "new") (Just "10.0.0.2") (Just 1111) (Just 2222) (Just "/images") (Just (Just "server")) (Just "maintenance") (Just True))
  , (["node", "edit", "host", "--description", ""], NodeEdit "host" Nothing Nothing Nothing Nothing Nothing (Just Nothing) Nothing Nothing)
  , (["node", "drain", "host"], NodeDrain "host")
  , (["node", "delete", "host"], NodeDelete "host")
  , (["cloud-init", "generate", "web"], CloudInitGenerate "web")
  , (["cloud-init", "set", "web"], CloudInitSet "web" Nothing)
  , (["cloud-init", "set", "web", "ci.yaml"], CloudInitSet "web" (Just "ci.yaml"))
  , (["cloud-init", "edit", "web"], CloudInitEdit "web")
  , (["cloud-init", "show", "web"], CloudInitShow "web")
  , (["cloud-init", "delete", "web"], CloudInitDelete "web")
  , (["apply", "env.yaml"], Apply "env.yaml" False (WaitOptions False Nothing))
  , (["apply", "env.yaml", "--skip-existing", "--wait"], Apply "env.yaml" True (WaitOptions True Nothing))
  , (["build", "build.yaml"], Build "build.yaml" (BuildClientOptions [] []) (WaitOptions False Nothing))
  , (["build", "build.yaml", "--var", "Name_1=a=b", "--var", "_empty=", "--var-file", "a.yaml", "--var-file", "b.yaml", "--wait"], Build "build.yaml" (BuildClientOptions [("Name_1", "a=b"), ("_empty", "")] ["a.yaml", "b.yaml"]) (WaitOptions True Nothing))
  , (["task", "list"], TaskList 10 Nothing Nothing False)
  , (["task", "list", "--last", "20", "--subsystem", "vm", "--result", "error", "--all"], TaskList 20 (Just "vm") (Just "error") True)
  , (["task", "show", "7"], TaskShow 7)
  , (["task", "cancel", "7"], TaskCancel 7)
  , (["task", "wait", "7"], TaskWait 7 Nothing)
  , (["task", "wait", "7", "--timeout", "30"], TaskWait 7 (Just 30))
  ]

failures :: [([String], String)]
failures =
  [ ([], "Missing")
  , (["unknown"], "Invalid argument")
  , (["vm", "unknown"], "Invalid argument")
  , (["vm", "show"], "Missing")
  , (["disk", "upload", "image", "/local"], "Missing")
  , (["--output", "xml", "ping"], "Unknown output format")
  , (["--borders", "round", "ping"], "Unknown border style")
  , (["--port", "abc", "ping"], "cannot parse")
  , (["vm", "edit", "web", "--headless", "maybe"], "Invalid boolean")
  , (["vm", "create", "web", "--graphics-adapter", "bad"], "graphics")
  , (["vm", "stop", "web", "--timeout", "-1"], "non-negative integer")
  , (["vm", "stop", "web", "--timeout", "oops"], "non-negative integer")
  , (["disk", "snapshot", "create", "image", "snap", "--quiesce", "bad"], "expected auto|require|skip")
  , (["build", "file", "--var", "no-equals"], "expected KEY=VALUE")
  , (["build", "file", "--var", "=value"], "expected KEY=VALUE")
  , (["build", "file", "--var", "1name=value"], "expected KEY=VALUE")
  , (["build", "file", "--var", "bad-name=value"], "expected KEY=VALUE")
  , (["disk", "create", "image", "--size", "9223372036854775808B"], "range")
  , (["vm", "balloon", "web", "18446744073709551616B"], "range")
  , (["vm", "balloon", "web", "-1B"], "Invalid")
  , (["disk", "create", "image", "--size", "10"], "suffix")
  , (["vm", "create", "web", "--ram", "1B"], "whole multiple")
  , (["task", "show", "oops"], "cannot parse")
  ]
