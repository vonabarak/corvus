{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Corvus.WireStructuresSpec (spec) where

import qualified Capnp.Gen.Cloudinit as CI
import qualified Capnp.Gen.Common as Common
import qualified Capnp.Gen.Enums as E
import qualified Capnp.Gen.Streams as S
import qualified Capnp.Gen.Template as T
import qualified Capnp.Gen.Vm as GV
import Corvus.DiskSelector (DiskSelector (..))
import Corvus.Model
import Corvus.Protocol (NamedRef (..))
import qualified Corvus.Protocol.Apply as A
import qualified Corvus.Protocol.Build as B
import qualified Corvus.Protocol.CloudInit as C
import qualified Corvus.Protocol.Disk as D
import qualified Corvus.Protocol.Network as N
import qualified Corvus.Protocol.SharedDir as SD
import qualified Corvus.Protocol.SshKey as K
import qualified Corvus.Protocol.Template as TP
import qualified Corvus.Protocol.Vm as V
import Corvus.Wire.Apply
import Corvus.Wire.Build
import Corvus.Wire.CloudInit
import Corvus.Wire.Disk
import Corvus.Wire.Errors
import Corvus.Wire.Network
import Corvus.Wire.SharedDir
import Corvus.Wire.SshKey
import Corvus.Wire.Template
import Corvus.Wire.Vm
import qualified Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec

spec :: Spec
spec = do
  describe "Wire streaming events" $ do
    mapM_
      (\ev -> it (show ev) $ fromCapnpApplyEvent (toCapnpApplyEvent ev) `shouldBe` Right ev)
      [A.ApplyLogLine "log", A.PhaseStart "disks" 3, A.EntityStart "vms" "web" "create", A.EntityEnd "vms" "web" TaskSuccess "done" 42, A.DownloadStart "base" "https://image", A.DownloadProgress "base" 12 24, A.DownloadEnd "base" False "failed", A.ApplyEnd TaskError "failed" 7]
    mapM_
      (\ev -> it (show ev) $ fromCapnpBuildEvent (toCapnpBuildEvent ev) `shouldBe` Right ev)
      [B.BuildLogLine "log", B.StepStart 2 "run" "echo hi", B.StepOutput 2 "hi", B.StepEnd 2 TaskSuccess Nothing, B.StepEnd 3 TaskError (Just "error"), B.BuildEnd (Left "failure"), B.BuildEnd (Right 42), B.PipelineEnd (B.BuildResult []), B.PipelineEnd (B.BuildResult [B.BuildOne "base" (Just 42) Nothing, B.BuildOne "other" Nothing (Just "failed")])]
    it "preserves all apply resource groups" $ do
      let result = A.ApplyResult [A.ApplyCreated "key" 1] [A.ApplyCreated "disk" 2] [A.ApplyCreated "net" 3] [A.ApplyCreated "vm" 4] [A.ApplyCreated "template" 5]
      fromCapnpApplyResult (toCapnpApplyResult result) `shouldBe` result
      fromCapnpApplyResult (toCapnpApplyResult (A.ApplyResult [] [] [] [] [])) `shouldBe` A.ApplyResult [] [] [] [] []
    it "normalizes empty build errors and zero artifact IDs" $
      fromCapnpBuildOneResult (S.BuildOneResult "build" 0 "") `shouldBe` Right (B.BuildOne "build" Nothing Nothing)
    it "rejects unknown event variants" $ do
      fromCapnpApplyEvent (S.ApplyEvent (S.ApplyEvent'unknown' 99)) `shouldBe` Left (WireMissingUnionVariant "ApplyEvent")
      fromCapnpBuildEvent (S.BuildEvent (S.BuildEvent'unknown' 99)) `shouldBe` Left (WireMissingUnionVariant "BuildEvent")
  describe "Wire cloud-init presence flags" $ do
    mapM_
      (\ci -> it (show ci) $ fromCapnpCloudInitInfo (toCapnpCloudInitInfo ci) `shouldBe` ci)
      [C.CloudInitInfo Nothing Nothing False, C.CloudInitInfo (Just "") (Just "") True, cloudInit]
    it "encodes explicit empty data as present" $ do
      let CI.CloudInitInfo {CI.hasUserData = present, CI.userData = content} = toCapnpCloudInitInfo (C.CloudInitInfo (Just "") Nothing False)
      (present, content) `shouldBe` (True, "")
    it "ignores payloads when their presence flags are false" $
      fromCapnpCloudInitInfo (CI.CloudInitInfo False "ignored" False "ignored" True) `shouldBe` C.CloudInitInfo Nothing Nothing True
  describe "Wire disk and auxiliary structures" $ do
    mapM_
      (\disk -> it (show disk) $ fromCapnpDiskImageInfo (toCapnpDiskImageInfo disk) `shouldBe` Right disk)
      [D.DiskImageInfo 2 ["latest", "stable"] "base" [D.DiskImagePlacement node "/base.qcow2"] FormatQcow2 (Just 1024) timestamp [vm] (Just (NamedRef 3 "parent")) True, D.DiskImageInfo 4 [] "empty" [] FormatRaw Nothing timestamp [] Nothing False]
    mapM_
      (\snapshot -> it (show snapshot) $ fromCapnpSnapshotInfo (toCapnpSnapshotInfo snapshot) `shouldBe` snapshot)
      [D.SnapshotInfo 7 "save" timestamp (Just 2048) True True True, D.SnapshotInfo 8 "offline" timestamp Nothing False False False]
    it "preserves nested cleanup reports" $ do
      let report = D.DiskCleanupReport True [D.DiskCleanupVersion vm ["old"] "removed" "unused" True [D.DiskCleanupPlacement node "/disk" "removed" "unused"]] 1 2 3
      fromCapnpDiskCleanupReport (toCapnpDiskCleanupReport report) `shouldBe` report
      fromCapnpDiskCleanupReport (toCapnpDiskCleanupReport (D.DiskCleanupReport False [] 0 0 0)) `shouldBe` D.DiskCleanupReport False [] 0 0 0
    mapM_
      (\net -> it (show net) $ fromCapnpNetworkInfo (toCapnpNetworkInfo net) `shouldBe` net)
      [N.NetworkInfo 1 "net" "10.0.0.0/24" True False True (Just 123) timestamp True (Just 456) [2, 3] ["1.1.1.1"] "example.test" True, N.NetworkInfo 2 "empty" "" False True False Nothing timestamp False Nothing [] [] "" False]
    mapM_
      (\dir -> it (show dir) $ fromCapnpSharedDirInfo (toCapnpSharedDirInfo dir) `shouldBe` Right dir)
      [sharedDir, sharedDir {SD.sdiPid = Nothing}]
    mapM_
      (\key -> it (show key) $ fromCapnpSshKeyInfo (toCapnpSshKeyInfo key) `shouldBe` key)
      [K.SshKeyInfo 3 "key" "ssh-ed25519 AAA" timestamp [vm], K.SshKeyInfo 4 "empty" "" timestamp []]
  describe "Wire malformed nested records" $ do
    it "propagates unknown graphics adapters from template summaries" $
      case toCapnpTemplateVmInfo (TP.TemplateVmInfo 1 "template" 1 1024 Nothing False False False False False GraphicsVga False False False) of
        T.TemplateVmInfo {T.id = wireId, ..} -> fromCapnpTemplateVmInfo T.TemplateVmInfo {T.id = wireId, T.graphicsAdapter = E.GraphicsAdapter'unknown' 99, ..} `shouldBe` Left (WireUnknownEnum "GraphicsAdapter" 99)
    it "propagates unknown audio backends" $
      case toCapnpAudioDeviceInfo vmAudio of
        GV.AudioDeviceInfo {GV.id = wireId, ..} -> fromCapnpAudioDeviceInfo GV.AudioDeviceInfo {GV.id = wireId, GV.backend = E.AudioBackend'unknown' 99, ..} `shouldBe` Left (WireUnknownEnum "AudioBackend" 99)
    it "propagates unknown drive interfaces from template details" $
      fromCapnpTemplateDetails ((toCapnpTemplateDetails template) {T.drives = [(toCapnpTemplateDriveInfo templateDrive) {T.interface = E.DriveInterface'unknown' 99}]}) `shouldBe` Left (WireUnknownEnum "DriveInterface" 99)
    it "normalizes absent drive media to the wire default" $
      fromCapnpDriveInfo (toCapnpDriveInfo (vmDrive {V.diMedia = Nothing})) `shouldBe` Right (vmDrive {V.diMedia = Just minBound})
    it "stores VM node references as nested IDs and names" $ do
      let GV.VmInfo {GV.node = Common.NamedRef {Common.id = ident, Common.name = name}, GV.cpuCount = cpus, GV.ram = ram} = toCapnpVmInfo (V.VmInfo 3 "web" node VmRunning 4 4096 True False True False Nothing True False "host" GraphicsVga True False True)
      (ident, name, cpus, ram) `shouldBe` (1, "node", 4, 4096)
    it "encodes build failure as failure with no artifact" $ do
      let S.BuildEvent {S.union' = variant} = toCapnpBuildEvent (B.BuildEnd (Left "failure"))
      case variant of
        S.BuildEvent'buildEnd S.BuildEvent'buildEnd' {S.success = success, S.errorMessage = message, S.artifactDiskId = artifact} -> (success, message, artifact) `shouldBe` (False, "failure", 0)
        _ -> expectationFailure "wrong event variant"
  describe "Wire templates" $ do
    mapM_
      (\summary -> it (show summary) $ fromCapnpTemplateVmInfo (toCapnpTemplateVmInfo summary) `shouldBe` Right summary)
      [TP.TemplateVmInfo 1 "template" 4 2048 (Just "description") True False True False True GraphicsQxlVga True False True, TP.TemplateVmInfo 2 "empty" 1 1024 Nothing False True False True False GraphicsVga False True False]
    mapM_
      (\drive -> it (show drive) $ fromCapnpTemplateDriveInfo (toCapnpTemplateDriveInfo drive) `shouldBe` Right drive)
      [templateDrive, templateDrive {TP.tvdiDiskImage = Nothing, TP.tvdiDiskSelector = Nothing, TP.tvdiDiskName = Nothing, TP.tvdiMedia = Nothing, TP.tvdiSize = Nothing, TP.tvdiFormat = Nothing, TP.tvdiEphemeral = Nothing}, templateDrive {TP.tvdiEphemeral = Just False}]
    it "encodes optional false separately from absence" $ do
      let T.TemplateDriveInfo {T.hasEphemeral = present, T.ephemeral = enabled, T.diskSelector = selector} = toCapnpTemplateDriveInfo (templateDrive {TP.tvdiEphemeral = Just False})
      (present, enabled, selector) `shouldBe` (True, False, "base:stable")
    it "rejects malformed disk selectors" $
      fromCapnpTemplateDriveInfo ((toCapnpTemplateDriveInfo templateDrive) {T.diskSelector = "bad:tag:extra"}) `shouldBe` Left (WireMalformed "Disk selector must be a name, name:tag, or image ID")
    mapM_
      (\nic -> it (show nic) $ fromCapnpTemplateNetIfInfo (toCapnpTemplateNetIfInfo nic) `shouldBe` Right nic)
      [templateNic, TP.TemplateNetIfInfo minBound minBound Nothing Nothing]
    it "preserves SSH key identity" $
      fromCapnpTemplateSshKeyInfo (toCapnpTemplateSshKeyInfo (TP.TemplateSshKeyInfo 9 "key")) `shouldBe` TP.TemplateSshKeyInfo 9 "key"
    it "preserves shared-directory options" $
      fromCapnpTemplateSharedDirInfo (toCapnpTemplateSharedDirInfo templateDir) `shouldBe` Right templateDir
    it "preserves audio options" $
      fromCapnpTemplateAudioDeviceInfo (toCapnpTemplateAudioDeviceInfo templateAudio) `shouldBe` Right templateAudio
    mapM_
      (\details -> it (show details) $ fromCapnpTemplateDetails (toCapnpTemplateDetails details) `shouldBe` Right details)
      [template, template {TP.tvdDescription = Nothing, TP.tvdCloudInitConfig = Nothing, TP.tvdDrives = [], TP.tvdNetIfs = [], TP.tvdSshKeys = [], TP.tvdSharedDirs = [], TP.tvdAudioDevices = []}]
  describe "Wire VM structures" $ do
    mapM_
      (\info -> it (show info) $ fromCapnpVmInfo (toCapnpVmInfo info) `shouldBe` Right info)
      [V.VmInfo 3 "web" node VmRunning 4 4096 True False True False (Just timestamp) True False "host" GraphicsVga True False True, V.VmInfo 4 "empty" node VmStopped 1 1024 False True False True Nothing False True "qemu64" GraphicsVirtioVga False True False]
    mapM_
      (\drive -> it (show drive) $ fromCapnpDriveInfo (toCapnpDriveInfo drive) `shouldBe` Right drive)
      [vmDrive, vmDrive {V.diDiskImage = Nothing}]
    mapM_
      (\nic -> it (show nic) $ fromCapnpNetIfInfo (toCapnpNetIfInfo nic) `shouldBe` Right nic)
      [vmNic, vmNic {V.niNetwork = Nothing, V.niGuestIpAddresses = Nothing, V.niIpAddress = Nothing}]
    it "preserves audio devices" $
      fromCapnpAudioDeviceInfo (toCapnpAudioDeviceInfo vmAudio) `shouldBe` Right vmAudio
    mapM_
      (\details -> it (show details) $ fromCapnpVmDetails (toCapnpVmDetails details [sharedDir] (toCapnpVmStats (V.vdStats details))) `shouldBe` Right (details, [sharedDir]))
      [vmDetails, vmDetails {V.vdDescription = Nothing, V.vdDrives = [], V.vdNetIfs = [], V.vdAudioDevices = [], V.vdSpicePort = Nothing, V.vdVsockCid = Nothing, V.vdCloudInitConfig = Nothing, V.vdHealthcheck = Nothing, V.vdErrorMessage = Nothing, V.vdLastErrorAt = Nothing}]
    it "uses the supplied cached stats sample" $
      fromCapnpVmDetails (toCapnpVmDetails vmDetails [] (toCapnpVmStats (V.zeroVmStats {V.vstHostRssBytes = 123}))) `shouldBe` Right (vmDetails {V.vdStats = V.zeroVmStats {V.vstHostRssBytes = 123}}, [])
    it "preserves full-machine snapshot identity" $ do
      let snapshot = V.VmSnapshotInfo "save" timestamp vm (NamedRef 5 "disk") 2 4096
      fromCapnpVmSnapshotInfo (toCapnpVmSnapshotInfo snapshot) `shouldBe` snapshot
  describe "Wire error diagnostics" $ do
    it "identifies unknown enum tags" $ showWireError (WireUnknownEnum "VmStatus" 99) `shouldBe` "unknown enum tag 99 for type VmStatus"
    it "identifies missing variants" $ showWireError (WireMissingUnionVariant "Event") `shouldBe` "no variant set on union Event"
    it "retains malformed-value context" $ showWireError (WireMalformed "bad input") `shouldBe` "malformed value: bad input"

node :: NamedRef
node = NamedRef 1 "node"
vm :: NamedRef
vm = NamedRef 3 "web"
timestamp :: Data.Time.UTCTime
timestamp = posixSecondsToUTCTime 1700000000
cloudInit :: C.CloudInitInfo
cloudInit = C.CloudInitInfo (Just "#cloud-config") (Just "version: 2") True
sharedDir :: SD.SharedDirInfo
sharedDir = SD.SharedDirInfo 8 "/shared" "shared" minBound True (Just 123)
templateDrive :: TP.TemplateDriveInfo
templateDrive = TP.TemplateDriveInfo (Just (NamedRef 2 "base")) (Just (ImageTag "base" "stable")) (Just "root") minBound (Just minBound) True minBound True minBound (Just 4096) (Just FormatQcow2) (Just True)
templateNic :: TP.TemplateNetIfInfo
templateNic = TP.TemplateNetIfInfo minBound NetworkE1000 (Just "bridge0") (Just "net")
templateDir :: TP.TemplateSharedDirInfo
templateDir = TP.TemplateSharedDirInfo 8 "/shared" "shared" minBound True
templateAudio :: TP.TemplateAudioDeviceInfo
templateAudio = TP.TemplateAudioDeviceInfo 9 AudioSpice AudioIntelHda "options"
template :: TP.TemplateDetails
template = TP.TemplateDetails 1 "template" 4 4096 (Just "description") True True False True False True (Just cloudInit) timestamp [templateDrive] [templateNic] [TP.TemplateSshKeyInfo 7 "key"] [templateDir] [templateAudio] GraphicsQxlVga True False True
vmDrive :: V.DriveInfo
vmDrive = V.DriveInfo 2 (Just (NamedRef 4 "disk")) minBound "/disk" FormatQcow2 (Just minBound) True minBound True
vmNic :: V.NetIfInfo
vmNic = V.NetIfInfo 3 minBound NetworkE1000 "bridge0" "aa:bb:cc:dd:ee:ff" (Just (NamedRef 4 "net")) (Just "10.0.0.2") (Just "10.0.0.3")
vmAudio :: V.AudioDeviceInfo
vmAudio = V.AudioDeviceInfo 4 AudioSpice AudioIntelHda "options"
vmDetails :: V.VmDetails
vmDetails = V.VmDetails 3 "web" node timestamp VmRunning 4 4096 (Just "description") [vmDrive] [vmNic] [vmAudio] True "/monitor" (Just 5900) (Just 7) True False True "/serial" "/guest" True False True (Just cloudInit) (Just timestamp) True (Just "failure") (Just timestamp) False "host" GraphicsQxlVga V.zeroVmStats
