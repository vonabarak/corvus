{-# LANGUAGE OverloadedStrings #-}

module Corvus.BuildIdentitySpec (spec) where

import Corvus.Build.Identity (buildInputIdentity)
import Corvus.DiskSelector (DiskSelector (..))
import Corvus.Model
import Corvus.Protocol (NamedRef (..))
import Corvus.Protocol.Template
import Corvus.Schema.Build
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import qualified Data.Yaml as Yaml
import Test.Hspec

sampleDetails :: TemplateDetails
sampleDetails =
  TemplateDetails
    { tvdId = 42
    , tvdName = "sample-tpl"
    , tvdCpuCount = 2
    , tvdRam = 4294967296
    , tvdDescription = Just "A sample template"
    , tvdHeadless = False
    , tvdGraphicsAdapter = GraphicsQxlVga
    , tvdCloudInit = True
    , tvdGuestAgent = True
    , tvdStartupTimeoutSec = 120
    , tvdTpm = True
    , tvdVsock = True
    , tvdBalloon = True
    , tvdRng = True
    , tvdAutostart = False
    , tvdRebootQuirk = False
    , tvdCloudInitConfig = Nothing
    , tvdCreatedAt = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
    , tvdDrives =
        [ TemplateDriveInfo
            { tvdiDiskImage = Just NamedRef {nrId = 7, nrName = "base-disk"}
            , tvdiDiskSelector = Just (ImageTag "base-disk" "latest")
            , tvdiDiskName = Nothing
            , tvdiInterface = InterfaceVirtio
            , tvdiMedia = Just MediaDisk
            , tvdiReadOnly = False
            , tvdiCacheType = CacheWriteback
            , tvdiDiscard = True
            , tvdiCloneStrategy = StrategyOverlay
            , tvdiSize = Just 21474836480
            , tvdiFormat = Nothing
            , tvdiEphemeral = Just True
            }
        ]
    , tvdNetIfs =
        [ TemplateNetIfInfo
            { tvniType = NetUser
            , tvniModel = NetworkE1000
            , tvniHostDevice = Nothing
            , tvniNetwork = Nothing
            }
        ]
    , tvdSshKeys =
        [ TemplateSshKeyInfo
            { tvskiId = 3
            , tvskiName = "admin-key"
            }
        ]
    , tvdSharedDirs =
        [ TemplateSharedDirInfo
            { tvsdiId = 11
            , tvsdiPath = "/srv/data"
            , tvsdiTag = "data"
            , tvsdiCache = CacheAuto
            , tvsdiReadOnly = False
            }
        ]
    , tvdAudioDevices =
        [ TemplateAudioDeviceInfo
            { tvadiId = 12
            , tvadiBackend = AudioPipewire
            , tvadiModel = AudioIch9IntelHda
            , tvadiOptions = "out.name=speakers,in.name=mic"
            }
        ]
    }

build :: Build
build = case Yaml.decodeEither' (BS.pack "name: output\ntemplate: sample-tpl\ntarget: {}\nprovisioners:\n  - shell: echo first\n  - file: {content: YQ==, to: /tmp/file}\n") of
  Right b -> b {buildResolvedTemplate = Just sampleDetails}
  Left err -> error (show err)

spec :: Spec
spec = describe "build input identity" $ do
  let identity b = buildInputIdentity b ["ssh-ed25519 key"]
      fingerprint = biFingerprint . identity
      template t = build {buildResolvedTemplate = Just t}
  it "includes readiness script contents, interpreter and timeout in the fingerprint" $ do
    let probe body interpreter seconds = case (Yaml.decodeEither' body :: Either Yaml.ParseException Shell) of
          Right sh -> build {buildProvisioners = [ProvWaitFor (WaitForScript sh {shellInterpreter = interpreter} seconds)]}
          Left err -> error (show err)
    fingerprint (probe "inline: exit 0" ShellPowerShell 300) `shouldNotBe` fingerprint (probe "inline: exit 1" ShellPowerShell 300)
    fingerprint (probe "inline: exit 0" ShellPowerShell 300) `shouldNotBe` fingerprint (probe "inline: exit 0" ShellSh 300)
    fingerprint (probe "inline: exit 0" ShellPowerShell 300) `shouldNotBe` fingerprint (probe "inline: exit 0" ShellPowerShell 900)
  it "pins offline-overlay identity to the resolved image version" $ do
    let offline = build {buildStrategy = BuildStrategyOfflineOverlay, buildSourceImage = Just (ImageTag "base" "latest"), buildResolvedSourceImage = Just 7}
    fingerprint offline `shouldBe` fingerprint (offline {buildSourceImage = Just (ImageId 7)})
    fingerprint offline `shouldNotBe` fingerprint (offline {buildResolvedSourceImage = Just 8})
    fingerprint offline `shouldNotBe` fingerprint build
    fingerprint (offline {buildTarget = (buildTarget offline) {btSize = Just 10737418240}}) `shouldNotBe` fingerprint offline
  it "retains legacy default target size identity" $ do
    fingerprint build `shouldBe` fingerprint (build {buildTarget = (buildTarget build) {btSize = Just 10737418240}})
  it "stores valid deterministic JSON and a SHA-256 fingerprint" $ do
    identity build `shouldBe` identity build
    biFingerprint (identity build) `shouldBe` "c14bccf6d1da66150cfc8a865c0a4a012055a6c88ba63a20ae32713aa23f7e77"
    (Aeson.eitherDecodeStrict' (TE.encodeUtf8 (biInputs (identity build))) :: Either String Aeson.Value) `shouldSatisfy` either (const False) (const True)
  it "includes inherited and overridden startup timeout budgets" $ do
    fingerprint build `shouldNotBe` fingerprint (template sampleDetails {tvdStartupTimeoutSec = 1200})
    fingerprint build `shouldNotBe` fingerprint (build {buildVm = (buildVm build) {bvmStartupTimeoutSec = Just 1200}})
  it "ignores allocated template, shared directory, audio and SSH key IDs and labels" $ do
    let recreated =
          sampleDetails
            { tvdId = 99
            , tvdName = "renamed"
            , tvdDescription = Just "changed label"
            , tvdCreatedAt = UTCTime (fromGregorian 2027 1 1) (secondsToDiffTime 0)
            , tvdSharedDirs = map (\d -> d {tvsdiId = 999}) (tvdSharedDirs sampleDetails)
            , tvdAudioDevices = map (\d -> d {tvadiId = 999}) (tvdAudioDevices sampleDetails)
            , tvdSshKeys = map (\k -> k {tvskiId = 999, tvskiName = "renamed"}) (tvdSshKeys sampleDetails)
            }
    fingerprint build `shouldBe` fingerprint (template recreated)
  it "compares actual source versions rather than tag or display labels" $ do
    let drive = case tvdDrives sampleDetails of
          d : _ -> d
          [] -> error "fixture has no drive"
        withDrive d = template sampleDetails {tvdDrives = [d]}
    fingerprint build `shouldBe` fingerprint (withDrive drive {tvdiDiskImage = Just (NamedRef 7 "renamed"), tvdiDiskSelector = Just (ImageId 7)})
    fingerprint build `shouldNotBe` fingerprint (withDrive drive {tvdiDiskImage = Just (NamedRef 8 "base-disk")})
  it "includes every source-backed drive, including read-only firmware/media" $ do
    let drives = tvdDrives sampleDetails
        media version = TemplateDriveInfo (Just (NamedRef version "media")) Nothing Nothing InterfaceIde (Just MediaCdrom) True CacheNone False StrategyDirect Nothing Nothing Nothing
        withMedia version = template sampleDetails {tvdDrives = drives ++ [media version]}
    fingerprint (withMedia 20) `shouldNotBe` fingerprint (withMedia 21)
    fingerprint (withMedia 20) `shouldNotBe` fingerprint (template sampleDetails {tvdDrives = reverse (drives ++ [media 20])})
  it "includes VM, target, template and provisioner configuration and file bytes" $ do
    let target = buildTarget build
        changes =
          [ build {buildVm = BuildVm 9 1234 Nothing}
          , build {buildTarget = target {btFormat = FormatRaw}}
          , build {buildTarget = target {btSize = Just 10737418241}}
          , build {buildTarget = target {btCompact = not (btCompact target)}}
          , build {buildStrategy = BuildStrategyFromScratch}
          , build {buildBootKeys = [BootKey "ret" 1 1 1]}
          , build {buildShellDefaults = ShellDefaults (Just "set -e") [("A", "value")]}
          , build {buildProvisioners = reverse (buildProvisioners build)}
          , build {buildProvisioners = [ProvShell (Shell (Just "echo changed") Nothing Nothing [] ShellSh Nothing)]}
          , build {buildProvisioners = [ProvShell (Shell (Just "echo changed") Nothing Nothing [] ShellPowerShell Nothing)]}
          , build {buildProvisioners = [ProvFile (FileProv Nothing (Just "Yg==") "/tmp/file" Nothing)]}
          , template sampleDetails {tvdRng = not (tvdRng sampleDetails)}
          , template sampleDetails {tvdNetIfs = []}
          ]
    mapM_ (\b -> fingerprint b `shouldNotBe` fingerprint build) changes
    biFingerprint (buildInputIdentity build ["new public key"]) `shouldNotBe` fingerprint build
  it "ignores output and operational policy" $ do
    let changed =
          build
            { buildName = "other"
            , buildDescription = Just "description"
            , buildNode = "other-node"
            , buildCleanup = CleanupNever
            , buildWaitForShutdownSec = 99
            , buildTarget = (buildTarget build) {btPath = Just "/another/path", btIfExists = BuildIfExistsUpdate, btEphemeral = True}
            }
    fingerprint changed `shouldBe` fingerprint build
  it "normalizes env order and filters runtime variables" $ do
    let base = build {buildShellDefaults = ShellDefaults Nothing [("A", "a"), ("B", "b")]}
        reordered = base {buildShellDefaults = ShellDefaults Nothing [("B", "b"), ("A", "a")]}
    fingerprint base `shouldBe` fingerprint reordered
    let injected = base {buildShellDefaults = ShellDefaults Nothing [("A", "a"), ("B", "b"), ("CORVUS_BUILD_TASK_ID", "123"), ("CORVUS_BAKEVM_ID", "456")]}
    fingerprint injected `shouldBe` fingerprint base
    fingerprint (base {buildShellDefaults = ShellDefaults Nothing [("A", "changed"), ("B", "b")]}) `shouldNotBe` fingerprint base
