{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | VM-template CRUD + instantiation guards.
--
-- The handler under test is `Corvus.Handlers.Template`. The
-- create / update path goes through `Corvus.Schema.Template`'s
-- YAML parser; we feed it minimal-but-valid YAML for the happy
-- paths and a malformed string for the parse-error path.
-- Instantiation needs at least one backing disk image on the
-- target node (`DiskImageNode` row) — when missing we expect a
-- `RespError`, not a panic.
module Corvus.TemplateSpec (spec) where

import qualified Corvus.Model as M
import Corvus.Protocol (CloudInitInfo (..), TemplateAudioDeviceInfo (..), TemplateDetails (..), TemplateDriveInfo (..), TemplateNetIfInfo (..), TemplateSharedDirInfo (..), TemplateSshKeyInfo (..))
import qualified Data.Text as T
import Database.Persist (Filter, count)
import Test.DSL.Core (runDb)
import Test.Prelude

-- A minimal template YAML accepted by `Corvus.Schema.Template`'s
-- `TemplateYaml` parser. `drives:` is required (no default), so
-- we include an empty list explicitly.
minimalYaml :: Text -> Text
minimalYaml name =
  "name: "
    <> name
    <> "\n\
       \cpuCount: 2\n\
       \ram: 1024M\n\
       \drives: []\n"

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "whenTemplateList" $ do
    testCase "returns an empty list when no templates exist" $ do
      _ <- when_ whenTemplateList
      then_ $ responseIs $ \case
        RespTemplateList [] -> True
        _ -> False

  describe "whenTemplateCreate" $ do
    testCase "writes a template row from valid YAML" $ do
      _ <- when_ $ whenTemplateCreate (minimalYaml "t1")
      then_ $ responseIs $ \case
        RespTemplateCreated _ -> True
        _ -> False

    testCase "rejects malformed YAML" $ do
      _ <- when_ $ whenTemplateCreate "not: a: valid template"
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

    testCase "rejects a duplicate template name" $ do
      _ <- when_ $ whenTemplateCreate (minimalYaml "dup")
      _ <- when_ $ whenTemplateCreate (minimalYaml "dup")
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

  describe "whenTemplateShow" $ do
    testCase "returns TemplateNotFound for unknown id" $ do
      _ <- when_ $ whenTemplateShow 999
      then_ $ responseIs $ \case
        RespTemplateNotFound -> True
        _ -> False

    testCase "returns details for an existing template" $ do
      _ <- when_ $ whenTemplateCreate (minimalYaml "showable")
      _ <- when_ $ whenTemplateShow 1
      then_ $ responseIs $ \case
        RespTemplateInfo _ -> True
        _ -> False

  describe "whenTemplateUpdate" $ do
    testCase "returns a clean error for unknown id" $ do
      -- handleTemplateUpdate surfaces missing-template as
      -- `RespError "Template not found"` rather than the
      -- dedicated RespTemplateNotFound constructor — TemplateShow
      -- uses the constructor; update uses the error string.
      _ <- when_ $ whenTemplateUpdate 999 (minimalYaml "ghost")
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

    testCase "rewrites the row when both id and YAML are valid" $ do
      _ <- when_ $ whenTemplateCreate (minimalYaml "orig")
      _ <- when_ $ whenTemplateUpdate 1 (minimalYaml "renamed")
      then_ $ responseIs $ \case
        RespTemplateUpdated _ -> True
        _ -> False

  describe "whenTemplateDelete" $ do
    testCase "deletes an existing template" $ do
      _ <- when_ $ whenTemplateCreate (minimalYaml "doomed")
      _ <- when_ $ whenTemplateDelete 1
      then_ $ responseIs (== RespTemplateDeleted)

  describe "template sharedDirs round-trip" $ do
    testCase "create + show preserves sharedDirs fields" $ do
      let yaml =
            "name: tpl-sd\n\
            \cpuCount: 1\n\
            \ram: 512M\n\
            \drives: []\n\
            \sharedDirs:\n\
            \  - path: /srv/data\n\
            \    tag: data\n\
            \  - path: /etc/ssl\n\
            \    tag: certs\n\
            \    cache: never\n\
            \    readOnly: true\n"
      _ <- when_ $ whenTemplateCreate yaml
      _ <- when_ $ whenTemplateShow 1
      then_ $ responseIs $ \case
        RespTemplateInfo details ->
          let sds = tvdSharedDirs details
              byTag t = filter (\sd -> tvsdiTag sd == t) sds
           in case (byTag "data", byTag "certs") of
                ([d], [c]) ->
                  length sds == 2
                    && tvsdiPath d == "/srv/data"
                    && tvsdiCache d == CacheAuto
                    && not (tvsdiReadOnly d)
                    && tvsdiPath c == "/etc/ssl"
                    && tvsdiCache c == CacheNever
                    && tvsdiReadOnly c
                _ -> False
        _ -> False

    testCase "rejects a template with a duplicate sharedDirs tag" $ do
      let yaml =
            "name: tpl-dup-tag\n\
            \cpuCount: 1\n\
            \ram: 512M\n\
            \drives: []\n\
            \sharedDirs:\n\
            \  - path: /a\n\
            \    tag: same\n\
            \  - path: /b\n\
            \    tag: same\n"
      _ <- when_ $ whenTemplateCreate yaml
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

  describe "populated template lifecycle" $ do
    testCase "preserves child records and replaces and removes them cleanly" $ do
      _ <- insertDiskImage "base" FormatQcow2
      _ <- insertSshKey "key" "ssh-ed25519 AAA"
      _ <- whenTemplateCreate populatedYaml
      responseIs $ \case
        RespTemplateCreated _ -> True
        _ -> False
      _ <- whenTemplateShow 1
      responseIs $ \case
        RespTemplateInfo d ->
          (tvdName d, tvdCpuCount d, tvdRam d, tvdDescription d) == ("populated", 4, 2147483648, Just "description")
            && (tvdHeadless d, tvdCloudInit d, tvdGuestAgent d, tvdTpm d, tvdAutostart d, tvdRebootQuirk d, tvdVsock d, tvdBalloon d, tvdRng d) == (False, True, True, True, True, True, False, False, False)
            && tvdCloudInitConfig d == Just (CloudInitInfo (Just "#cloud-config") (Just "version: 2") False)
            && map (\x -> (tvdiDiskName x, tvdiSize x, tvdiFormat x, tvdiEphemeral x)) (tvdDrives d) == [(Nothing, Nothing, Nothing, Just False), (Just "root", Just 1073741824, Just FormatQcow2, Just True)]
            && map (\x -> (tvniHostDevice x, tvniNetwork x)) (tvdNetIfs d) == [(Just "br0", Nothing), (Nothing, Just "net")]
            && map tvskiName (tvdSshKeys d) == ["key"]
            && map tvsdiTag (tvdSharedDirs d) == ["shared"]
            && map tvadiOptions (tvdAudioDevices d) == ["server=/tmp/pulse"]
        _ -> False
      _ <- whenTemplateList
      responseIs $ \case
        RespTemplateList [_] -> True
        _ -> False
      replacement <- whenTemplateUpdate 1 (minimalYaml "replacement")
      responseIs $ \case
        RespTemplateUpdated _ -> True
        _ -> False
      assertChildCounts 0
      let replacementId = case replacement of
            RespTemplateUpdated ident -> ident
            _ -> error "expected updated template"
      restored <- whenTemplateUpdate replacementId populatedYaml
      responseIs $ \case
        RespTemplateUpdated _ -> True
        _ -> False
      let restoredId = case restored of
            RespTemplateUpdated ident -> ident
            _ -> error "expected updated template"
      _ <- whenTemplateDelete restoredId
      responseIs (== RespTemplateDeleted)
      assertChildCounts 0
      _ <- whenTemplateShow 1
      responseIs (== RespTemplateNotFound)
    testCase "restores every child record when replacement validation fails" $ do
      _ <- insertDiskImage "base" FormatQcow2
      _ <- insertSshKey "key" "ssh-ed25519 AAA"
      _ <- whenTemplateCreate populatedYaml
      original <- whenTemplateShow 1
      _ <- whenTemplateUpdate 1 "name: changed\ncpuCount: 4\nram: 2G\ndrives: [{interface: virtio, strategy: direct, diskImage: missing}]"
      responseIs $ \case
        RespError message -> "Disk image not found" `T.isInfixOf` message
        _ -> False
      _ <- whenTemplateShow 1
      responseIs (== original)
    mapM_
      ( \(addition, fragment) -> testCase ("rejects " <> T.unpack fragment <> " without changing the template") $ do
          _ <- whenTemplateCreate (minimalYaml "original")
          _ <- whenTemplateUpdate 1 ("name: changed\ncpuCount: 4\nram: 2G\n" <> addition)
          responseIs $ \case
            RespError message -> fragment `T.isInfixOf` message
            _ -> False
          _ <- whenTemplateShow 1
          responseIs $ \case
            RespTemplateInfo d -> tvdName d == "original" && tvdCpuCount d == 2 && null (tvdDrives d) && null (tvdNetIfs d)
            _ -> False
          assertChildCounts 0
      )
      [ ("drives: [{interface: virtio, strategy: create}]", "format is required")
      , ("drives: [{interface: virtio, strategy: direct}]", "diskImage is required")
      , ("drives: []\nnetworkInterfaces: [{type: managed}]", "network is required")
      , ("drives: []\nnetworkInterfaces: [{type: bridge}]", "hostDevice is required")
      , ("drives: [{interface: virtio, strategy: direct, diskImage: missing}]", "Disk image not found")
      , ("cloudInit: true\ndrives: []\nsshKeys: [{name: missing}]", "SSH key not found")
      ]

assertChildCounts :: Int -> TestM ()
assertChildCounts expected = do
  counts <-
    sequence
      [ runDb $ count ([] :: [Filter M.TemplateDrive])
      , runDb $ count ([] :: [Filter M.TemplateNetworkInterface])
      , runDb $ count ([] :: [Filter M.TemplateSharedDir])
      , runDb $ count ([] :: [Filter M.TemplateSshKey])
      , runDb $ count ([] :: [Filter M.TemplateAudioDevice])
      , runDb $ count ([] :: [Filter M.TemplateCloudInit])
      ]
  liftIO $ counts `shouldBe` replicate 6 expected

populatedYaml :: Text
populatedYaml =
  T.unlines
    [ "name: populated"
    , "cpuCount: 4"
    , "ram: 2G"
    , "description: description"
    , "cloudInit: true"
    , "guestAgent: true"
    , "tpm: true"
    , "autostart: true"
    , "rebootQuirk: true"
    , "graphicsAdapter: qxl-vga"
    , "vsock: false"
    , "balloon: false"
    , "rng: false"
    , "cloudInitConfig: {userData: '#cloud-config', networkConfig: 'version: 2', injectSshKeys: false}"
    , "drives:"
    , "  - {diskImage: base, interface: virtio, strategy: direct, media: disk, readOnly: true, cacheType: none, discard: true, ephemeral: false}"
    , "  - {diskName: root, interface: virtio, strategy: create, size: 1G, format: qcow2, ephemeral: true}"
    , "networkInterfaces: [{type: bridge, hostDevice: br0, model: e1000}, {network: net}]"
    , "sshKeys: [{name: key}]"
    , "sharedDirs: [{path: /shared, tag: shared, cache: always, readOnly: true}]"
    , "audioDevices: [{backend: pulse, model: intel-hda, options: server=/tmp/pulse}]"
    ]
