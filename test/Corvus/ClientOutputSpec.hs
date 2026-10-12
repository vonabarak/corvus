{-# LANGUAGE OverloadedStrings #-}

module Corvus.ClientOutputSpec (spec) where

import Corvus.Client.Human (humanJSON)
import Corvus.Client.Output
  ( Align (..)
  , BorderStyle (..)
  , Column (..)
  , TableOpts (..)
  , computeWidths
  , defaultTableOpts
  , fitToWidth
  , formatBool
  , renderCell
  , renderTable
  , resolveColumns
  , selectColumns
  )
import Corvus.Client.Output.Columns (diskColumns, sharedDirColumns, snapshotColumns, sshKeyVmColumns, vmColumns)
import Corvus.Client.Output.Selection (columnCompleter, columnCompletionRequest, completeColumns, parseColumnSelection)
import Corvus.Client.Types (ColumnSelection (..))
import Corvus.Model
import Corvus.Protocol
import Data.Aeson (ToJSON (..), Value (..), encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Options.Applicative.Types (Completer (..))
import System.Environment (withArgs)
import Test.Hspec

-- Fixed timestamp for deterministic tests
testTime :: UTCTime
testTime = posixSecondsToUTCTime 1700000000

spec :: Spec
spec = sequential $ do
  describe "CLI column selection and completion" $ do
    it "normalizes case and whitespace while keeping duplicates" $
      parseColumnSelection " ID, name ,ID " `shouldBe` Right (NamedColumns ("id" :| ["name", "id"]))
    it "recognizes all case insensitively" $ parseColumnSelection " ALL " `shouldBe` Right AllColumns
    mapM_ (\raw -> it ("rejects malformed selection " ++ show raw) $ parseColumnSelection raw `shouldSatisfy` either (const True) (const False)) ["", " ", ",ID", "ID,", "ID,,NAME", "all,ID", "ID,ALL"]
    it "completes a partial comma segment with canonical names" $
      completeColumns ["vm", "list", "-O"] "ID,na" `shouldBe` ["ID,NAME"]
    it "omits already selected names" $ completeColumns ["vm", "list", "-O"] "ID,ID" `shouldBe` []
    it "offers all only by itself" $ do
      completeColumns ["vm", "list", "-O"] "al" `shouldBe` ["all"]
      completeColumns ["vm", "list", "-O"] "NAME,al" `shouldBe` []
      completeColumns ["vm", "list", "-O"] "ALL," `shouldBe` []
    it "uses each nested list catalog" $ do
      completeColumns ["disk", "snapshot", "list", "disk", "-O"] "HAS" `shouldBe` ["HAS_VMSTATE"]
      completeColumns ["vm", "snapshot", "list", "disk", "-O"] "VM" `shouldBe` ["VM"]
      completeColumns ["ssh-key", "list-vm", "disk", "-O"] "ATT" `shouldBe` ["ATTACHED_VMS"]
    it "offers a deduplicated union before a command is known" $ do
      let names = completeColumns [] ""
      length (filter (== "NAME") names) `shouldBe` 1
      names `shouldSatisfy` elem "BACKING_IMAGE"
      completeColumns ["disk", "snapshot"] "HAS" `shouldBe` ["HAS_VMSTATE"]
    it "does not complete structured output or non-list commands" $ do
      mapM_ (\ws -> completeColumns ws "" `shouldBe` []) [["vm", "show", "list"], ["ping"], ["vm", "snapshot", "show"], ["-o", "json", "vm", "list"], ["vm", "list", "--output=yaml"], ["vm", "list", "-oJSON"]]
    it "skips global option values and entity positional arguments" $ do
      completeColumns ["--host", "disk", "vm", "list", "-O"] "CLOUD" `shouldBe` ["CLOUD_INIT"]
      completeColumns ["--socket", "vm", "--port", "1234", "--borders", "none", "--tls-cert-dir", "list", "vm", "show", "disk", "list", "-O"] "" `shouldBe` []
    it "reads the shared shell completion protocol locally" $
      withArgs ["--bash-completion-index", "4", "--bash-completion-word", "crv", "--bash-completion-word", "vm", "--bash-completion-word", "list", "--bash-completion-word", "-O", "--bash-completion-word", "ID,NA"] $
        runCompleter columnCompleter "ID,NA" >>= (`shouldBe` ["ID,NAME"])

    it "handles attached values and Bash split equals tokens" $ do
      let request index ws = ["--bash-completion-index", show (index :: Int)] ++ concatMap (\w -> ["--bash-completion-word", w]) ws
      columnCompletionRequest (request 3 ["crv", "vm", "list", "--columns=ID,NA"]) `shouldBe` Just ["--columns=ID,NAME"]
      columnCompletionRequest (request 3 ["crv", "vm", "list", "-OID,NA"]) `shouldBe` Just ["-OID,NAME"]
      columnCompletionRequest (request 5 ["crv", "vm", "list", "--columns", "=", "ID,NA"]) `shouldBe` Just ["ID,NAME"]
      columnCompletionRequest (request 4 ["crv", "vm", "list", "--columns", "=ID,NA"]) `shouldBe` Just ["=ID,NAME"]
      columnCompletionRequest (request 1 ["crv", "--columns=ID,NA", "vm", "list"]) `shouldBe` Just ["--columns=ID,NAME"]
      columnCompletionRequest (request 1 ["crv", "--columns=ID,NA", "vm", "list", "-o", "json"]) `shouldBe` Just []
      columnCompletionRequest (request 4 ["crv", "vm", "list", "-O", "ID,NA"]) `shouldBe` Nothing
      columnCompletionRequest (request 8 ["crv"]) `shouldBe` Nothing
      columnCompletionRequest (request (-1) ["crv"]) `shouldBe` Nothing
      columnCompletionRequest [] `shouldBe` Nothing
  describe "CLI boolean output" $ do
    mapM_
      ( \flag -> it ("uses yes/no for boolean columns: " ++ show flag) $ do
          let expected = if flag then "yes" else "no"
              vm = VmInfo 1 "vm" (NamedRef 2 "node") VmRunning 2 1024 flag flag flag flag Nothing flag flag "host" GraphicsVirtioVga flag flag flag 90
              disk = DiskImageInfo 1 [] "disk" [] FormatRaw Nothing testTime [] Nothing flag
              snapshot = SnapshotInfo 1 "snapshot" testTime Nothing flag flag flag
              dir = SharedDirInfo 1 "/share" "tag" CacheAuto flag Nothing
              values names cols row = map (`colRender` row) (filter ((`elem` names) . colName) cols)
          formatBool flag `shouldBe` expected
          values ["TPM", "CLOUD_INIT", "AUTOSTART", "HEADLESS", "GUEST_AGENT", "REBOOT_QUIRK", "VSOCK", "BALLOON", "RNG"] (vmColumns testTime) vm `shouldBe` replicate 9 expected
          values ["EPHEMERAL"] diskColumns disk `shouldBe` [expected]
          values ["LIVE", "QUIESCED", "HAS_VMSTATE"] snapshotColumns snapshot `shouldBe` replicate 3 expected
          values ["READ_ONLY"] sharedDirColumns dir `shouldBe` [expected]
      )
      [False, True]
  describe "CLI additional column values" $ do
    it "renders disk references, node placements, and missing fields" $ do
      let ref = NamedRef 12 "base"
          disk = DiskImageInfo 1 [] "disk" [DiskImagePlacement (NamedRef 3 "node") "/images/disk"] FormatQcow2 (Just 1024) testTime [] (Just ref) False
          cells chosen row = case resolveColumns chosen diskColumns of
            Left err -> error err
            Right cols -> map (`colRender` row) cols
          selection = NamedColumns ("BACKING_IMAGE" :| ["PLACEMENTS", "CREATED"])
      cells selection disk `shouldBe` ["base (12)", "node (3): /images/disk", "2023-11-14 22:13:20"]
      cells selection (disk {diiBackingImage = Nothing, diiPlacements = []}) `shouldBe` ["-", "-", "2023-11-14 22:13:20"]
    it "keeps default VM columns while all includes extra values" $ do
      let vm = VmInfo 1 "vm" (NamedRef 2 "node") VmRunning 2 1024 True True False True (Just testTime) True False "host" GraphicsVirtioVga True False True 90
          cols = vmColumns testTime
      fmap (map colName) (resolveColumns DefaultColumns cols) `shouldBe` Right ["ID", "NAME", "NODE", "STATUS", "CPUS", "RAM", "HEALTH", "GUEST_AGENT", "CLOUD_INIT", "HEADLESS"]
      case resolveColumns (NamedColumns ("HEADLESS" :| ["GUEST_AGENT", "BALLOON", "STARTUP_TIMEOUT_SEC", "HEALTHCHECK"])) cols of
        Left err -> expectationFailure err
        Right selected -> map (`colRender` vm) selected `shouldBe` ["yes", "yes", "no", "90", "2023-11-14 22:13:20"]
    it "makes VM key attachments optional rather than unavailable" $ do
      fmap (map colName) (resolveColumns DefaultColumns sshKeyVmColumns) `shouldBe` Right ["ID", "NAME", "PUBLIC_KEY"]
      fmap (map colName) (resolveColumns AllColumns sshKeyVmColumns) `shouldBe` Right ["ID", "NAME", "PUBLIC_KEY", "ATTACHED_VMS", "CREATED"]
  describe "human and machine size output" $ do
    it "keeps numeric bytes in JSON and exact suffixes in human YAML values" $ do
      let disk = DiskImageInfo 1 [] "tiny" [] FormatRaw (Just 512) testTime [] Nothing False
      case (toJSON disk, humanJSON disk) of
        (Object machine, Object human) -> do
          KM.lookup "size" machine `shouldBe` Just (Number 512)
          KM.lookup "size" human `shouldBe` Just (String "512B")
        _ -> expectationFailure "Expected disk objects"
    it "preserves unknown sizes and integers larger than JavaScript's safe range" $ do
      let disk = DiskImageInfo 1 [] "huge" [] FormatRaw (Just 9007199254740993) testTime [] Nothing False
      BL.unpack (encode disk) `shouldSatisfy` isInfixOf "9007199254740993"
      case humanJSON disk of
        Object human -> KM.lookup "size" human `shouldBe` Just (String "9007199254740993B")
        _ -> expectationFailure "Expected disk object"
      humanJSON (disk {diiSize = Nothing}) `shouldBe` toJSON (disk {diiSize = Nothing})
  describe "JSON serialization of Protocol types" $ do
    describe "StatusInfo" $ do
      it "serializes with correct field names" $ do
        let info =
              StatusInfo
                { siUptime = 3600
                , siConnections = 5
                , siVersion = "1.0.0-abcdef12"
                , siProtocolVersion = 29
                , siDatabaseBackend = "sqlite"
                , siDatabaseVersion = "3.50.2"
                }
            val = toJSON info
        val
          `shouldBe` object
            [ "uptime" .= (3600 :: Int)
            , "connections" .= (5 :: Int)
            , "version" .= ("1.0.0-abcdef12" :: String)
            , "protocol_version" .= (29 :: Int)
            , "database_backend" .= ("sqlite" :: String)
            , "database_version" .= ("3.50.2" :: String)
            ]

    describe "VmInfo" $ do
      it "serializes with correct field names and enum values" $ do
        let nodeRef = NamedRef {nrId = 7, nrName = "alpha"}
            vm = VmInfo 1 "my-vm" nodeRef VmRunning 4 2048 False False False False Nothing False False "host" GraphicsQxlVga True True True 120
            val = toJSON vm
        -- 'omitNothingFields = True' in 'innerOptions' drops the
        -- Nothing-valued 'healthcheck' entirely rather than serialising
        -- it as null. Python-side @Optional[...] = None@ covers both
        -- shapes via dict.get().
        val
          `shouldBe` object
            [ "id" .= (1 :: Int)
            , "name" .= ("my-vm" :: String)
            , "node" .= object ["id" .= (7 :: Int), "name" .= ("alpha" :: String)]
            , "status" .= ("running" :: String)
            , "cpu_count" .= (4 :: Int)
            , "ram" .= (2048 :: Int)
            , "headless" .= False
            , "guest_agent" .= False
            , "startup_timeout_sec" .= (120 :: Int)
            , "tpm" .= False
            , "cloud_init" .= False
            , "autostart" .= False
            , "reboot_quirk" .= False
            , "cpu_model" .= ("host" :: String)
            , "graphics_adapter" .= ("qxl-vga" :: String)
            , "vsock" .= True
            , "balloon" .= True
            , "rng" .= True
            ]

      it "serializes stopped status correctly" $ do
        let vm =
              VmInfo
                2
                "test"
                NamedRef {nrId = 1, nrName = "alpha"}
                VmStopped
                1
                512
                False
                False
                False
                False
                Nothing
                False
                False
                "host"
                GraphicsVirtioVga
                True
                True
                True
                120
            json = encode vm
        BL.unpack json `shouldSatisfy` isInfixOf "\"stopped\""

    describe "VmStats" $ do
      it "serializes the zero-sample sentinel cleanly" $ do
        -- The zero placeholder is what 'crv vm show --output json'
        -- emits when the daemon hasn't received an agent sample
        -- yet — it must round-trip through aeson without surprises.
        let val = toJSON zeroVmStats
        case val of
          Object obj -> do
            KM.lookup "sampled_at_nanos" obj `shouldBe` Just (Number 0)
            KM.lookup "interval_millis" obj `shouldBe` Just (Number 0)
            KM.lookup "cpu_jiffies_total" obj `shouldBe` Just (Number 0)
          _ -> fail "Expected JSON object"

      it "serializes a populated sample with snake-case field names" $ do
        let s =
              VmStats
                { vstSampledAtNanos = 1700000000000000000
                , vstIntervalMillis = 10000
                , vstCpuJiffiesTotal = 142000
                , vstClkTck = 100
                , vstHostRssBytes = 4031733760
                , vstBalloonActualBytes = 3221225472
                , vstBalloonMaxBytes = 4294967296
                , vstDrives =
                    [ DriveIo
                        { dioName = "drive0"
                        , dioReadBytesTotal = 51768954880
                        , dioWriteBytesTotal = 12998545408
                        , dioReadOpsTotal = 1240315
                        , dioWriteOpsTotal = 332108
                        }
                    ]
                , vstNets =
                    [ NetIo
                        { nioTapName = "vmtap0"
                        , nioRxBytesTotal = 34025467904
                        , nioTxBytesTotal = 4509265920
                        }
                    ]
                }
            json = encode s
        BL.unpack json `shouldSatisfy` isInfixOf "\"clk_tck\":100"
        BL.unpack json `shouldSatisfy` isInfixOf "\"drive0\""
        BL.unpack json `shouldSatisfy` isInfixOf "\"vmtap0\""

    describe "DiskImageInfo" $ do
      it "serializes with all fields" $ do
        let node = NamedRef {nrId = 1, nrName = "test-node"}
            placement = DiskImagePlacement node "/path/boot.qcow2"
            attached =
              [ NamedRef {nrId = 1, nrName = "vm1"}
              , NamedRef {nrId = 2, nrName = "vm2"}
              ]
            disk = DiskImageInfo 1 [] "boot" [placement] FormatQcow2 (Just 10240) testTime attached Nothing False
            val = toJSON disk
        case val of
          Object obj -> do
            KM.lookup "id" obj `shouldBe` Just (Number 1)
            KM.lookup "name" obj `shouldBe` Just (String "boot")
            KM.lookup "format" obj `shouldBe` Just (String "qcow2")
            KM.lookup "attached_to" obj `shouldSatisfy` isJust
          _ -> fail "Expected JSON object"

      it "serializes overlay backing info" $ do
        let node = NamedRef {nrId = 1, nrName = "test-node"}
            placement = DiskImagePlacement node "/path/overlay.qcow2"
            backing = Just NamedRef {nrId = 1, nrName = "base"}
            disk = DiskImageInfo 2 [] "overlay" [placement] FormatQcow2 Nothing testTime [] backing False
            val = toJSON disk
        case val of
          Object obj ->
            -- backing_image is a nested object now, not flat id+name.
            KM.lookup "backing_image" obj
              `shouldBe` Just (object ["id" .= (1 :: Int), "name" .= ("base" :: String)])
          _ -> fail "Expected JSON object"

    describe "SnapshotInfo" $ do
      it "serializes correctly" $ do
        let snap = SnapshotInfo 5 "before-upgrade" testTime (Just 100) False False False
            val = toJSON snap
        case val of
          Object obj -> do
            KM.lookup "id" obj `shouldBe` Just (Number 5)
            KM.lookup "name" obj `shouldBe` Just (String "before-upgrade")
            KM.lookup "size" obj `shouldBe` Just (Number 100)
            KM.lookup "live" obj `shouldBe` Just (Bool False)
            KM.lookup "quiesced" obj `shouldBe` Just (Bool False)
            KM.lookup "has_vmstate" obj `shouldBe` Just (Bool False)
          _ -> fail "Expected JSON object"

    describe "SharedDirInfo" $ do
      it "serializes with cache enum" $ do
        let dir = SharedDirInfo 1 "/host/share" "myshare" CacheAuto False Nothing
            val = toJSON dir
        case val of
          Object obj -> do
            KM.lookup "cache" obj `shouldBe` Just (String "auto")
            KM.lookup "read_only" obj `shouldBe` Just (Bool False)
          _ -> fail "Expected JSON object"

    describe "SshKeyInfo" $ do
      it "serializes with attached VMs" $ do
        let attached =
              [ NamedRef {nrId = 1, nrName = "vm1"}
              , NamedRef {nrId = 3, nrName = "vm3"}
              ]
            key = SshKeyInfo 1 "mykey" "ssh-ed25519 AAAA..." testTime attached
            val = toJSON key
        case val of
          Object obj -> do
            KM.lookup "name" obj `shouldBe` Just (String "mykey")
            KM.lookup "attached_vms" obj `shouldSatisfy` isJust
          _ -> fail "Expected JSON object"

    describe "DriveInfo" $ do
      it "serializes interface and cache enums" $ do
        let diskRef = NamedRef {nrId = 10, nrName = "disk"}
            drive = DriveInfo 1 (Just diskRef) InterfaceVirtio "/path/disk.qcow2" FormatQcow2 (Just MediaDisk) False CacheWriteback True
            val = toJSON drive
        case val of
          Object obj -> do
            KM.lookup "interface" obj `shouldBe` Just (String "virtio")
            KM.lookup "cache_type" obj `shouldBe` Just (String "writeback")
            KM.lookup "media" obj `shouldBe` Just (String "disk")
            KM.lookup "discard" obj `shouldBe` Just (Bool True)
            -- disk_image is a nested object now.
            KM.lookup "disk_image" obj
              `shouldBe` Just (object ["id" .= (10 :: Int), "name" .= ("disk" :: String)])
          _ -> fail "Expected JSON object"

    describe "TemplateVmInfo" $ do
      it "serializes with optional description" $ do
        let t = TemplateVmInfo 1 "my-template" 2 1024 (Just "A test template") False False False False False GraphicsVirtioVga True True True 120
            val = toJSON t
        case val of
          Object obj -> do
            KM.lookup "name" obj `shouldBe` Just (String "my-template")
            KM.lookup "description" obj `shouldBe` Just (String "A test template")
          _ -> fail "Expected JSON object"

      it "omits null description under omitNothingFields" $ do
        let t = TemplateVmInfo 1 "minimal" 1 512 Nothing False False False False False GraphicsVirtioVga True True True 120
            val = toJSON t
        case val of
          Object obj ->
            KM.lookup "description" obj `shouldBe` Nothing
          _ -> fail "Expected JSON object"

    describe "List serialization" $ do
      it "empty list serializes to []" $ do
        encode ([] :: [VmInfo]) `shouldBe` "[]"

      it "VM list serializes as JSON array" $ do
        let alpha = NamedRef {nrId = 1, nrName = "alpha"}
            beta = NamedRef {nrId = 2, nrName = "beta"}
            vms =
              [ VmInfo 1 "a" alpha VmRunning 1 512 False False False False Nothing False False "host" GraphicsVirtioVga True True True 120
              , VmInfo 2 "b" beta VmStopped 2 1024 False False False False Nothing False False "host" GraphicsVirtioVga True True True 120
              ]
            val = toJSON vms
        case val of
          Array arr -> length arr `shouldBe` 2
          _ -> fail "Expected JSON array"

  describe "printTable internals" $ do
    -- Sample columns used across the renderCell/renderTable tests.
    let sampleCols :: [Column (String, String)]
        sampleCols =
          [ Column "ID" RightAlign fst
          , Column "NAME" LeftAlign snd
          ]

    describe "selectColumns" $ do
      it "empty selector returns all columns unchanged" $ do
        case selectColumns [] sampleCols of
          Right cs -> map colName cs `shouldBe` ["ID", "NAME"]
          Left err -> expectationFailure err

      it "selector filters and reorders" $ do
        case selectColumns ["NAME", "ID"] sampleCols of
          Right cs -> map colName cs `shouldBe` ["NAME", "ID"]
          Left err -> expectationFailure err

      it "selector matching is case-insensitive" $ do
        case selectColumns ["name", "id"] sampleCols of
          Right cs -> map colName cs `shouldBe` ["NAME", "ID"]
          Left err -> expectationFailure err

      it "unknown column returns Left with available columns listed" $ do
        case selectColumns ["bogus"] sampleCols of
          Left msg -> do
            msg `shouldContain` "unknown column"
            msg `shouldContain` "ID"
            msg `shouldContain` "NAME"
          Right _ -> expectationFailure "expected Left"

      it "duplicate selector names are permitted" $ do
        case selectColumns ["ID", "ID"] sampleCols of
          Right cs -> length cs `shouldBe` 2
          Left err -> expectationFailure err

    describe "computeWidths" $ do
      it "uses header length when rows are empty" $ do
        computeWidths sampleCols [] `shouldBe` [2, 4]

      it "expands to widest rendered value" $ do
        computeWidths sampleCols [("123456", "x"), ("9", "longname00")]
          `shouldBe` [6, 10]

      it "returns header length when all rows are shorter" $ do
        computeWidths sampleCols [("1", "a")] `shouldBe` [2, 4]

    describe "fitToWidth" $ do
      it "returns widths unchanged when they already fit" $ do
        fitToWidth 50 [10, 10, 10] `shouldBe` [10, 10, 10]

      it "shrinks the widest column first" $ do
        -- Start: [5, 20, 5] = 30 total. Budget 25 → shrink widest by 5.
        fitToWidth 25 [5, 20, 5] `shouldBe` [5, 15, 5]

      it "never shrinks below 3 chars" $ do
        -- Budget 10, three 20-wide columns. Total shrink needed = 50. Each
        -- column can shrink at most 17 (20 → 3), so we get 17+17+16 = 50
        -- shrinkage, landing at [3, 3, 4]. No column falls below 3.
        fitToWidth 10 [20, 20, 20] `shouldBe` [3, 3, 4]

      it "never shrinks any column below 3 even with impossible budgets" $ do
        minimum (fitToWidth 1 [20, 20, 20]) `shouldSatisfy` (>= 3)

      it "non-positive budget floors every column at 3" $ do
        fitToWidth 0 [10, 20] `shouldBe` [3, 3]

    describe "renderCell" $ do
      it "left-aligns and pads short values" $ do
        renderCell True LeftAlign 6 "hi" `shouldBe` "hi    "

      it "right-aligns and pads short values" $ do
        renderCell True RightAlign 6 "7" `shouldBe` "     7"

      it "truncates with ellipsis when value exceeds width and truncate is on" $ do
        renderCell True LeftAlign 5 "abcdefgh" `shouldBe` "abcd…"

      it "right-align truncation keeps the tail of the value" $ do
        renderCell True RightAlign 5 "abcdefgh" `shouldBe` "…efgh"

      it "prints full value when truncation is disabled" $ do
        renderCell False LeftAlign 5 "abcdefgh" `shouldBe` "abcdefgh"

      it "exact-length value is returned unchanged" $ do
        renderCell True LeftAlign 5 "hello" `shouldBe` "hello"

    describe "borderChars" $ do
      it "Unicode style uses box-drawing glyphs" $ do
        -- Verify via a full render rather than poking fields directly.
        let opts = defaultTableOpts {toBorders = BordersUnicode}
        case renderTable opts Nothing sampleCols [] of
          Right txt -> T.unpack txt `shouldSatisfy` ("│" `isInfixOf`)
          Left err -> expectationFailure err

      it "ASCII style uses | and - as separators" $ do
        let opts = defaultTableOpts {toBorders = BordersAscii}
        case renderTable opts Nothing sampleCols [] of
          Right txt -> do
            T.unpack txt `shouldSatisfy` ("|" `isInfixOf`)
            T.unpack txt `shouldSatisfy` ("+" `isInfixOf`)
          Left err -> expectationFailure err

      it "None style emits no border characters" $ do
        let opts = defaultTableOpts {toBorders = BordersNone}
        case renderTable opts Nothing sampleCols [] of
          Right txt -> do
            T.unpack txt `shouldSatisfy` (not . ("│" `isInfixOf`))
            T.unpack txt `shouldSatisfy` (not . ("|" `isInfixOf`))
          Left err -> expectationFailure err

    describe "renderTable" $ do
      let diskRows :: [(String, String)]
          diskRows =
            [ ("1", "alma")
            , ("2", "corvus-dev-gentoo-base-overlay")
            ]

      it "default options render Unicode borders; no truncation when width is unbounded" $ do
        case renderTable defaultTableOpts Nothing sampleCols diskRows of
          Right txt -> do
            -- Unicode vertical bar present.
            T.unpack txt `shouldSatisfy` ("│" `isInfixOf`)
            -- No artificial truncation — full name preserved.
            T.unpack txt `shouldSatisfy` (not . ("…" `isInfixOf`))
            T.unpack txt `shouldSatisfy` ("corvus-dev-gentoo-base-overlay" `isInfixOf`)
          Left err -> expectationFailure err

      it "truncates only when table would exceed the terminal width" $ do
        -- Natural total is ~30+ chars for NAME; budget is tight.
        case renderTable defaultTableOpts (Just 15) sampleCols diskRows of
          Right txt -> T.unpack txt `shouldSatisfy` ("…" `isInfixOf`)
          Left err -> expectationFailure err

      it "does not truncate when natural layout fits within the terminal" $ do
        case renderTable defaultTableOpts (Just 200) sampleCols diskRows of
          Right txt -> T.unpack txt `shouldSatisfy` (not . ("…" `isInfixOf`))
          Left err -> expectationFailure err

      it "--no-borders suppresses │ characters" $ do
        let opts = defaultTableOpts {toBorders = BordersNone}
        case renderTable opts Nothing sampleCols diskRows of
          Right txt -> T.unpack txt `shouldSatisfy` (not . ("│" `isInfixOf`))
          Left err -> expectationFailure err

      it "--no-truncate prints the full value (no ellipsis)" $ do
        let opts = defaultTableOpts {toTruncate = False}
        case renderTable opts Nothing sampleCols diskRows of
          Right txt -> do
            T.unpack txt `shouldSatisfy` (not . ("…" `isInfixOf`))
            T.unpack txt `shouldSatisfy` ("corvus-dev-gentoo-base-overlay" `isInfixOf`)
          Left err -> expectationFailure err

      it "--columns reorders output columns" $ do
        let opts = defaultTableOpts {toColumns = NamedColumns ("NAME" :| ["ID"]), toBorders = BordersNone}
        case renderTable opts Nothing sampleCols [("1", "a")] of
          Right txt -> do
            -- Header line: NAME should appear before ID.
            let firstLine = takeWhile (/= '\n') (T.unpack txt)
            let nameIx = findIndex "NAME" firstLine
            let idIx = findIndex "ID" firstLine
            (nameIx < idIx) `shouldBe` True
          Left err -> expectationFailure err

      it "unknown column name returns Left" $ do
        let opts = defaultTableOpts {toColumns = NamedColumns ("bogus" :| [])}
        case renderTable opts Nothing sampleCols [] of
          Left msg -> msg `shouldContain` "unknown column"
          Right _ -> expectationFailure "expected Left"

      it "narrow terminal width triggers fit-based shrinking" $ do
        -- With a tight width, the long name cell must get truncated.
        let opts = defaultTableOpts
        case renderTable opts (Just 20) sampleCols diskRows of
          Right txt -> T.unpack txt `shouldSatisfy` ("…" `isInfixOf`)
          Left err -> expectationFailure err
  where
    findIndex needle haystack =
      let go _ [] = maxBound :: Int
          go i s@(_ : rest)
            | needle `isPrefixOf` s = i
            | otherwise = go (i + 1) rest
       in go 0 haystack
    isPrefixOf p s = take (length p) s == p
