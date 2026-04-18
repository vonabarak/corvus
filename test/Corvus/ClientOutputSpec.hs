{-# LANGUAGE OverloadedStrings #-}

module Corvus.ClientOutputSpec (spec) where

import Corvus.Client.Output
  ( Align (..)
  , BorderStyle (..)
  , Column (..)
  , TableOpts (..)
  , computeWidths
  , defaultTableOpts
  , fitToWidth
  , renderCell
  , renderTable
  , selectColumns
  )
import Corvus.Model
import Corvus.Protocol
import Data.Aeson (ToJSON (..), Value (..), encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec

-- Fixed timestamp for deterministic tests
testTime :: UTCTime
testTime = posixSecondsToUTCTime 1700000000

spec :: Spec
spec = sequential $ do
  describe "JSON serialization of Protocol types" $ do
    describe "StatusInfo" $ do
      it "serializes with correct field names" $ do
        let info = StatusInfo 3600 5 "1.0.0-abcdef12" 29 (Just 12345)
            val = toJSON info
        val
          `shouldBe` object
            [ "uptime" .= (3600 :: Int)
            , "connections" .= (5 :: Int)
            , "version" .= ("1.0.0-abcdef12" :: String)
            , "protocolVersion" .= (29 :: Int)
            , "namespacePid" .= Just (12345 :: Int)
            ]

    describe "VmInfo" $ do
      it "serializes with correct field names and enum values" $ do
        let vm = VmInfo 1 "my-vm" VmRunning 4 2048 False False False Nothing False
            val = toJSON vm
        val
          `shouldBe` object
            [ "id" .= (1 :: Int)
            , "name" .= ("my-vm" :: String)
            , "status" .= ("running" :: String)
            , "cpuCount" .= (4 :: Int)
            , "ramMb" .= (2048 :: Int)
            , "headless" .= False
            , "guestAgent" .= False
            , "cloudInit" .= False
            , "healthcheck" .= Null
            , "autostart" .= False
            ]

      it "serializes stopped status correctly" $ do
        let vm = VmInfo 2 "test" VmStopped 1 512 False False False Nothing False
            json = encode vm
        BL.unpack json `shouldSatisfy` isInfixOf "\"stopped\""

    describe "DiskImageInfo" $ do
      it "serializes with all fields" $ do
        let disk = DiskImageInfo 1 "boot" "/path/boot.qcow2" FormatQcow2 (Just 10240) testTime [(1, "vm1"), (2, "vm2")] Nothing Nothing
            val = toJSON disk
        case val of
          Object obj -> do
            KM.lookup "id" obj `shouldBe` Just (Number 1)
            KM.lookup "name" obj `shouldBe` Just (String "boot")
            KM.lookup "format" obj `shouldBe` Just (String "qcow2")
            KM.lookup "attachedTo" obj `shouldSatisfy` isJust
          _ -> fail "Expected JSON object"

      it "serializes overlay backing info" $ do
        let disk = DiskImageInfo 2 "overlay" "/path/overlay.qcow2" FormatQcow2 Nothing testTime [] (Just 1) (Just "base")
            val = toJSON disk
        case val of
          Object obj -> do
            KM.lookup "backingImageId" obj `shouldBe` Just (Number 1)
            KM.lookup "backingImageName" obj `shouldBe` Just (String "base")
          _ -> fail "Expected JSON object"

    describe "SnapshotInfo" $ do
      it "serializes correctly" $ do
        let snap = SnapshotInfo 5 "before-upgrade" testTime (Just 100)
            val = toJSON snap
        case val of
          Object obj -> do
            KM.lookup "id" obj `shouldBe` Just (Number 5)
            KM.lookup "name" obj `shouldBe` Just (String "before-upgrade")
            KM.lookup "sizeMb" obj `shouldBe` Just (Number 100)
          _ -> fail "Expected JSON object"

    describe "SharedDirInfo" $ do
      it "serializes with cache enum" $ do
        let dir = SharedDirInfo 1 "/host/share" "myshare" CacheAuto False Nothing
            val = toJSON dir
        case val of
          Object obj -> do
            KM.lookup "cache" obj `shouldBe` Just (String "auto")
            KM.lookup "readOnly" obj `shouldBe` Just (Bool False)
          _ -> fail "Expected JSON object"

    describe "SshKeyInfo" $ do
      it "serializes with attached VMs" $ do
        let key = SshKeyInfo 1 "mykey" "ssh-ed25519 AAAA..." testTime [(1, "vm1"), (3, "vm3")]
            val = toJSON key
        case val of
          Object obj -> do
            KM.lookup "name" obj `shouldBe` Just (String "mykey")
            KM.lookup "attachedVms" obj `shouldSatisfy` isJust
          _ -> fail "Expected JSON object"

    describe "DriveInfo" $ do
      it "serializes interface and cache enums" $ do
        let drive = DriveInfo 1 10 "disk" InterfaceVirtio "/path/disk.qcow2" FormatQcow2 (Just MediaDisk) False CacheWriteback True
            val = toJSON drive
        case val of
          Object obj -> do
            KM.lookup "interface" obj `shouldBe` Just (String "virtio")
            KM.lookup "cacheType" obj `shouldBe` Just (String "writeback")
            KM.lookup "media" obj `shouldBe` Just (String "disk")
            KM.lookup "discard" obj `shouldBe` Just (Bool True)
          _ -> fail "Expected JSON object"

    describe "TemplateVmInfo" $ do
      it "serializes with optional description" $ do
        let t = TemplateVmInfo 1 "my-template" 2 1024 (Just "A test template") False False False
            val = toJSON t
        case val of
          Object obj -> do
            KM.lookup "name" obj `shouldBe` Just (String "my-template")
            KM.lookup "description" obj `shouldBe` Just (String "A test template")
          _ -> fail "Expected JSON object"

      it "serializes null description" $ do
        let t = TemplateVmInfo 1 "minimal" 1 512 Nothing False False False
            val = toJSON t
        case val of
          Object obj -> do
            KM.lookup "description" obj `shouldBe` Just Null
          _ -> fail "Expected JSON object"

    describe "List serialization" $ do
      it "empty list serializes to []" $ do
        encode ([] :: [VmInfo]) `shouldBe` "[]"

      it "VM list serializes as JSON array" $ do
        let vms = [VmInfo 1 "a" VmRunning 1 512 False False False Nothing False, VmInfo 2 "b" VmStopped 2 1024 False False False Nothing False]
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
        let opts = defaultTableOpts {toColumns = ["NAME", "ID"], toBorders = BordersNone}
        case renderTable opts Nothing sampleCols [("1", "a")] of
          Right txt -> do
            -- Header line: NAME should appear before ID.
            let firstLine = takeWhile (/= '\n') (T.unpack txt)
            let nameIx = findIndex "NAME" firstLine
            let idIx = findIndex "ID" firstLine
            (nameIx < idIx) `shouldBe` True
          Left err -> expectationFailure err

      it "unknown column name returns Left" $ do
        let opts = defaultTableOpts {toColumns = ["bogus"]}
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
