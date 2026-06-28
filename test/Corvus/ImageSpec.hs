{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Unit tests for 'Corvus.Node.Image.parseImageInfo' and format
-- detection helpers.
--
-- Fixtures are written with @aesonQQ@ and encoded back to 'String' via
-- 'jsonStr', which is what 'parseImageInfo' expects (it shells out to
-- @qemu-img info --output=json@ in production). The round-trip keeps
-- the fixtures readable and structurally valid — a typo in a key name
-- is a Template Haskell parse error, not a silent runtime failure.
module Corvus.ImageSpec (spec) where

import Corvus.Model (DriveFormat (..))
import Corvus.Node.Image
import Data.Aeson (Value, encode)
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec

-- | Encode an 'aesonQQ'-produced 'Value' to the JSON 'String' that
-- 'parseImageInfo' accepts.
jsonStr :: Value -> String
jsonStr = BSL.unpack . encode

spec :: Spec
spec = do
  describe "parseImageInfo" $ do
    it "parses qcow2 image info" $ do
      let fixture =
            jsonStr
              [aesonQQ|
                { "virtual-size": 10737418240
                , "filename": "test.qcow2"
                , "cluster-size": 65536
                , "format": "qcow2"
                , "actual-size": 200704
                , "dirty-flag": false
                }
              |]
      case parseImageInfo fixture of
        Right info -> do
          iiFormat info `shouldBe` FormatQcow2
          iiVirtualSizeMb info `shouldBe` 10240
          iiActualSizeMb info `shouldSatisfy` maybe False (>= 0)
        Left err -> fail $ "Parse failed: " ++ show err

    it "parses raw image info" $ do
      let fixture =
            jsonStr
              [aesonQQ|
                { "virtual-size": 1073741824
                , "filename": "test.raw"
                , "format": "raw"
                , "actual-size": 0
                , "dirty-flag": false
                }
              |]
      case parseImageInfo fixture of
        Right info -> do
          iiFormat info `shouldBe` FormatRaw
          iiVirtualSizeMb info `shouldBe` 1024
        Left err -> fail $ "Parse failed: " ++ show err

    it "parses vpc image info" $ do
      let fixture =
            jsonStr
              [aesonQQ|
                { "virtual-size": 2147483648
                , "filename": "test.vpc"
                , "format": "vpc"
                , "actual-size": 512
                , "dirty-flag": false
                }
              |]
      case parseImageInfo fixture of
        Right info -> do
          iiFormat info `shouldBe` FormatVpc
          iiVirtualSizeMb info `shouldBe` 2048
        Left err -> fail $ "Parse failed: " ++ show err

    it "parses image with snapshots" $ do
      let fixture =
            jsonStr
              [aesonQQ|
                { "virtual-size": 10737418240
                , "filename": "test.qcow2"
                , "cluster-size": 65536
                , "format": "qcow2"
                , "actual-size": 1073741824
                , "dirty-flag": false
                , "snapshots":
                    [ { "id": "1"
                      , "name": "snap1"
                      , "vm-state-size": 0
                      , "date-sec": 1704067200
                      , "date-nsec": 0
                      , "vm-clock-sec": 0
                      , "vm-clock-nsec": 0
                      , "icount": 0
                      }
                    , { "id": "2"
                      , "name": "snap2"
                      , "vm-state-size": 0
                      , "date-sec": 1704153600
                      , "date-nsec": 0
                      , "vm-clock-sec": 0
                      , "vm-clock-nsec": 0
                      , "icount": 0
                      }
                    ]
                }
              |]
      case parseImageInfo fixture of
        Right info -> do
          length (iiSnapshots info) `shouldBe` 2
          map sdName (iiSnapshots info) `shouldBe` ["snap1", "snap2"]
          map sdId (iiSnapshots info) `shouldBe` ["1", "2"]
        Left err -> fail $ "Parse failed: " ++ show err

    it "parses image without snapshots array" $ do
      -- qemu-img omits the "snapshots" field entirely when there are none.
      let fixture =
            jsonStr
              [aesonQQ|
                { "virtual-size": 10485760
                , "filename": "plain.qcow2"
                , "format": "qcow2"
                , "actual-size": 196608
                , "dirty-flag": false
                }
              |]
      case parseImageInfo fixture of
        Right info -> iiSnapshots info `shouldBe` []
        Left err -> fail $ "Parse failed: " ++ show err

    it "parses image with missing actual-size" $ do
      -- Some backing drivers don't report actual-size; we tolerate it.
      let fixture =
            jsonStr
              [aesonQQ|
                { "virtual-size": 1048576
                , "filename": "t.raw"
                , "format": "raw"
                }
              |]
      case parseImageInfo fixture of
        Right info -> iiActualSizeMb info `shouldBe` Nothing
        Left err -> fail $ "Parse failed: " ++ show err

    it "returns error for non-JSON output" $
      -- Plain string here — this input is /intentionally/ not JSON.
      parseImageInfo "garbage output" `shouldSatisfy` isLeft

    it "returns error for missing virtual-size field" $ do
      let fixture = jsonStr [aesonQQ| { "filename": "t.qcow2", "format": "qcow2" } |]
      parseImageInfo fixture `shouldSatisfy` isLeft

    it "returns error for missing format field" $ do
      let fixture = jsonStr [aesonQQ| { "filename": "t.qcow2", "virtual-size": 10737418240 } |]
      parseImageInfo fixture `shouldSatisfy` isLeft

    it "returns error for unknown format value" $ do
      let fixture = jsonStr [aesonQQ| { "virtual-size": 1024, "format": "bogus-format" } |]
      parseImageInfo fixture `shouldSatisfy` isLeft

  describe "detectFormatFromUrl" $ do
    it "detects qcow2" $
      detectFormatFromUrl "https://example.com/image.qcow2" `shouldBe` Just FormatQcow2

    it "detects raw from .img" $
      detectFormatFromUrl "https://example.com/image.img" `shouldBe` Just FormatRaw

    it "detects vmdk" $
      detectFormatFromUrl "https://example.com/image.vmdk" `shouldBe` Just FormatVmdk

    it "detects vdi" $
      detectFormatFromUrl "https://example.com/image.vdi" `shouldBe` Just FormatVdi

    it "detects vpc" $
      detectFormatFromUrl "https://example.com/image.vpc" `shouldBe` Just FormatVpc

    it "detects vhd as vpc" $
      detectFormatFromUrl "https://example.com/image.vhd" `shouldBe` Just FormatVpc

    it "detects vhdx" $
      detectFormatFromUrl "https://example.com/image.vhdx" `shouldBe` Just FormatVhdx

    it "strips .xz suffix before detection" $
      detectFormatFromUrl "https://example.com/image.qcow2.xz" `shouldBe` Just FormatQcow2

    it "strips query string" $
      detectFormatFromUrl "https://example.com/image.qcow2?token=abc" `shouldBe` Just FormatQcow2

    it "returns Nothing for unknown extension" $
      detectFormatFromUrl "https://example.com/image.iso" `shouldBe` Nothing

  describe "isHttpUrl" $ do
    it "detects HTTP" $
      isHttpUrl "http://example.com/image.qcow2" `shouldBe` True

    it "detects HTTPS" $
      isHttpUrl "https://example.com/image.qcow2" `shouldBe` True

    it "rejects local path" $
      isHttpUrl "/local/path.qcow2" `shouldBe` False

    it "rejects relative path" $
      isHttpUrl "relative/path.qcow2" `shouldBe` False

  describe "hashFile" $ do
    it "computes supported digest algorithms" $
      withSystemTempFile "corvus-hash.txt" $ \path h -> do
        BSL.hPutStr h "abc"
        hClose h
        hashFile "md5" path `shouldReturn` Right "900150983cd24fb0d6963f7d28e17f72"
        hashFile "sha1" path `shouldReturn` Right "a9993e364706816aba3e25717850c26c9cd0d89d"
        hashFile "sha256" path `shouldReturn` Right "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        hashFile "sha512" path
          `shouldReturn` Right
            "ddaf35a193617abacc417349ae204131\
            \12e6fa4e89a97ea20a9eeee64b55d39a\
            \2192992a274fc1a836ba3c23a3feebbd\
            \454d4423643ce80e2a9ac94fa54ca49f"
        hashFile "blake2b" path
          `shouldReturn` Right
            "ba80a53f981c4d0d6a2797b69f12f6e9\
            \4c212f14685ac4b74b12bb6fdbffa2d1\
            \7d87c5392aab792dc252d5de4533cc95\
            \18d38aa8dbf1925ab92386edd4009923"

    it "rejects unsupported digest algorithms" $
      withSystemTempFile "corvus-hash.txt" $ \path h -> do
        BSL.hPutStr h "abc"
        hClose h
        hashFile "sha3" path `shouldReturn` Left "unsupported hash algorithm: sha3"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
