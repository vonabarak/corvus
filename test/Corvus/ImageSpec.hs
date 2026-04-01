{-# LANGUAGE OverloadedStrings #-}

module Corvus.ImageSpec (spec) where

import Corvus.Model (DriveFormat (..))
import Corvus.Qemu.Image
import Test.Hspec

spec :: Spec
spec = do
  describe "parseImageInfo" $ do
    it "parses qcow2 image info" $ do
      let output =
            unlines
              [ "image: test.qcow2"
              , "file format: qcow2"
              , "virtual size: 10 GiB (10737418240 bytes)"
              , "disk size: 196 KiB"
              , "cluster_size: 65536"
              , "Format specific information:"
              , "    compat: 1.1"
              ]
      case parseImageInfo output of
        Right info -> do
          iiFormat info `shouldBe` FormatQcow2
          iiVirtualSizeMb info `shouldBe` 10240
        Left err -> fail $ "Parse failed: " ++ show err

    it "parses raw image info" $ do
      let output =
            unlines
              [ "image: test.raw"
              , "file format: raw"
              , "virtual size: 1 GiB (1073741824 bytes)"
              , "disk size: 0"
              ]
      case parseImageInfo output of
        Right info -> do
          iiFormat info `shouldBe` FormatRaw
          iiVirtualSizeMb info `shouldBe` 1024
        Left err -> fail $ "Parse failed: " ++ show err

    it "parses vpc image info" $ do
      let output =
            unlines
              [ "image: test.vpc"
              , "file format: vpc"
              , "virtual size: 2 GiB (2147483648 bytes)"
              , "disk size: 512 bytes"
              ]
      case parseImageInfo output of
        Right info -> do
          iiFormat info `shouldBe` FormatVpc
          iiVirtualSizeMb info `shouldBe` 2048
        Left err -> fail $ "Parse failed: " ++ show err

    it "parses image with snapshots" $ do
      let output =
            unlines
              [ "image: test.qcow2"
              , "file format: qcow2"
              , "virtual size: 10 GiB (10737418240 bytes)"
              , "disk size: 1 GiB (1073741824 bytes)"
              , "Snapshot list:"
              , "ID        TAG               VM SIZE                DATE       VM CLOCK"
              , "1         snap1                   0 2024-01-01 00:00:00   00:00:00.000"
              , "2         snap2                   0 2024-01-02 00:00:00   00:00:00.000"
              ]
      case parseImageInfo output of
        Right info -> do
          length (iiSnapshots info) `shouldBe` 2
          map sdName (iiSnapshots info) `shouldBe` ["snap1", "snap2"]
        Left err -> fail $ "Parse failed: " ++ show err

    it "returns error for malformed output" $ do
      parseImageInfo "garbage output" `shouldSatisfy` isLeft

    it "returns error for missing virtual size" $ do
      let output =
            unlines
              [ "image: test.qcow2"
              , "file format: qcow2"
              , "disk size: 196 KiB"
              ]
      parseImageInfo output `shouldSatisfy` isLeft

    it "returns error for missing format line" $ do
      let output =
            unlines
              [ "image: test.qcow2"
              , "virtual size: 10 GiB (10737418240 bytes)"
              ]
      parseImageInfo output `shouldSatisfy` isLeft

  describe "detectFormatFromUrl" $ do
    it "detects qcow2" $ do
      detectFormatFromUrl "https://example.com/image.qcow2" `shouldBe` Just FormatQcow2

    it "detects raw from .img" $ do
      detectFormatFromUrl "https://example.com/image.img" `shouldBe` Just FormatRaw

    it "detects vmdk" $ do
      detectFormatFromUrl "https://example.com/image.vmdk" `shouldBe` Just FormatVmdk

    it "detects vdi" $ do
      detectFormatFromUrl "https://example.com/image.vdi" `shouldBe` Just FormatVdi

    it "detects vpc" $ do
      detectFormatFromUrl "https://example.com/image.vpc" `shouldBe` Just FormatVpc

    it "detects vhd as vpc" $ do
      detectFormatFromUrl "https://example.com/image.vhd" `shouldBe` Just FormatVpc

    it "detects vhdx" $ do
      detectFormatFromUrl "https://example.com/image.vhdx" `shouldBe` Just FormatVhdx

    it "strips .xz suffix before detection" $ do
      detectFormatFromUrl "https://example.com/image.qcow2.xz" `shouldBe` Just FormatQcow2

    it "strips query string" $ do
      detectFormatFromUrl "https://example.com/image.qcow2?token=abc" `shouldBe` Just FormatQcow2

    it "returns Nothing for unknown extension" $ do
      detectFormatFromUrl "https://example.com/image.iso" `shouldBe` Nothing

  describe "isHttpUrl" $ do
    it "detects HTTP" $ do
      isHttpUrl "http://example.com/image.qcow2" `shouldBe` True

    it "detects HTTPS" $ do
      isHttpUrl "https://example.com/image.qcow2" `shouldBe` True

    it "rejects local path" $ do
      isHttpUrl "/local/path.qcow2" `shouldBe` False

    it "rejects relative path" $ do
      isHttpUrl "relative/path.qcow2" `shouldBe` False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
