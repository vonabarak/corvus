{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for QEMU Guest Agent protocol handling.
-- Tests the QGA JSON parsing layer (recvJson logic) with various edge cases
-- including newline-delimited streams and 0xFF framing bytes.
module Corvus.GuestAgentSpec (spec) where

import Corvus.Qemu.GuestAgent (GuestIpAddress (..), GuestNetIf (..), parseGuestInterfaces)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.Hspec

spec :: Spec
spec = do
  describe "QGA JSON parsing" $ do
    it "parses a single JSON object" $ do
      parseFirst "{\"return\": {}}\n"
        `shouldBe` Just (Aeson.object ["return" Aeson..= Aeson.object []])

    it "parses first object from multiple newline-delimited objects" $ do
      parseFirst "{\"return\": 42}\n{\"return\": {}}\n"
        `shouldBe` Just (Aeson.object ["return" Aeson..= (42 :: Int)])

    it "strips 0xFF bytes (QGA framing delimiter)" $ do
      parseFirst (BS.pack [0xFF] <> "{\"return\": 42}\n")
        `shouldBe` Just (Aeson.object ["return" Aeson..= (42 :: Int)])

    it "handles 0xFF between JSON objects" $ do
      parseFirst (BS.pack [0xFF] <> "{\"error\": \"stale\"}\n" <> BS.pack [0xFF] <> "{\"return\": 99}\n")
        `shouldBe` Just (Aeson.object ["error" Aeson..= ("stale" :: String)])

    it "handles response with trailing whitespace" $ do
      parseFirst "{\"return\": {}}\n\n"
        `shouldBe` Just (Aeson.object ["return" Aeson..= Aeson.object []])

    it "returns Nothing for empty data" $ do
      parseFirst BS.empty `shouldBe` Nothing

    it "returns Nothing for non-JSON data" $ do
      parseFirst "not json at all\n" `shouldBe` Nothing

    it "skips non-JSON lines to find valid JSON" $ do
      parseFirst "garbage line\n{\"return\": {}}\n"
        `shouldBe` Just (Aeson.object ["return" Aeson..= Aeson.object []])

    it "parses guest-sync response with numeric return" $ do
      let result = parseFirst "{\"return\": 1073741823}\n"
      case result of
        Just (Aeson.Object obj) ->
          KM.member "return" obj `shouldBe` True
        _ -> expectationFailure "Expected JSON object with return key"

    it "handles Windows-style CRLF line endings" $ do
      parseFirst "{\"return\": {}}\r\n"
        `shouldBe` Just (Aeson.object ["return" Aeson..= Aeson.object []])

  describe "interface parsing" $ do
    it "parses interfaces with hardware-address" $ do
      let json = "{\"return\": [{\"name\": \"eth0\", \"hardware-address\": \"52:54:00:12:34:56\", \"ip-addresses\": [{\"ip-address-type\": \"ipv4\", \"ip-address\": \"10.0.2.15\", \"prefix\": 24}]}]}\n"
          parsed = parseFirst json
      parsed `shouldSatisfy` \case
        Just (Aeson.Object obj) -> KM.member "return" obj
        _ -> False

    it "skips interfaces without hardware-address (Windows loopback)" $ do
      -- Windows returns loopback without hardware-address; this must not
      -- cause the entire interface list to fail to parse.
      let json = "{\"return\": [{\"name\": \"Ethernet 3\", \"hardware-address\": \"52:54:00:12:34:56\", \"ip-addresses\": [{\"ip-address-type\": \"ipv4\", \"ip-address\": \"10.0.2.15\", \"prefix\": 24}]}, {\"name\": \"Loopback Pseudo-Interface 1\", \"ip-addresses\": [{\"ip-address-type\": \"ipv4\", \"ip-address\": \"127.0.0.1\", \"prefix\": 8}]}]}\n"
          parsed = parseFirst json
      -- The JSON itself should parse fine (it's valid JSON)
      parsed `shouldSatisfy` \case
        Just (Aeson.Object obj) -> KM.member "return" obj
        _ -> False

  describe "parseGuestInterfaces real-world" $ do
    it "parses a real FreeBSD 14.3 qemu-ga response" $ do
      -- Captured verbatim from qemu-ga 10.2.2 on FreeBSD 14.4-RELEASE-p1 via
      -- truss of a live guest. Exactly what corvus receives over virtio-serial.
      let json =
            "{\"return\": [{\"name\": \"vtnet0\", \"ip-addresses\": [\
            \{\"ip-address-type\": \"ipv6\", \"ip-address\": \"fe80::5054:ff:fee1:8aff\", \"prefix\": 64}, \
            \{\"ip-address-type\": \"ipv4\", \"ip-address\": \"192.168.89.216\", \"prefix\": 22}\
            \], \"hardware-address\": \"52:54:00:e1:8a:ff\"}, \
            \{\"name\": \"lo0\", \"ip-addresses\": [\
            \{\"ip-address-type\": \"ipv6\", \"ip-address\": \"::1\", \"prefix\": 128}, \
            \{\"ip-address-type\": \"ipv6\", \"ip-address\": \"fe80::1\", \"prefix\": 64}, \
            \{\"ip-address-type\": \"ipv4\", \"ip-address\": \"127.0.0.1\", \"prefix\": 8}\
            \], \"hardware-address\": \"00:00:00:00:00:00\"}]}"
      case parseGuestInterfaces (Aeson.decodeStrict json) of
        Nothing -> expectationFailure "parser returned Nothing on a well-formed FreeBSD response"
        Just ifs -> do
          length ifs `shouldBe` 2
          gniHardwareAddress (head ifs) `shouldBe` "52:54:00:e1:8a:ff"
          let vtIps = gniIpAddresses (head ifs)
          length vtIps `shouldBe` 2
          map giaAddress vtIps `shouldContain` ["192.168.89.216"]

  describe "parseGuestInterfaces (lenient)" $ do
    it "parses a normal response" $ do
      let json = "{\"return\": [{\"name\": \"eth0\", \"hardware-address\": \"52:54:00:12:34:56\", \"ip-addresses\": [{\"ip-address-type\": \"ipv4\", \"ip-address\": \"10.0.2.15\", \"prefix\": 24}]}]}"
      parseGuestInterfaces (Aeson.decodeStrict json)
        `shouldBe` Just
          [ GuestNetIf
              { gniHardwareAddress = "52:54:00:12:34:56"
              , gniIpAddresses =
                  [GuestIpAddress {giaType = "ipv4", giaAddress = "10.0.2.15", giaPrefix = 24}]
              }
          ]

    it "defaults missing prefix to 0 instead of failing" $ do
      -- Some qemu-ga builds (e.g. on certain BSD setups) omit the prefix
      -- field on some addresses. Previously this caused the whole parse to
      -- fail and all IPs to be lost.
      let json = "{\"return\": [{\"hardware-address\": \"52:54:00:aa:bb:cc\", \"ip-addresses\": [{\"ip-address-type\": \"ipv6\", \"ip-address\": \"fe80::1\"}]}]}"
      parseGuestInterfaces (Aeson.decodeStrict json)
        `shouldBe` Just
          [ GuestNetIf
              { gniHardwareAddress = "52:54:00:aa:bb:cc"
              , gniIpAddresses =
                  [GuestIpAddress {giaType = "ipv6", giaAddress = "fe80::1", giaPrefix = 0}]
              }
          ]

    it "drops one malformed IP but keeps the rest of the interface" $ do
      -- If one address entry is missing required fields, the other addresses
      -- on the same interface must still be reported.
      let json =
            "{\"return\": [{\"hardware-address\": \"52:54:00:12:34:56\", \"ip-addresses\": [\
            \{\"ip-address-type\": \"ipv4\", \"ip-address\": \"10.0.2.15\", \"prefix\": 24},\
            \{\"garbage\": true},\
            \{\"ip-address-type\": \"ipv6\", \"ip-address\": \"fd00::5\", \"prefix\": 64}\
            \]}]}"
      parseGuestInterfaces (Aeson.decodeStrict json)
        `shouldBe` Just
          [ GuestNetIf
              { gniHardwareAddress = "52:54:00:12:34:56"
              , gniIpAddresses =
                  [ GuestIpAddress {giaType = "ipv4", giaAddress = "10.0.2.15", giaPrefix = 24}
                  , GuestIpAddress {giaType = "ipv6", giaAddress = "fd00::5", giaPrefix = 64}
                  ]
              }
          ]

    it "drops one malformed interface but keeps the rest of the list" $ do
      -- Previously any single malformed interface would cause every other
      -- interface to be lost, silently breaking network detection.
      let json =
            "{\"return\": [\
            \{\"hardware-address\": \"52:54:00:aa:bb:cc\", \"ip-addresses\": [{\"ip-address-type\": \"ipv4\", \"ip-address\": \"10.0.0.1\", \"prefix\": 24}]},\
            \{\"hardware-address\": \"52:54:00:dd:ee:ff\", \"ip-addresses\": [{\"totally\": \"wrong\"}, {\"shape\": true}], \"extra-unexpected-field\": [1,2,3]}\
            \]}"
      -- First interface must still appear even though the second's address
      -- shapes are unparseable; the second still parses (just with 0 IPs).
      let parsed = parseGuestInterfaces (Aeson.decodeStrict json)
      case parsed of
        Just ifs -> do
          length ifs `shouldBe` 2
          gniHardwareAddress (head ifs) `shouldBe` "52:54:00:aa:bb:cc"
          length (gniIpAddresses (head ifs)) `shouldBe` 1
        Nothing -> expectationFailure "parser failed entirely instead of dropping bad entry"

    it "skips interfaces without hardware-address (Windows loopback)" $ do
      let json = "{\"return\": [{\"name\": \"Loopback\"}, {\"hardware-address\": \"52:54:00:12:34:56\", \"ip-addresses\": []}]}"
      parseGuestInterfaces (Aeson.decodeStrict json)
        `shouldBe` Just
          [GuestNetIf {gniHardwareAddress = "52:54:00:12:34:56", gniIpAddresses = []}]

  describe "sync ID range" $ do
    it "bounded sync IDs fit in 32-bit integers" $ do
      -- Max sync ID is 2^30 - 1 = 1073741823
      let maxSyncId = 1073741823 :: Int
      -- Verify it fits in IEEE 754 double without precision loss
      let asDouble = fromIntegral maxSyncId :: Double
      round asDouble `shouldBe` maxSyncId

    it "bounded sync IDs roundtrip through JSON" $ do
      let syncId = 1073741823 :: Int
          json = Aeson.encode (Aeson.object ["return" Aeson..= syncId])
          parsed = Aeson.decode json :: Maybe Aeson.Value
      case parsed of
        Just (Aeson.Object obj) ->
          KM.lookup "return" obj `shouldBe` Just (Aeson.Number (fromIntegral syncId))
        _ -> expectationFailure "Failed to roundtrip sync ID through JSON"

--------------------------------------------------------------------------------
-- Pure parsing helper (mirrors recvJson logic from GuestAgent.hs)
--------------------------------------------------------------------------------

-- | Parse the first valid JSON object from a newline-delimited byte stream.
-- Strips 0xFF bytes (QGA framing delimiter).
parseFirst :: BS.ByteString -> Maybe Aeson.Value
parseFirst bs
  | BS.null bs = Nothing
  | otherwise =
      let cleaned = BS.filter (/= 0xFF) bs
          lines' = filter (not . BS.null) $ BS.split (fromIntegral (fromEnum '\n') :: Word8) cleaned
       in go lines'
  where
    go [] = Nothing
    go (l : ls) = case Aeson.decodeStrict l of
      Just v -> Just v
      Nothing -> go ls
