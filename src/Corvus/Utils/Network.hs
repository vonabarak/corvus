{-# LANGUAGE OverloadedStrings #-}

-- | Network utilities: IPv4 CIDR subnet operations and MAC address generation.
module Corvus.Utils.Network
  ( -- * Subnet operations
    validateSubnet
  , gatewayAddress
  , dhcpRangeStart
  , dhcpRangeEnd
  , prefixLength
  , subnetMask

    -- * MAC address generation
  , generateMacAddress
  )
where

import Control.Monad (replicateM)
import Data.Bits (complement, shiftL, shiftR, (.&.))
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import System.Random (randomRIO)
import Text.Printf (printf)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- MAC Address
--------------------------------------------------------------------------------

-- | Generate a random MAC address with the QEMU OUI prefix (52:54:00).
generateMacAddress :: IO Text
generateMacAddress = do
  [b1, b2, b3] <- replicateM 3 (randomRIO (0, 255 :: Int))
  pure $ T.pack $ printf "52:54:00:%02x:%02x:%02x" b1 b2 b3

--------------------------------------------------------------------------------
-- Subnet Operations
--------------------------------------------------------------------------------

-- | Validate and normalize a CIDR subnet string (e.g., "10.0.1.0/24").
-- Returns the normalized CIDR string or an error message.
validateSubnet :: Text -> Either Text Text
validateSubnet cidr = do
  (addr, prefix) <- parseCidr cidr
  let mask = prefixToMask prefix
      network = addr .&. mask
  pure $ ipToText network <> "/" <> T.pack (show prefix)

-- | Get the gateway address (first usable IP) from a CIDR subnet.
gatewayAddress :: Text -> Either Text Text
gatewayAddress cidr = do
  (addr, prefix) <- parseCidr cidr
  let mask = prefixToMask prefix
      network = addr .&. mask
  pure $ ipToText (network + 1)

-- | Get the DHCP range start (second usable IP) from a CIDR subnet.
dhcpRangeStart :: Text -> Either Text Text
dhcpRangeStart cidr = do
  (addr, prefix) <- parseCidr cidr
  let mask = prefixToMask prefix
      network = addr .&. mask
  pure $ ipToText (network + 2)

-- | Get the DHCP range end (last usable IP before broadcast) from a CIDR subnet.
dhcpRangeEnd :: Text -> Either Text Text
dhcpRangeEnd cidr = do
  (addr, prefix) <- parseCidr cidr
  let mask = prefixToMask prefix
      network = addr .&. mask
      broadcast = network + complement mask
  pure $ ipToText (broadcast - 1)

-- | Extract the prefix length from a CIDR string.
prefixLength :: Text -> Either Text Text
prefixLength cidr = do
  (_, prefix) <- parseCidr cidr
  pure $ T.pack (show prefix)

-- | Compute the subnet mask from a CIDR string (e.g., "255.255.255.0").
subnetMask :: Text -> Either Text Text
subnetMask cidr = do
  (_, prefix) <- parseCidr cidr
  pure $ ipToText (prefixToMask prefix)

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

-- | Parse a CIDR string into (network address, prefix length).
parseCidr :: Text -> Either Text (Word32, Int)
parseCidr cidr = case T.splitOn "/" (T.strip cidr) of
  [addrPart, prefixPart] -> do
    addr <- parseIpv4 addrPart
    prefix <- case readMaybe (T.unpack prefixPart) of
      Just p
        | p >= 1 && p <= 30 -> Right p
      _ -> Left $ "Invalid prefix length: " <> prefixPart <> " (must be 1-30)"
    pure (addr, prefix)
  _ -> Left $ "Invalid CIDR notation: " <> cidr <> " (expected format: A.B.C.D/N)"

-- | Parse a dotted-quad IPv4 address into a Word32.
parseIpv4 :: Text -> Either Text Word32
parseIpv4 txt = case map (readMaybe . T.unpack) (T.splitOn "." txt) of
  [Just a, Just b, Just c, Just d]
    | all (\x -> x >= 0 && x <= 255) [a, b, c, d] ->
        Right $
          fromIntegral a `shiftL` 24
            + fromIntegral b `shiftL` 16
            + fromIntegral (c :: Int) `shiftL` 8
            + fromIntegral d
  _ -> Left $ "Invalid IPv4 address: " <> txt

-- | Convert a prefix length to a subnet mask.
prefixToMask :: Int -> Word32
prefixToMask n = complement (shiftL 1 (32 - n) - 1)

-- | Convert a Word32 to dotted-quad text.
ipToText :: Word32 -> Text
ipToText w =
  T.pack $
    intercalate
      "."
      [ show (shiftR w 24 .&. 0xFF)
      , show (shiftR w 16 .&. 0xFF)
      , show (shiftR w 8 .&. 0xFF)
      , show (w .&. 0xFF)
      ]
