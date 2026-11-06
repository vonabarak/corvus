{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | CLI presentation encoding. Machine JSON uses the original DTO encoders.
module Corvus.Client.Human (HumanJSON (..), formatHumanSize, formatLoad) where

import Corvus.Protocol
import Corvus.Size (formatSize)
import Data.Aeson (ToJSON, Value (..), toJSON)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Compact text sizes; YAML and configuration continue to use exact sizes.
formatHumanSize :: (Integral a) => a -> String
formatHumanSize input
  | length (filter isDigit exact) <= 4 = exact
  | otherwise = "~" ++ sign ++ approximate power
  where
    exact = formatSize input
    bytes = abs (toInteger input)
    sign = if input < 0 then "-" else ""
    power = length (takeWhile (<= bytes) [1024 ^ p | p <- [1 .. 4 :: Int]])
    approximate p =
      let factor = 1024 ^ p
          places = 4 - length (show (bytes `div` factor))
          numerator = bytes * 10 ^ max 0 places
          denominator = factor * 10 ^ max 0 (-places)
          rounded = (2 * numerator + denominator) `div` (2 * denominator)
          scaled = rounded * 10 ^ max 0 (-places)
          decimals = max 0 places
       in if p < 4 && scaled >= 1024 * 10 ^ decimals
            then approximate (p + 1)
            else renderFixed scaled decimals ++ unit p
    unit p = case p of
      1 -> "K"
      2 -> "M"
      3 -> "G"
      4 -> "T"
      _ -> "B"

-- | Fixed notation with up to four decimal places, including plain zero.
formatLoad :: Double -> String
formatLoad value = renderFixed (round (value * 10000)) 4

renderFixed :: Integer -> Int -> String
renderFixed scaled places
  | places == 0 = show scaled
  | otherwise = sign ++ show whole ++ fraction
  where
    sign = if scaled < 0 then "-" else ""
    (whole, remainder) = abs scaled `divMod` (10 ^ places)
    digits = show remainder
    padded = replicate (places - length digits) '0' ++ digits
    trimmed = reverse (dropWhile (== '0') (reverse padded))
    fraction = if null trimmed then "" else '.' : trimmed

class (ToJSON a) => HumanJSON a where
  humanJSON :: a -> Value

instance {-# OVERLAPPABLE #-} (ToJSON a) => HumanJSON a where
  humanJSON = toJSON

instance {-# OVERLAPPING #-} (HumanJSON a) => HumanJSON [a] where
  humanJSON = Array . V.fromList . map humanJSON

fields :: Value -> [(Key, Value)] -> Value
fields (Object o) replacements = Object (foldr (\(k, v) acc -> if v == Null && not (KM.member k acc) then acc else KM.insert k v acc) o replacements)
fields v _ = v

sizeValue :: (Integral a) => a -> Value
sizeValue = String . T.pack . formatSize

optionalSize :: (Integral a) => Maybe a -> Value
optionalSize = maybe Null sizeValue

instance {-# OVERLAPPING #-} HumanJSON DiskImageInfo where
  humanJSON a = fields (toJSON a) [("size", optionalSize (diiSize a))]

instance {-# OVERLAPPING #-} HumanJSON SnapshotInfo where
  humanJSON a = fields (toJSON a) [("size", optionalSize (sniSize a))]

instance {-# OVERLAPPING #-} HumanJSON VmInfo where
  humanJSON a = fields (toJSON a) [("ram", sizeValue (viRam a))]

instance {-# OVERLAPPING #-} HumanJSON VmDetails where
  humanJSON a = fields (toJSON a) [("ram", sizeValue (vdRam a))]

instance {-# OVERLAPPING #-} HumanJSON VmSnapshotInfo where
  humanJSON a = fields (toJSON a) [("total_size", sizeValue (vsiTotalSize a))]

instance {-# OVERLAPPING #-} HumanJSON NodeInfo where
  humanJSON a = fields (toJSON a) [("ram_total", optionalSize (noiRamTotal a)), ("ram_free", optionalSize (noiRamFree a)), ("storage_bytes_total", optionalSize (noiStorageBytesTotal a)), ("storage_bytes_free", optionalSize (noiStorageBytesFree a))]

instance {-# OVERLAPPING #-} HumanJSON NodeDetails where
  humanJSON a = fields (toJSON a) [("ram_total", optionalSize (nodRamTotal a)), ("ram_free", optionalSize (nodRamFree a)), ("storage_bytes_total", optionalSize (nodStorageBytesTotal a)), ("storage_bytes_free", optionalSize (nodStorageBytesFree a))]

instance {-# OVERLAPPING #-} HumanJSON TemplateVmInfo where
  humanJSON a = fields (toJSON a) [("ram", sizeValue (tviRam a))]

instance {-# OVERLAPPING #-} HumanJSON TemplateDriveInfo where
  humanJSON a = fields (toJSON a) [("size", optionalSize (tvdiSize a))]

instance {-# OVERLAPPING #-} HumanJSON TemplateDetails where
  humanJSON a = fields (toJSON a) [("ram", sizeValue (tvdRam a)), ("drives", humanJSON (tvdDrives a))]

instance {-# OVERLAPPING #-} HumanJSON VmStats where
  humanJSON a = fields (toJSON a) [("host_rss_bytes", sizeValue (vstHostRssBytes a)), ("balloon_actual_bytes", sizeValue (vstBalloonActualBytes a)), ("balloon_max_bytes", sizeValue (vstBalloonMaxBytes a)), ("drives", humanJSON (vstDrives a)), ("nets", humanJSON (vstNets a))]

instance {-# OVERLAPPING #-} HumanJSON DriveIo where
  humanJSON a = fields (toJSON a) [("read_bytes_total", sizeValue (dioReadBytesTotal a)), ("write_bytes_total", sizeValue (dioWriteBytesTotal a))]

instance {-# OVERLAPPING #-} HumanJSON NetIo where
  humanJSON a = fields (toJSON a) [("rx_bytes_total", sizeValue (nioRxBytesTotal a)), ("tx_bytes_total", sizeValue (nioTxBytesTotal a))]
