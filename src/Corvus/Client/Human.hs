{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | CLI presentation encoding. Machine JSON uses the original DTO encoders.
module Corvus.Client.Human (HumanJSON (..)) where

import Corvus.Protocol
import Corvus.Size (formatSize)
import Data.Aeson (ToJSON, Value (..), toJSON)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Vector as V

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
