{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Block-device and hotplug QMP operations.
module Corvus.Node.Qmp.Block where

import Control.Concurrent (threadDelay)
import Corvus.Model (CacheType (..), DriveFormat (..), DriveInterface (..), EnumText (..))
import Corvus.Node.Qmp.Transport (classifyQmpResponse, extractReplyLine, sendQmpCommand, sendQmpRaw)
import Corvus.Node.Qmp.Types (QmpMigrationStatus (..), QmpResult (..))
import Corvus.Node.QmpQQ (qmpQQ)
import Corvus.Node.Runtime (shellQuotePath)
import Corvus.Qemu.Config (QemuConfig)
import qualified Data.Aeson as A
import qualified Data.ByteString as BSWide
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- Media eject / change (CD-ROM drives)
--------------------------------------------------------------------------------

-- Hot-plug block devices
--------------------------------------------------------------------------------

-- | Add a block device for hot-plug.
qmpBlockdevAdd :: QemuConfig -> Int64 -> Text -> FilePath -> DriveFormat -> Bool -> CacheType -> Bool -> IO QmpResult
qmpBlockdevAdd config vmId nodeName filePath format readOnly cache discard = do
  let formatStr = enumToText format
      filePathText = T.pack filePath
      (cacheDirect, cacheNoFlush, _) = cacheSettings cache
      discardMode :: Text
      discardMode = if discard then "unmap" else "ignore"
      cmd =
        [qmpQQ|
          {
            "execute": "blockdev-add",
            "arguments": {
              "driver": #{formatStr},
              "node-name": #{nodeName},
              "read-only": #{readOnly},
              "cache": {
                "direct": #{cacheDirect},
                "no-flush": #{cacheNoFlush}
              },
              "discard": #{discardMode},
              "file": {
                "driver": "file",
                "filename": #{filePathText},
                "read-only": #{readOnly}
              }
            }
          }
        |]
  sendQmpCommand config vmId cmd

-- | Add a device backed by a hot-plug block node.
qmpDeviceAddDrive :: QemuConfig -> Int64 -> Text -> Text -> DriveInterface -> Text -> CacheType -> Bool -> IO QmpResult
qmpDeviceAddDrive config vmId deviceId nodeName iface media cache discard =
  case iface of
    InterfaceVirtio ->
      sendQmpCommand
        config
        vmId
        [qmpQQ|
        { "execute": "device_add", "arguments": {
          "driver": "virtio-blk-pci", "id": #{deviceId}, "drive": #{nodeName},
          "bus": "hotplug", "write-cache": #{writeCache}, "discard": #{discard}
        }}
      |]
    _ ->
      sendQmpCommand
        config
        vmId
        [qmpQQ|
        { "execute": "device_add", "arguments": {
          "driver": #{driver}, "id": #{deviceId}, "drive": #{nodeName},
          "bus": #{bus}, "write-cache": #{writeCache}
        }}
      |]
  where
    (driver, bus) = deviceDriverAndBus iface media
    (_, _, writeCacheEnabled) = cacheSettings cache
    writeCache = if writeCacheEnabled then ("on" :: Text) else "off"

-- | Map a drive interface to its QEMU device driver and bus.
deviceDriverAndBus :: DriveInterface -> Text -> (Text, Text)
deviceDriverAndBus InterfaceVirtio _ = ("virtio-blk-pci", "hotplug")
deviceDriverAndBus InterfaceScsi "cdrom" = ("scsi-cd", "scsi0.0")
deviceDriverAndBus InterfaceScsi _ = ("scsi-hd", "scsi0.0")
deviceDriverAndBus InterfaceIde _ = ("ide-hd", "hotplug")
deviceDriverAndBus InterfaceSata _ = ("ide-hd", "hotplug")
deviceDriverAndBus InterfaceNvme _ = ("nvme", "hotplug")
deviceDriverAndBus InterfacePflash _ = ("pflash", "hotplug")
deviceDriverAndBus InterfaceFloppy _ = ("floppy", "hotplug")

-- | Translate Corvus cache modes into QEMU block-device settings.
cacheSettings :: CacheType -> (Bool, Bool, Bool)
cacheSettings CacheNone = (True, True, True)
cacheSettings CacheWriteback = (False, False, True)
cacheSettings CacheWritethrough = (False, False, False)
cacheSettings CacheDirectsync = (True, False, True)
cacheSettings CacheUnsafe = (False, True, True)

-- | Remove a hot-plugged device.
qmpDeviceDel :: QemuConfig -> Int64 -> Text -> IO QmpResult
qmpDeviceDel config vmId deviceId =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
    { "execute": "device_del", "arguments": { "id": #{deviceId} } }
  |]

-- | Remove a hot-plugged block node after its device is removed.
qmpBlockdevDel :: QemuConfig -> Int64 -> Text -> IO QmpResult
qmpBlockdevDel config vmId nodeName =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
    { "execute": "blockdev-del", "arguments": { "node-name": #{nodeName} } }
  |]

-- | A single entry from a QMP @query-block@ reply.
--
-- 'beDevice' is present only for legacy @-drive id=...@ backends —
-- the ones the @eject@ and @blockdev-change-medium@ commands can
-- address; blockdev-graph-only drives carry no @device@ field.
-- 'beInserted' is absent (Nothing) while the tray is empty.
data BlockEntry = BlockEntry
  { beDevice :: !(Maybe Text)
  , beRemovable :: !Bool
  , beTrayOpen :: !Bool
  , beInserted :: !(Maybe A.Value)
  }
  deriving (Eq, Show)

instance A.FromJSON BlockEntry where
  parseJSON = A.withObject "BlockEntry" $ \o -> do
    rem <- o A..: "removable"
    -- 'tray_open' is @allow-omitted@ in QEMU's BlockInfo: it is present
    -- only for removable cdrom block devices, absent on plain disks.
    openM <- o A..:? "tray_open"
    deviceM <- o A..:? "device"
    insertedM <- o A..:? "inserted"
    pure
      BlockEntry
        { beDevice = deviceM
        , beRemovable = rem
        , beTrayOpen = fromMaybe False openM
        , beInserted = fromMaybe Nothing insertedM
        }

-- | Issue @query-block@ and decode the drive list. Used by the
-- media eject / change capability check to find the legacy
-- @-drive id=...@ backend for a drive and verify @removable@.
qmpQueryBlock :: QemuConfig -> Int64 -> IO (Either Text [BlockEntry])
qmpQueryBlock config vmId = do
  raw <- sendQmpRaw config vmId [qmpQQ| { "execute": "query-block" } |]
  pure $ do
    bs <- raw
    line <- extractReplyLine bs
    case A.eitherDecodeStrict line of
      Left e -> Left (T.pack ("query-block decode: " <> e))
      Right (BlockReply entries) -> Right entries

newtype BlockReply = BlockReply [BlockEntry]

instance A.FromJSON BlockReply where
  parseJSON = A.withObject "BlockReply" $ \o ->
    BlockReply <$> o A..: "return"

-- | Eject the media of a CD-ROM drive (@eject@). 'driveNode' is
-- the legacy @-drive id=...@ backend name (@"drive-<N>"@).
qmpEject :: QemuConfig -> Int64 -> Text -> IO QmpResult
qmpEject config vmId driveNode =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "eject",
        "arguments": {
          "device": #{driveNode}
        }
      }
    |]

-- | Replace the media of a CD-ROM drive (@blockdev-change-medium@).
-- 'mFormat' is the image format name (@raw@ / @qcow2@); pass
-- Nothing to let QEMU probe it.
qmpChangeMedium
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- ^ Legacy @-drive id=...@ backend name (@"drive-<N>"@)
  -> Text
  -- ^ New image file path
  -> Maybe Text
  -- ^ Image format (omitted when Nothing)
  -> IO QmpResult
qmpChangeMedium config vmId driveNode filePath mFormat =
  case mFormat of
    Nothing ->
      sendQmpCommand
        config
        vmId
        [qmpQQ|
          {
            "execute": "blockdev-change-medium",
            "arguments": {
              "device": #{driveNode},
              "filename": #{filePath}
            }
          }
        |]
    Just fmt ->
      sendQmpCommand
        config
        vmId
        [qmpQQ|
          {
            "execute": "blockdev-change-medium",
            "arguments": {
              "device": #{driveNode},
              "filename": #{filePath},
              "format": #{fmt}
            }
          }
        |]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Block-node discovery
--------------------------------------------------------------------------------

-- | Look up the QEMU block-node name (BlockDriverState @node-name@)
-- for a given absolute file path on a running VM.
-- 'query-named-block-nodes' returns one entry per BDS with a
-- 'node-name', a 'file' (the absolute path), an 'ro' bool, and a
-- 'drv' (driver, e.g. @qcow2@). We match on 'file' exactly and
-- return the 'node-name'.
--
-- Returns @Left@ when the QMP call itself fails or no BDS matches
-- the path. The error message names the path so the caller can
-- include it verbatim in a user-facing error.
--
-- Distinct from 'qmpFindBlockDeviceByPath', which resolves to the
-- BlockBackend namespace consumed by 'blockdev-snapshot-internal-sync'.
qmpFindBlockNodeByPath
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> FilePath
  -- ^ absolute file path to match
  -> IO (Either Text Text)
qmpFindBlockNodeByPath config vmId path = do
  raw <- sendQmpRaw config vmId [qmpQQ| { "execute": "query-named-block-nodes" } |]
  pure $ do
    bs <- raw
    line <- extractReplyLine bs
    case A.eitherDecodeStrict line of
      Left e -> Left (T.pack ("query-named-block-nodes decode: " <> e))
      Right (QueryNamedBlockNodesReply rows) ->
        case filter ((== T.pack path) . qnbnFile) rows of
          (m : _) -> Right (qnbnNodeName m)
          [] ->
            Left $
              "no block-node attached to VM "
                <> T.pack (show vmId)
                <> " has file="
                <> T.pack path

data QueryNamedBlockNode = QueryNamedBlockNode
  { qnbnNodeName :: !Text
  , qnbnFile :: !Text
  }
  deriving (Eq, Show)

newtype QueryNamedBlockNodesReply = QueryNamedBlockNodesReply [QueryNamedBlockNode]

instance A.FromJSON QueryNamedBlockNodesReply where
  parseJSON = A.withObject "QueryNamedBlockNodesReply" $ \o ->
    QueryNamedBlockNodesReply <$> o A..: "return"

instance A.FromJSON QueryNamedBlockNode where
  parseJSON = A.withObject "QueryNamedBlockNode" $ \o -> do
    nn <- o A..: "node-name"
    fp <- o A..:? "file" A..!= ""
    pure (QueryNamedBlockNode nn fp)
