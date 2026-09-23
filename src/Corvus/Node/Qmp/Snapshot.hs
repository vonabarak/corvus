{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Live and full-machine snapshot QMP operations.
module Corvus.Node.Qmp.Snapshot where

import Control.Concurrent (threadDelay)
import Corvus.Model (CacheType (..), DriveFormat (..), DriveInterface (..), EnumText (..))
import Corvus.Node.Qmp.Block (qmpFindBlockNodeByPath)
import Corvus.Node.Qmp.Runtime (pollQmpJob)
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

-- Full-machine snapshots (vmstate + block, via snapshot-save / snapshot-load)
--------------------------------------------------------------------------------
--
-- QEMU 6.0+ exposes 'snapshot-save', 'snapshot-load', and
-- 'snapshot-delete' as asynchronous jobs. Each operation is delegated to
-- the shared QMP job poller after QEMU accepts its command.

-- | Generate a per-call job-id from the VM id and tag.
mkJobId :: Text -> Int64 -> Text -> Text
mkJobId verb vmId tag = "corvus-" <> verb <> "-" <> T.pack (show vmId) <> "-" <> tag

-- | Save a full-machine (vmstate + block) snapshot asynchronously.
qmpSnapshotSave :: QemuConfig -> Int64 -> Text -> Text -> [Text] -> IO (Either Text ())
qmpSnapshotSave config vmId tag vmstateDevice devices = do
  let jobId = mkJobId "save" vmId tag
      body =
        A.object
          [ "execute" A..= A.String "snapshot-save"
          , "arguments"
              A..= A.object
                [ "job-id" A..= A.String jobId
                , "tag" A..= A.String tag
                , "vmstate" A..= A.String vmstateDevice
                , "devices" A..= A.toJSON devices
                ]
          ]
  fire <- sendQmpCommand config vmId (LBS.toStrict (A.encode body))
  case fire of
    QmpSuccess -> pollQmpJob config vmId jobId
    QmpError msg -> pure (Left msg)
    QmpConnectionFailed msg -> pure (Left msg)

-- | Load a full-machine snapshot asynchronously. The caller must pause the VM.
qmpSnapshotLoad :: QemuConfig -> Int64 -> Text -> Text -> [Text] -> IO (Either Text ())
qmpSnapshotLoad config vmId tag vmstateDevice devices = do
  let jobId = mkJobId "load" vmId tag
      body =
        A.object
          [ "execute" A..= A.String "snapshot-load"
          , "arguments"
              A..= A.object
                [ "job-id" A..= A.String jobId
                , "tag" A..= A.String tag
                , "vmstate" A..= A.String vmstateDevice
                , "devices" A..= A.toJSON devices
                ]
          ]
  fire <- sendQmpCommand config vmId (LBS.toStrict (A.encode body))
  case fire of
    QmpSuccess -> pollQmpJob config vmId jobId
    QmpError msg -> pure (Left msg)
    QmpConnectionFailed msg -> pure (Left msg)

-- | Delete a full-machine snapshot asynchronously.
qmpSnapshotDelete :: QemuConfig -> Int64 -> Text -> [Text] -> IO (Either Text ())
qmpSnapshotDelete config vmId tag devices = do
  let jobId = mkJobId "del" vmId tag
      body =
        A.object
          [ "execute" A..= A.String "snapshot-delete"
          , "arguments"
              A..= A.object
                [ "job-id" A..= A.String jobId
                , "tag" A..= A.String tag
                , "devices" A..= A.toJSON devices
                ]
          ]
  fire <- sendQmpCommand config vmId (LBS.toStrict (A.encode body))
  case fire of
    QmpSuccess -> pollQmpJob config vmId jobId
    QmpError msg -> pure (Left msg)
    QmpConnectionFailed msg -> pure (Left msg)

-- Live (online) qcow2 internal snapshots
--------------------------------------------------------------------------------
--
-- 'blockdev-snapshot-internal-sync' is the online equivalent of
-- @qemu-img snapshot -c@: it writes a snapshot record into qcow2 metadata
-- while the VM is running. There is no online rollback equivalent; Corvus
-- performs that operation with the VM stopped.

-- | Issue @blockdev-snapshot-internal-sync@ to stamp a named
-- snapshot into the qcow2 metadata for the running VM's named
-- block device.
qmpBlockSnapshotCreate
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- \^ device or node name
  -> Text
  -- ^ snapshot name
  -> IO QmpResult
qmpBlockSnapshotCreate config vmId device name =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "blockdev-snapshot-internal-sync",
        "arguments": {
          "device": #{device},
          "name": #{name}
        }
      }
    |]

-- | Issue ONE QMP @transaction@ wrapping N
-- @blockdev-snapshot-internal-sync@ actions, all sharing the same
-- snapshot name. Either every disk's snapshot lands or none of them
-- do — QEMU rolls the whole transaction back on any action failure,
-- which is what the build-step cache wants (a partial cache row is
-- worse than no cache row at all).
--
-- Caller must have resolved each disk's device or node name via
-- 'qmpFindBlockDeviceByPath' first; this function just bundles
-- the pre-resolved @(device, snapshotName)@ pairs into the single
-- atomic command.
qmpBlockSnapshotCreateMany
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> [(Text, Text)]
  -- ^ @(device, snapshotName)@ pairs
  -> IO QmpResult
qmpBlockSnapshotCreateMany _ _ [] = pure QmpSuccess
qmpBlockSnapshotCreateMany config vmId pairs =
  sendQmpCommand config vmId (LBS.toStrict (A.encode body))
  where
    body =
      A.object
        [ "execute" A..= A.String "transaction"
        , "arguments"
            A..= A.object
              [ "actions" A..= A.toJSON (map snapAction pairs)
              ]
        ]
    snapAction (dev, name) =
      A.object
        [ "type" A..= A.String "blockdev-snapshot-internal-sync"
        , "data"
            A..= A.object
              [ "device" A..= A.String dev
              , "name" A..= A.String name
              ]
        ]

-- | Issue @blockdev-snapshot-delete-internal-sync@ to remove a
-- named snapshot from the qcow2 metadata of a running VM's named
-- block device. The current disk state is preserved (the snapshot
-- record is just dropped).
qmpBlockSnapshotDelete
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- ^ device name
  -> Text
  -- ^ snapshot name
  -> IO QmpResult
qmpBlockSnapshotDelete config vmId device name =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "blockdev-snapshot-delete-internal-sync",
        "arguments": {
          "device": #{device},
          "name": #{name}
        }
      }
    |]

-- | Look up the QEMU device identifier for a given absolute file
-- path on a running VM. @query-block@ returns one entry per
-- BlockBackend with @{ device, inserted: { file, ... }, ... }@;
-- we walk the list and match @inserted.file@ exactly. Drives created
-- through @-blockdev@ do not get a BlockBackend, so their matching
-- entry has an empty @device@. In that case we fall back to the
-- BlockDriverState node name from @query-named-block-nodes@, which
-- the snapshot commands also accept.
--
-- Returns @Left@ when the QMP call itself fails or the path isn't
-- attached to the VM. The error message names the path so the
-- caller can include it verbatim in any user-facing error.
qmpFindBlockDeviceByPath
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> FilePath
  -- ^ absolute file path to match
  -> IO (Either Text Text)
qmpFindBlockDeviceByPath config vmId path = do
  raw <- sendQmpRaw config vmId [qmpQQ| { "execute": "query-block" } |]
  case raw >>= extractReplyLine of
    Left err -> pure (Left err)
    Right line ->
      case A.eitherDecodeStrict line of
        Left e -> pure (Left (T.pack ("query-block decode: " <> e)))
        Right (QueryBlockReply rows) ->
          case filter ((== T.pack path) . qbiFilename) rows of
            (m : _) | not (T.null (qbiDevice m)) -> pure (Right (qbiDevice m))
            _ -> qmpFindBlockNodeByPath config vmId path

data QueryBlockItem = QueryBlockItem
  { qbiDevice :: !Text
  , qbiFilename :: !Text
  }
  deriving (Eq, Show)

newtype QueryBlockReply = QueryBlockReply [QueryBlockItem]

instance A.FromJSON QueryBlockReply where
  parseJSON = A.withObject "QueryBlockReply" $ \o ->
    QueryBlockReply <$> o A..: "return"

instance A.FromJSON QueryBlockItem where
  parseJSON = A.withObject "QueryBlockItem" $ \o -> do
    dev <- o A..: "device"
    inserted <- o A..:? "inserted"
    fname <- case inserted of
      Just ins -> ins A..: "file"
      Nothing -> pure ""
    pure (QueryBlockItem dev fname)

--------------------------------------------------------------------------------
