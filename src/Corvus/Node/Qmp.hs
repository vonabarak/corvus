{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | QMP (QEMU Machine Protocol) interaction.
-- Provides functions to control running VMs via QMP.
module Corvus.Node.Qmp
  ( -- * Types
    QmpResult (..)
  , QmpMigrationStatus (..)
  , BlockstatsRow (..)

    -- * Commands
  , qmpShutdown
  , qmpContinue
  , qmpStop
  , qmpQuit

    -- * Migration (save / load coordination)
  , qmpMigrate
  , qmpQueryMigrate

    -- * SPICE ticket commands
  , qmpSetSpicePassword
  , qmpExpireSpicePassword

    -- * Input injection
  , qmpSendCtrlAltDel
  , qmpSendKey

    -- * Hot-plug commands
  , qmpBlockdevAdd
  , qmpDeviceAddDrive
  , qmpDeviceDel
  , qmpBlockdevDel

    -- * Live snapshots (qcow2 internal, online)
  , qmpBlockSnapshotCreate
  , qmpBlockSnapshotDelete
  , qmpFindBlockDeviceByPath

    -- * Stats sampling
  , qmpQueryBlockstats
  , qmpQueryBalloon

    -- * Low-level
  , classifyQmpResponse

    -- * Re-export quasi-quoter
  , qmpQQ
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, bracket, catch, try)
import Corvus.Model (DriveFormat (..), DriveInterface (..), EnumText (..))
import Corvus.Node.QmpQQ (qmpQQ)
import Corvus.Node.Runtime (getQmpSocket, shellQuotePath)
import Corvus.Qemu.Config (QemuConfig)
import qualified Data.Aeson as A
import qualified Data.ByteString as BSWide
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.IO.Exception (IOErrorType (..))
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..), close, connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv, sendAll)
import System.IO.Error (ioeGetErrorType)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Result of a QMP command
data QmpResult
  = QmpSuccess
  | QmpError !Text
  | QmpConnectionFailed !Text
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- QMP Commands
--------------------------------------------------------------------------------

-- | Send graceful shutdown command via QMP
qmpShutdown :: QemuConfig -> Int64 -> IO QmpResult
qmpShutdown config vmId =
  sendQmpCommand config vmId [qmpQQ| { "execute": "system_powerdown" } |]

-- | Send continue command via QMP (resume paused VM)
qmpContinue :: QemuConfig -> Int64 -> IO QmpResult
qmpContinue config vmId =
  sendQmpCommand config vmId [qmpQQ| { "execute": "cont" } |]

-- | Send stop command via QMP (pause VM)
qmpStop :: QemuConfig -> Int64 -> IO QmpResult
qmpStop config vmId =
  sendQmpCommand config vmId [qmpQQ| { "execute": "stop" } |]

-- | Send the QMP @quit@ command, asking QEMU to exit cleanly. Used
-- by the save flow once the outgoing @migrate@ has reported
-- @completed@.
qmpQuit :: QemuConfig -> Int64 -> IO QmpResult
qmpQuit config vmId =
  sendQmpCommand config vmId [qmpQQ| { "execute": "quit" } |]

--------------------------------------------------------------------------------
-- Migration (save / load coordination)
--------------------------------------------------------------------------------

-- | Status returned by QMP @query-migrate@. The full QEMU vocabulary
-- ('none', 'setup', 'cancelling', 'cancelled', 'active',
-- 'postcopy-active', 'postcopy-paused', 'postcopy-recover-setup',
-- 'postcopy-recover', 'completed', 'failed', 'colo',
-- 'pre-switchover', 'device', 'wait-unplug') is collapsed into the
-- four states the save/load coordinator actually cares about. Any
-- terminal failure (including @cancelled@) lands in 'MigFailed' with
-- the verbatim response payload for the daemon to surface.
data QmpMigrationStatus
  = MigInactive
  | MigActive
  | MigCompleted
  | MigFailed !Text
  deriving (Eq, Show)

-- | Issue @migrate "exec:zstd -T0 > <path>"@. QMP returns
-- immediately on accepting the command; the actual transfer
-- happens asynchronously — poll 'qmpQueryMigrate' for completion.
--
-- The @exec:@ URI pipes the migration stream through @zstd@
-- (multi-threaded, default level 3) so the saved RAM image lands
-- compressed. For a typical guest with mostly-zero or
-- highly-redundant RAM this shrinks the on-disk file by ~5–10×
-- and shrinks the cross-host transfer in 'crv vm migrate' by the
-- same factor; CPU cost is comfortably below the network or
-- disk bandwidth on every realistic host.
--
-- The path is single-quoted via 'shellQuotePath' — QEMU spawns
-- the command via @\/bin\/sh -c@, so unquoted spaces or shell
-- metachars in @basePath@ would tear the command apart.
qmpMigrate :: QemuConfig -> Int64 -> FilePath -> IO QmpResult
qmpMigrate config vmId path =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "migrate",
        "arguments": {
          "uri": #{T.pack ("exec:zstd -T0 > " <> shellQuotePath path)}
        }
      }
    |]

-- | Issue @query-migrate@ and classify the response. Detection is
-- substring-based on the JSON payload — matches the existing
-- 'classifyQmpResponse' style and avoids pulling aeson into this
-- module. The QMP wire format guarantees the literal
-- @"status": "<value>"@ key appears in the @return@ object (modulo
-- whitespace, which QEMU does not emit).
qmpQueryMigrate :: QemuConfig -> Int64 -> IO (Either Text QmpMigrationStatus)
qmpQueryMigrate config vmId = do
  qmpSock <- getQmpSocket config vmId
  result <- try $ withUnixSocket qmpSock $ \sock -> do
    _ <- recv sock 4096
    sendAll sock [qmpQQ| { "execute": "qmp_capabilities" } |]
    _ <- drainUntilReply sock BS.empty
    sendAll sock [qmpQQ| { "execute": "query-migrate" } |]
    drainUntilReply sock BS.empty
  pure $ case result of
    Left (e :: SomeException) -> Left $ T.pack $ show e
    Right response
      | BS.isInfixOf "\"error\"" response ->
          Left $ T.pack $ BS.unpack response
      | BS.isInfixOf "\"status\": \"completed\"" response ->
          Right MigCompleted
      | BS.isInfixOf "\"status\": \"failed\"" response ->
          Right (MigFailed (T.pack (BS.unpack response)))
      | BS.isInfixOf "\"status\": \"cancelled\"" response ->
          Right (MigFailed "migration cancelled")
      | BS.isInfixOf "\"status\": \"cancelling\"" response ->
          Right (MigFailed "migration cancelling")
      | BS.isInfixOf "\"status\": \"active\"" response
          || BS.isInfixOf "\"status\": \"setup\"" response
          || BS.isInfixOf "\"status\": \"device\"" response
          || BS.isInfixOf "\"status\": \"pre-switchover\"" response
          || BS.isInfixOf "\"status\": \"wait-unplug\"" response ->
          Right MigActive
      | BS.isInfixOf "\"status\": \"none\"" response ->
          Right MigInactive
      -- Anything else (e.g. postcopy variants we don't trigger):
      -- treat as still-running so the caller keeps polling. If it
      -- never resolves, the caller's own timeout fires.
      | otherwise -> Right MigActive

-- | Install a fresh SPICE password on a running VM via QMP. Uses
-- @connected: "keep"@ so an already-connected viewer is not dropped when
-- the password rotates.
qmpSetSpicePassword :: QemuConfig -> Int64 -> Text -> IO QmpResult
qmpSetSpicePassword config vmId password =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "set_password",
        "arguments": {
          "protocol": "spice",
          "password": #{password},
          "connected": "keep"
        }
      }
    |]

-- | Schedule the current SPICE password to expire. Accepts a relative
-- time in seconds (@+N@ semantics — "expire N seconds from now").
qmpExpireSpicePassword :: QemuConfig -> Int64 -> Int -> IO QmpResult
qmpExpireSpicePassword config vmId seconds = do
  let ttl = T.pack ("+" ++ show seconds)
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "expire_password",
        "arguments": {
          "protocol": "spice",
          "time": #{ttl}
        }
      }
    |]

-- | Inject a Ctrl+Alt+Del key combination via QMP @send-key@. This is
-- how 'crv vm view' delivers the escape-prefix @d@ command now that
-- the HMP monitor socket is exclusively held by the daemon's ring
-- buffer thread.
qmpSendCtrlAltDel :: QemuConfig -> Int64 -> IO QmpResult
qmpSendCtrlAltDel config vmId =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "send-key",
        "arguments": {
          "keys": [
            { "type": "qcode", "data": "ctrl" },
            { "type": "qcode", "data": "alt" },
            { "type": "qcode", "data": "delete" }
          ]
        }
      }
    |]

-- | Send a sequence of QEMU @qcode@ keys to the VM as a single chord
-- (all pressed and released together). For the @crv build@ installer
-- strategy this is used to dismiss the "Press any key to boot from CD"
-- prompt at UEFI firmware time. Use one call per key press; the QEMU
-- @qcode@ vocabulary covers @ret@, @esc@, @spc@, @tab@, @up@/@down@,
-- the alphanumerics, etc.
qmpSendKey :: QemuConfig -> Int64 -> Text -> IO QmpResult
qmpSendKey config vmId qcode =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "send-key",
        "arguments": {
          "keys": [
            { "type": "qcode", "data": #{qcode} }
          ]
        }
      }
    |]

--------------------------------------------------------------------------------
-- Hot-plug Commands
--------------------------------------------------------------------------------

-- | Add a block device (for hot-plug)
qmpBlockdevAdd
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- ^ Node name (unique identifier for this block device)
  -> FilePath
  -- ^ File path to disk image
  -> DriveFormat
  -- ^ Disk format
  -> Bool
  -- ^ Read-only mode
  -> IO QmpResult
qmpBlockdevAdd config vmId nodeName filePath format readOnly = do
  let formatStr = enumToText format
      filePathText = T.pack filePath
      cmd =
        [qmpQQ|
          {
            "execute": "blockdev-add",
            "arguments": {
              "driver": #{formatStr},
              "node-name": #{nodeName},
              "read-only": #{readOnly},
              "file": {
                "driver": "file",
                "filename": #{filePathText},
                "read-only": #{readOnly}
              }
            }
          }
        |]
  sendQmpCommand config vmId cmd

-- | Add a device using a block device (for hot-plug)
qmpDeviceAddDrive
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- ^ Device ID (unique identifier for this device)
  -> Text
  -- ^ Block device node name (from qmpBlockdevAdd)
  -> DriveInterface
  -- ^ Drive interface type
  -> IO QmpResult
qmpDeviceAddDrive config vmId deviceId nodeName iface = do
  let driver = T.pack $ interfaceToDriver iface
      bus = "hotplug" :: Text
      cmd =
        [qmpQQ|
          {
            "execute": "device_add",
            "arguments": {
              "driver": #{driver},
              "id": #{deviceId},
              "drive": #{nodeName},
              "bus": #{bus}
            }
          }
        |]
  sendQmpCommand config vmId cmd

-- | Map drive interface to QEMU device driver name
interfaceToDriver :: DriveInterface -> String
interfaceToDriver InterfaceVirtio = "virtio-blk-pci"
interfaceToDriver InterfaceIde = "ide-hd"
interfaceToDriver InterfaceScsi = "scsi-hd"
interfaceToDriver InterfaceSata = "ide-hd"
interfaceToDriver InterfaceNvme = "nvme"
interfaceToDriver InterfacePflash = "pflash"
interfaceToDriver InterfaceFloppy = "floppy"

-- | Remove a device (for hot-unplug)
qmpDeviceDel
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- ^ Device ID (same as used in qmpDeviceAddDrive)
  -> IO QmpResult
qmpDeviceDel config vmId deviceId =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "device_del",
        "arguments": {
          "id": #{deviceId}
        }
      }
    |]

-- | Remove a block device (for hot-unplug, after device is removed)
qmpBlockdevDel
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- ^ Node name (same as used in qmpBlockdevAdd)
  -> IO QmpResult
qmpBlockdevDel config vmId nodeName =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "blockdev-del",
        "arguments": {
          "node-name": #{nodeName}
        }
      }
    |]

--------------------------------------------------------------------------------
-- Live (online) qcow2 internal snapshots
--
-- 'blockdev-snapshot-internal-sync' is the online equivalent of
-- @qemu-img snapshot -c@: writes a snapshot record into the
-- qcow2 metadata while the VM is running. QEMU pauses I/O on
-- the target blockdev for the duration of the metadata write,
-- flushing the writeback cache first so the snapshot captures
-- a consistent disk view.
--
-- The 'device' argument is a BlockBackend / device name.
-- Corvus's old-style @-drive@ command line does NOT pass an
-- explicit @id=@, so the BB name is auto-generated by QEMU from
-- the @if=@ + slot index. The reliable way to discover it is
-- 'qmpFindBlockDeviceByPath' (queries @query-block@ and matches
-- by file path).
--
-- There is NO online equivalent of @qemu-img snapshot -a@
-- (rollback) — the daemon's autoStop path stops the VM, applies
-- the offline rollback, then restarts.
--------------------------------------------------------------------------------

-- | Issue @blockdev-snapshot-internal-sync@ to stamp a named
-- snapshot into the qcow2 metadata for the running VM's named
-- block device.
qmpBlockSnapshotCreate
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- ^ device name (BlockBackend name from query-block)
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

-- | Look up the QEMU BlockBackend name for a given absolute file
-- path on a running VM. @query-block@ returns one entry per BB
-- with @{ device, inserted: { file, ... }, ... }@; we walk the
-- list and match @inserted.file@ exactly.
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
  pure $ do
    bs <- raw
    line <- extractReplyLine bs
    case A.eitherDecodeStrict line of
      Left e -> Left (T.pack ("query-block decode: " <> e))
      Right (QueryBlockReply rows) ->
        case filter ((== T.pack path) . qbiFilename) rows of
          (m : _) -> Right (qbiDevice m)
          [] ->
            Left $
              "no block device attached to VM "
                <> T.pack (show vmId)
                <> " has file="
                <> T.pack path

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
-- Low-level QMP Communication
--------------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Stats sampling

-- | One row of @query-blockstats@. Names match the QMP wire field
-- names for traceability: @bsrRdBytes@ / @bsrWrBytes@ are cumulative
-- since QEMU launch.
data BlockstatsRow = BlockstatsRow
  { bsrDevice :: !Text
  , bsrRdBytes :: !Word64
  , bsrWrBytes :: !Word64
  , bsrRdOps :: !Word64
  , bsrWrOps :: !Word64
  }
  deriving (Eq, Show)

-- | Issue @query-blockstats@ and decode the per-device counters.
-- Used by the agent's StatusPoller to populate @VmStats.drives@.
qmpQueryBlockstats :: QemuConfig -> Int64 -> IO (Either Text [BlockstatsRow])
qmpQueryBlockstats config vmId = do
  raw <- sendQmpRaw config vmId [qmpQQ| { "execute": "query-blockstats" } |]
  pure $ do
    bs <- raw
    line <- extractReplyLine bs
    case A.eitherDecodeStrict line of
      Left e -> Left (T.pack ("query-blockstats decode: " <> e))
      Right (BlockstatsReply rows) -> Right rows

-- | Issue @query-balloon@. Returns @Right (Just bytes)@ when the
-- VM has an active balloon device, @Right Nothing@ when the VM
-- has no balloon (QEMU error @DeviceNotActive@ / @CommandNotFound@),
-- or @Left@ for transport / decode failures.
qmpQueryBalloon :: QemuConfig -> Int64 -> IO (Either Text (Maybe Word64))
qmpQueryBalloon config vmId = do
  raw <- sendQmpRaw config vmId [qmpQQ| { "execute": "query-balloon" } |]
  pure $ do
    bs <- raw
    line <- extractReplyLine bs
    if BSWide.isInfixOf "\"error\"" line
      then Right Nothing
      else case A.eitherDecodeStrict line of
        Left e -> Left (T.pack ("query-balloon decode: " <> e))
        Right (BalloonReply actual) -> Right (Just actual)

-- | Raw QMP send that returns the response bytes without
-- 'classifyQmpResponse' coercion. Used by commands whose reply
-- carries structured data (counters, lists, …).
sendQmpRaw
  :: QemuConfig -> Int64 -> BS.ByteString -> IO (Either Text BS.ByteString)
sendQmpRaw config vmId cmd = do
  qmpSock <- getQmpSocket config vmId
  result <- try $ withUnixSocket qmpSock $ \sock -> do
    _ <- recv sock 4096
    sendAll sock [qmpQQ| { "execute": "qmp_capabilities" } |]
    _ <- drainUntilReply sock BS.empty
    sendAll sock cmd
    drainUntilReply sock BS.empty
  pure $ case result of
    Left (e :: SomeException) -> Left (T.pack (show e))
    Right response -> Right response

-- | Pick the line that carries @"return"@ or @"error"@ out of the
-- QMP response buffer. QMP may interleave async events between
-- the qmp_capabilities reply and our command's reply; the last
-- reply line is the one we want.
extractReplyLine :: BS.ByteString -> Either Text BS.ByteString
extractReplyLine bs =
  case reverse (filter isReply (BS.lines bs)) of
    (x : _) -> Right x
    [] -> Left "no QMP reply line in response"
  where
    isReply line =
      BSWide.isInfixOf "\"return\"" line
        || BSWide.isInfixOf "\"error\"" line

newtype BlockstatsReply = BlockstatsReply [BlockstatsRow]

instance A.FromJSON BlockstatsReply where
  parseJSON = A.withObject "BlockstatsReply" $ \o ->
    BlockstatsReply <$> o A..: "return"

instance A.FromJSON BlockstatsRow where
  parseJSON = A.withObject "BlockstatsRow" $ \o -> do
    dev <- o A..: "device"
    stats <- o A..: "stats"
    BlockstatsRow dev
      <$> stats A..: "rd_bytes"
      <*> stats A..: "wr_bytes"
      <*> stats A..: "rd_operations"
      <*> stats A..: "wr_operations"

newtype BalloonReply = BalloonReply Word64

instance A.FromJSON BalloonReply where
  parseJSON = A.withObject "BalloonReply" $ \o -> do
    ret <- o A..: "return"
    BalloonReply <$> ret A..: "actual"

-- ---------------------------------------------------------------------------

-- | Send a QMP command to a VM
sendQmpCommand :: QemuConfig -> Int64 -> BS.ByteString -> IO QmpResult
sendQmpCommand config vmId cmd = do
  qmpSock <- getQmpSocket config vmId
  result <- try $ withUnixSocket qmpSock $ \sock -> do
    -- Read QMP greeting
    _ <- recv sock 4096
    -- Send qmp_capabilities to enter command mode
    sendAll sock [qmpQQ| { "execute": "qmp_capabilities" } |]
    _ <- drainUntilReply sock BS.empty
    -- Send the actual command
    sendAll sock cmd
    drainUntilReply sock BS.empty
  pure $ case result of
    Left (e :: SomeException) -> QmpConnectionFailed $ T.pack $ show e
    Right response -> classifyQmpResponse response

-- | Read from the QMP socket until the accumulated buffer contains a
-- COMPLETE command reply (either @"return"@ or @"error"@). Async
-- events get folded into the accumulator and stay there for the
-- classifier to ignore.
--
-- A single 'recv' call is not robust: QEMU emits state-change events
-- (e.g. @RESUME@ after @cont@, @STOP@ after @stop@) BEFORE the
-- command's @{"return": {}}@, and the kernel may surface them in
-- separate reads — so a naive @recv 4096@ can return only the event
-- with no @"return"@ substring, leaving 'classifyQmpResponse' to
-- wrongly decide the command failed. Reading until we actually see
-- a reply payload fixes it.
--
-- *Substring on @"return"@ is also not enough on its own.* A large
-- reply (e.g. @query-blockstats@ on a multi-drive VM, which is
-- ~8 KiB on a single line) arrives in multiple 4 KiB chunks; the
-- first chunk contains @"return":@ at the start but the JSON line
-- isn't complete yet. Returning early hands @sendQmpRaw@ a
-- truncated payload that aeson then fails to decode. So we
-- additionally require the buffer to end with @'\\n'@ — QMP
-- terminates every message with a newline, so a trailing newline
-- past the @"return"@ substring means the line is whole.
drainUntilReply :: Socket -> BS.ByteString -> IO BS.ByteString
drainUntilReply sock acc = do
  chunk <- recv sock 4096
  if BS.null chunk
    then pure acc
    else do
      let combined = acc <> chunk
          hasReply =
            BS.isInfixOf "\"return\"" combined
              || BS.isInfixOf "\"error\"" combined
          messageComplete = case BS.unsnoc combined of
            Just (_, '\n') -> True
            _ -> False
      if hasReply && messageComplete
        then pure combined
        else drainUntilReply sock combined

-- | Classify a raw QMP response payload as success or error.
--
-- QEMU QMP sends either @{"return": ...}@ for a successful command or
-- @{"error": {"class": ..., "desc": ...}}@ on failure. We detect success
-- by substring match on @\"return\"@ rather than parsing the JSON — the
-- QMP wire format guarantees the key appears literally, and the keys we
-- send ourselves don't contain the literal @\"return\"@ substring.
-- Exposed for unit tests; real callers go through 'sendQmpCommand'.
classifyQmpResponse :: BS.ByteString -> QmpResult
classifyQmpResponse response
  | BS.isInfixOf "\"return\"" response = QmpSuccess
  | otherwise = QmpError $ T.pack $ BS.unpack response

-- | Connect to a Unix socket and run an action.
-- Retries on EAGAIN (resource temporarily unavailable), which occurs when
-- QEMU's chardev listen backlog (1) is full under heavy parallel load.
withUnixSocket :: FilePath -> (Socket -> IO a) -> IO a
withUnixSocket path =
  bracket (connectWithRetry 10) close
  where
    connectWithRetry :: Int -> IO Socket
    connectWithRetry 0 = do
      sock <- socket AF_UNIX Stream defaultProtocol
      connect sock (SockAddrUnix path)
      pure sock
    connectWithRetry n = do
      sock <- socket AF_UNIX Stream defaultProtocol
      (connect sock (SockAddrUnix path) >> pure sock)
        `catch` \(e :: IOException) ->
          if ioeGetErrorType e == ResourceExhausted
            then do
              close sock
              threadDelay 300000 -- 300ms
              connectWithRetry (n - 1)
            else do
              close sock
              ioError e
