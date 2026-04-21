{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | QMP (QEMU Machine Protocol) interaction.
-- Provides functions to control running VMs via QMP.
module Corvus.Qemu.Qmp
  ( -- * Types
    QmpResult (..)

    -- * Commands
  , qmpShutdown
  , qmpContinue
  , qmpStop

    -- * SPICE ticket commands
  , qmpSetSpicePassword
  , qmpExpireSpicePassword

    -- * Input injection
  , qmpSendCtrlAltDel

    -- * Hot-plug commands
  , qmpBlockdevAdd
  , qmpDeviceAddDrive
  , qmpDeviceDel
  , qmpBlockdevDel

    -- * Low-level
  , sendQmpCommand
  , classifyQmpResponse
  , withUnixSocket

    -- * Re-export quasi-quoter
  , qmpQQ
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, bracket, catch, try)
import Corvus.Model (DriveFormat (..), DriveInterface (..), EnumText (..))
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Qemu.QmpQQ (qmpQQ)
import Corvus.Qemu.Runtime (getQmpSocket)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
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
-- Low-level QMP Communication
--------------------------------------------------------------------------------

-- | Send a QMP command to a VM
sendQmpCommand :: QemuConfig -> Int64 -> BS.ByteString -> IO QmpResult
sendQmpCommand config vmId cmd = do
  qmpSock <- getQmpSocket config vmId
  result <- try $ withUnixSocket qmpSock $ \sock -> do
    -- Read QMP greeting
    _ <- recv sock 4096
    -- Send qmp_capabilities to enter command mode
    sendAll sock [qmpQQ| { "execute": "qmp_capabilities" } |]
    _ <- recv sock 4096
    -- Send the actual command
    sendAll sock cmd
    recv sock 4096
  pure $ case result of
    Left (e :: SomeException) -> QmpConnectionFailed $ T.pack $ show e
    Right response -> classifyQmpResponse response

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
