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

    -- * Hot-plug commands
  , qmpBlockdevAdd
  , qmpDeviceAddDrive
  , qmpDeviceDel
  , qmpBlockdevDel

    -- * Low-level
  , sendQmpCommand
  , withUnixSocket

    -- * Re-export quasi-quoter
  , qmpQQ
  )
where

import Control.Exception (SomeException, bracket, try)
import Corvus.Model (DriveFormat (..), DriveInterface (..), EnumText (..))
import Corvus.Qemu.QmpQQ (qmpQQ)
import Corvus.Qemu.Runtime (getQmpSocket)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..), close, connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv, sendAll)

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
qmpShutdown :: Int64 -> IO QmpResult
qmpShutdown vmId =
  sendQmpCommand vmId [qmpQQ| { "execute": "system_powerdown" } |]

-- | Send continue command via QMP (resume paused VM)
qmpContinue :: Int64 -> IO QmpResult
qmpContinue vmId =
  sendQmpCommand vmId [qmpQQ| { "execute": "cont" } |]

-- | Send stop command via QMP (pause VM)
qmpStop :: Int64 -> IO QmpResult
qmpStop vmId =
  sendQmpCommand vmId [qmpQQ| { "execute": "stop" } |]

--------------------------------------------------------------------------------
-- Hot-plug Commands
--------------------------------------------------------------------------------

-- | Add a block device (for hot-plug)
qmpBlockdevAdd
  :: Int64
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
qmpBlockdevAdd vmId nodeName filePath format readOnly = do
  let formatStr = enumToText format
      filePathText = T.pack filePath
      readOnlyStr = if readOnly then "true" :: Text else "false" :: Text
      cmd =
        [qmpQQ|
          {
            "execute": "blockdev-add",
            "arguments": {
              "driver": #{formatStr},
              "node-name": #{nodeName},
              "read-only": #{readOnlyStr},
              "file": {
                "driver": "file",
                "filename": #{filePathText},
                "read-only": #{readOnlyStr}
              }
            }
          }
        |]
  sendQmpCommand vmId cmd

-- | Add a device using a block device (for hot-plug)
qmpDeviceAddDrive
  :: Int64
  -- ^ VM ID
  -> Text
  -- ^ Device ID (unique identifier for this device)
  -> Text
  -- ^ Block device node name (from qmpBlockdevAdd)
  -> DriveInterface
  -- ^ Drive interface type
  -> IO QmpResult
qmpDeviceAddDrive vmId deviceId nodeName iface = do
  let driver = T.pack $ interfaceToDriver iface
      cmd =
        [qmpQQ|
          {
            "execute": "device_add",
            "arguments": {
              "driver": #{driver},
              "id": #{deviceId},
              "drive": #{nodeName}
            }
          }
        |]
  sendQmpCommand vmId cmd

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
  :: Int64
  -- ^ VM ID
  -> Text
  -- ^ Device ID (same as used in qmpDeviceAddDrive)
  -> IO QmpResult
qmpDeviceDel vmId deviceId =
  sendQmpCommand
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
  :: Int64
  -- ^ VM ID
  -> Text
  -- ^ Node name (same as used in qmpBlockdevAdd)
  -> IO QmpResult
qmpBlockdevDel vmId nodeName =
  sendQmpCommand
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
sendQmpCommand :: Int64 -> BS.ByteString -> IO QmpResult
sendQmpCommand vmId cmd = do
  qmpSock <- getQmpSocket vmId
  result <- try $ withUnixSocket qmpSock $ \sock -> do
    -- Read QMP greeting
    _ <- recv sock 4096
    -- Send qmp_capabilities to enter command mode
    sendAll sock [qmpQQ| { "execute": "qmp_capabilities" } |]
    _ <- recv sock 4096
    -- Send the actual command
    sendAll sock cmd
    recv sock 4096
  case result of
    Left (e :: SomeException) -> pure $ QmpConnectionFailed $ T.pack $ show e
    Right response ->
      if BS.isInfixOf "\"return\"" response
        then pure QmpSuccess
        else pure $ QmpError $ T.pack $ BS.unpack response

-- | Connect to a Unix socket and run an action
withUnixSocket :: FilePath -> (Socket -> IO a) -> IO a
withUnixSocket path =
  bracket
    ( do
        sock <- socket AF_UNIX Stream defaultProtocol
        connect sock (SockAddrUnix path)
        pure sock
    )
    close
