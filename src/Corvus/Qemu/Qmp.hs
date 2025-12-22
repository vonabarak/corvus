{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | QMP (QEMU Machine Protocol) interaction.
-- Provides functions to control running VMs via QMP.
module Corvus.Qemu.Qmp
  ( -- * Types
    QmpResult (..),

    -- * Commands
    qmpShutdown,
    qmpContinue,
    qmpStop,

    -- * Low-level
    sendQmpCommand,
    withUnixSocket,
  )
where

import Control.Exception (SomeException, bracket, try)
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
qmpShutdown vmId = sendQmpCommand vmId "{\"execute\": \"system_powerdown\"}\n"

-- | Send continue command via QMP (resume paused VM)
qmpContinue :: Int64 -> IO QmpResult
qmpContinue vmId = sendQmpCommand vmId "{\"execute\": \"cont\"}\n"

-- | Send stop command via QMP (pause VM)
qmpStop :: Int64 -> IO QmpResult
qmpStop vmId = sendQmpCommand vmId "{\"execute\": \"stop\"}\n"

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
    sendAll sock "{\"execute\": \"qmp_capabilities\"}\n"
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
