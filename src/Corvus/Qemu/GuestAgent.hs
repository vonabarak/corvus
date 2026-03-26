{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | QEMU Guest Agent (QGA) interaction.
-- Provides functions to execute commands inside VMs via the guest agent.
module Corvus.Qemu.GuestAgent
  ( -- * Types
    GuestExecResult (..)
  , GuestIpAddress (..)
  , GuestNetIf (..)

    -- * Commands
  , guestExec
  , guestPing
  , guestShutdown
  , guestNetworkGetInterfaces
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Corvus.Qemu.Qmp (withUnixSocket)
import Corvus.Qemu.Runtime (getGuestAgentSocket)
import Data.Aeson (Value (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)

-- | Result of a guest-exec command
data GuestExecResult
  = -- | Command completed (exitcode, stdout, stderr)
    GuestExecSuccess !Int !Text !Text
  | -- | Error communicating with guest agent
    GuestExecError !Text
  | -- | Could not connect to guest agent socket
    GuestExecConnectionFailed !Text
  deriving (Eq, Show)

-- | A single IP address reported by the guest agent
data GuestIpAddress = GuestIpAddress
  { giaType :: !Text
  -- ^ "ipv4" or "ipv6"
  , giaAddress :: !Text
  -- ^ e.g. "10.0.0.5"
  , giaPrefix :: !Int
  -- ^ e.g. 24
  }
  deriving (Eq, Show)

-- | A network interface reported by the guest agent
data GuestNetIf = GuestNetIf
  { gniHardwareAddress :: !Text
  -- ^ MAC address, e.g. "52:54:00:xx:xx:xx"
  , gniIpAddresses :: ![GuestIpAddress]
  -- ^ All IP addresses on this interface
  }
  deriving (Eq, Show)

-- | Execute a command inside the guest via the QEMU Guest Agent.
-- Runs the command via /bin/sh -c, captures stdout and stderr.
guestExec :: Int64 -> Text -> IO GuestExecResult
guestExec vmId command = do
  gaSock <- getGuestAgentSocket vmId
  result <- try $ withUnixSocket gaSock $ \sock -> do
    -- Sync with the guest agent; 3s timeout so polling doesn't block
    mSync <- timeout 3000000 $ syncGuest sock
    case mSync of
      Nothing -> pure $ GuestExecConnectionFailed "Timed out waiting for guest-sync"
      Just () -> do
        -- Send guest-exec command
        let execCmd =
              Aeson.object
                [ "execute" .= ("guest-exec" :: Text)
                , "arguments"
                    .= Aeson.object
                      [ "path" .= ("/bin/sh" :: Text)
                      , "arg" .= ["-c" :: Text, command]
                      , "capture-output" .= True
                      ]
                ]
        sendJson sock execCmd
        execResp <- recvJson sock

        case parsePid execResp of
          Nothing -> pure $ GuestExecError "Failed to parse guest-exec response"
          Just pid -> pollStatus sock pid 0
  case result of
    Left (e :: SomeException) -> pure $ GuestExecConnectionFailed $ T.pack $ show e
    Right r -> pure r

-- | Ping the guest agent to check if it's available.
guestPing :: Int64 -> IO Bool
guestPing vmId = do
  gaSock <- getGuestAgentSocket vmId
  mResult <- timeout 3000000 $ try $ withUnixSocket gaSock $ \sock -> do
    syncGuest sock
    sendJson sock $ Aeson.object ["execute" .= ("guest-ping" :: Text)]
    resp <- recvJson sock
    case resp of
      Just (Object obj) -> pure $ KM.member "return" obj
      _ -> pure False
  case mResult of
    Nothing -> pure False
    Just (Left (_ :: SomeException)) -> pure False
    Just (Right r) -> pure r

-- | Request a graceful shutdown via the guest agent.
-- This triggers a clean shutdown from inside the guest (like running "poweroff").
-- Returns True if the command was accepted, False on error.
guestShutdown :: Int64 -> IO Bool
guestShutdown vmId = do
  gaSock <- getGuestAgentSocket vmId
  mResult <- timeout 5000000 $ try $ withUnixSocket gaSock $ \sock -> do
    syncGuest sock
    -- guest-shutdown with mode "powerdown" triggers a clean OS shutdown
    sendJson sock $
      Aeson.object
        [ "execute" .= ("guest-shutdown" :: Text)
        , "arguments" .= Aeson.object ["mode" .= ("powerdown" :: Text)]
        ]
    -- guest-shutdown doesn't return a response on success (the agent shuts down),
    -- so a timeout here is expected and means success.
    -- If it does respond, it's an error.
    resp <- recvJson sock
    case resp of
      Just (Object obj) -> pure $ not $ KM.member "error" obj
      _ -> pure True
  case mResult of
    -- Timeout is expected: the guest agent shuts down before responding
    Nothing -> pure True
    Just (Left (_ :: SomeException)) -> pure True
    Just (Right r) -> pure r

-- | Query network interfaces from the guest via the QEMU Guest Agent.
-- Returns Nothing on failure, Just [] if no interfaces reported.
guestNetworkGetInterfaces :: Int64 -> IO (Maybe [GuestNetIf])
guestNetworkGetInterfaces vmId = do
  gaSock <- getGuestAgentSocket vmId
  mResult <- timeout 3000000 $ try $ withUnixSocket gaSock $ \sock -> do
    syncGuest sock
    sendJson sock $ Aeson.object ["execute" .= ("guest-network-get-interfaces" :: Text)]
    resp <- recvJson sock
    pure $ parseGuestInterfaces resp
  case mResult of
    Nothing -> pure Nothing
    Just (Left (_ :: SomeException)) -> pure Nothing
    Just (Right r) -> pure r

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

-- | Synchronize with the guest agent.
-- Each new connection requires a guest-sync handshake before commands will work.
syncGuest :: Socket -> IO ()
syncGuest sock = do
  let syncCmd =
        Aeson.object
          [ "execute" .= ("guest-sync" :: Text)
          , "arguments" .= Aeson.object ["id" .= (1 :: Int)]
          ]
  sendJson sock syncCmd
  _ <- recvJson sock
  pure ()

-- | Send a JSON value over the socket
sendJson :: Socket -> Value -> IO ()
sendJson sock val = sendAll sock (BL.toStrict (Aeson.encode val) <> "\n")

-- | Read a JSON response from the socket
recvJson :: Socket -> IO (Maybe Value)
recvJson sock = do
  bs <- recv sock 65536
  pure $ Aeson.decodeStrict bs

-- | Extract PID from guest-exec response: {"return": {"pid": N}}
parsePid :: Maybe Value -> Maybe Int
parsePid mVal = do
  val <- mVal
  AT.parseMaybe pidParser val
  where
    pidParser = AT.withObject "response" $ \obj -> do
      ret <- obj .: "return"
      ret .: "pid"

-- | Poll guest-exec-status until the process exits.
-- Timeout after ~60 seconds (600 * 100ms).
pollStatus :: Socket -> Int -> Int -> IO GuestExecResult
pollStatus sock pid attempts
  | attempts > 600 = pure $ GuestExecError "guest-exec timed out waiting for process to exit"
  | otherwise = do
      let statusCmd =
            Aeson.object
              [ "execute" .= ("guest-exec-status" :: Text)
              , "arguments" .= Aeson.object ["pid" .= pid]
              ]
      sendJson sock statusCmd
      statusResp <- recvJson sock

      case parseExecStatus statusResp of
        Just (True, exitcode, stdout, stderr) ->
          pure $ GuestExecSuccess exitcode stdout stderr
        Just (False, _, _, _) -> do
          threadDelay 100000 -- 100ms
          pollStatus sock pid (attempts + 1)
        Nothing -> pure $ GuestExecError "Failed to parse guest-exec-status response"

-- | Parse guest-exec-status response.
-- Returns (exited, exitcode, stdout, stderr)
parseExecStatus :: Maybe Value -> Maybe (Bool, Int, Text, Text)
parseExecStatus mVal = do
  val <- mVal
  AT.parseMaybe statusParser val
  where
    statusParser = AT.withObject "response" $ \obj -> do
      ret <- obj .: "return"
      exited <- ret .: "exited"
      if exited
        then do
          exitcode <- ret .:? "exitcode" AT..!= 1
          outData <- ret .:? "out-data" AT..!= ""
          errData <- ret .:? "err-data" AT..!= ""
          pure (True, exitcode, decodeBase64 outData, decodeBase64 errData)
        else pure (False, 0, "", "")

-- | Parse the guest-network-get-interfaces response.
-- Expected format: {"return": [{"name": "eth0", "hardware-address": "...", "ip-addresses": [...]}]}
parseGuestInterfaces :: Maybe Value -> Maybe [GuestNetIf]
parseGuestInterfaces mVal = do
  val <- mVal
  AT.parseMaybe interfacesParser val
  where
    interfacesParser = AT.withObject "response" $ \obj -> do
      ret <- obj .: "return"
      mapM parseIface ret

    parseIface = AT.withObject "interface" $ \obj -> do
      hwAddr <- obj .: "hardware-address"
      ipAddrs <- obj .:? "ip-addresses" AT..!= []
      parsedIps <- mapM parseIpAddr ipAddrs
      pure GuestNetIf {gniHardwareAddress = hwAddr, gniIpAddresses = parsedIps}

    parseIpAddr = AT.withObject "ip-address" $ \obj -> do
      ipType <- obj .: "ip-address-type"
      ipAddr <- obj .: "ip-address"
      prefix <- obj .: "prefix"
      pure GuestIpAddress {giaType = ipType, giaAddress = ipAddr, giaPrefix = prefix}

-- | Decode a base64-encoded text field from QGA response
decodeBase64 :: Text -> Text
decodeBase64 t
  | T.null t = ""
  | otherwise = case B64.decode (encodeUtf8 t) of
      Right decoded -> decodeUtf8With lenientDecode decoded
      Left _ -> t
