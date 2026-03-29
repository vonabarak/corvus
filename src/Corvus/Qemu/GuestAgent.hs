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
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Qemu.Qmp (withUnixSocket)
import Corvus.Qemu.Runtime (getGuestAgentSocket)
import Data.Aeson (Value (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import System.Random (randomIO)
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
guestExec :: QemuConfig -> Int64 -> Text -> IO GuestExecResult
guestExec config vmId command = do
  gaSock <- getGuestAgentSocket config vmId
  mResult <- withSyncedSocket gaSock $ \sock -> do
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
      Nothing -> pure $ GuestExecError $ "Failed to parse guest-exec response: " <> T.pack (show execResp)
      Just pid -> pollStatus sock pid 0
  case mResult of
    Left err -> pure $ GuestExecConnectionFailed err
    Right r -> pure r

-- | Ping the guest agent to check if it's available.
guestPing :: QemuConfig -> Int64 -> IO Bool
guestPing config vmId = do
  gaSock <- getGuestAgentSocket config vmId
  mResult <- withSyncedSocket gaSock $ \sock -> do
    sendJson sock $ Aeson.object ["execute" .= ("guest-ping" :: Text)]
    resp <- recvJson sock
    case resp of
      Just (Object obj) -> pure $ KM.member "return" obj
      _ -> pure False
  case mResult of
    Left _ -> pure False
    Right r -> pure r

-- | Request a graceful shutdown via the guest agent.
-- This triggers a clean shutdown from inside the guest (like running "poweroff").
-- Returns True if the command was accepted, False on error.
guestShutdown :: QemuConfig -> Int64 -> IO Bool
guestShutdown config vmId = do
  gaSock <- getGuestAgentSocket config vmId
  mResult <- withSyncedSocket gaSock $ \sock -> do
    -- guest-shutdown with mode "powerdown" triggers a clean OS shutdown
    sendJson sock $
      Aeson.object
        [ "execute" .= ("guest-shutdown" :: Text)
        , "arguments" .= Aeson.object ["mode" .= ("powerdown" :: Text)]
        ]
    -- guest-shutdown doesn't return a response on success (the agent shuts down),
    -- so a timeout here is expected and means success.
    -- If it does respond, it's an error.
    resp <- timeout 3000000 $ recvJson sock
    case resp of
      Just (Just (Object obj)) -> pure $ not $ KM.member "error" obj
      _ -> pure True
  case mResult of
    Left _ -> pure True
    Right r -> pure r

-- | Query network interfaces from the guest via the QEMU Guest Agent.
-- Returns Nothing on failure, Just [] if no interfaces reported.
guestNetworkGetInterfaces :: QemuConfig -> Int64 -> IO (Maybe [GuestNetIf])
guestNetworkGetInterfaces config vmId = do
  gaSock <- getGuestAgentSocket config vmId
  mResult <- withSyncedSocket gaSock $ \sock -> do
    sendJson sock $ Aeson.object ["execute" .= ("guest-network-get-interfaces" :: Text)]
    resp <- recvJson sock
    pure $ parseGuestInterfaces resp
  case mResult of
    Left _ -> pure Nothing
    Right r -> pure r

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

-- | Connect to the guest agent socket, perform guest-sync handshake, and run
-- an action. Retries up to 5 times with 1-second backoff if connect or sync
-- fails (handles transient unavailability from socket contention with the
-- guest agent poller, or cloud-init restarting the agent).
withSyncedSocket :: FilePath -> (Socket -> IO a) -> IO (Either Text a)
withSyncedSocket path action = go 5
  where
    go 0 = pure $ Left "Guest agent unavailable after retries"
    go n = do
      mResult <- timeout 10000000 $ try $ withUnixSocket path $ \sock -> do
        syncGuest sock
        action sock
      case mResult of
        Just (Right a) -> pure $ Right a
        _ | n > 1 -> do
          threadDelay 1000000
          go (n - 1)
        Just (Left (e :: SomeException)) ->
          pure $ Left $ "Connection failed: " <> T.pack (show e)
        Nothing ->
          pure $ Left "Timed out waiting for guest agent"

-- | Synchronize with the guest agent.
-- Each new connection requires a guest-sync handshake before commands will work.
-- Drains any stale responses until the sync response (matching ID) arrives.
syncGuest :: Socket -> IO ()
syncGuest sock = do
  syncId <- abs <$> randomIO :: IO Int
  let syncCmd =
        Aeson.object
          [ "execute" .= ("guest-sync" :: Text)
          , "arguments" .= Aeson.object ["id" .= syncId]
          ]
  sendJson sock syncCmd
  waitForSync syncId 10
  where
    waitForSync _ 0 = pure ()
    waitForSync expected n = do
      resp <- recvJson sock
      case resp of
        Just (Object obj)
          | KM.lookup "return" obj == Just (Number (fromIntegral expected)) -> pure ()
        _ -> waitForSync expected (n - 1 :: Int)

-- | Send a JSON value over the socket
sendJson :: Socket -> Value -> IO ()
sendJson sock val = sendAll sock (BL.toStrict (Aeson.encode val) <> "\n")

-- | Read a JSON response from the socket.
-- QGA sends newline-delimited JSON. Read a chunk and parse the first JSON value.
recvJson :: Socket -> IO (Maybe Value)
recvJson sock = do
  bs <- recv sock 65536
  if BS.null bs
    then pure Nothing
    else pure $ Aeson.decodeStrict bs

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
