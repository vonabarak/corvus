{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TCP server for handling client connections.
-- This module handles network communication, message framing,
-- and connection lifecycle. Business logic is delegated to Corvus.Handlers.
module Corvus.Server
  ( runServer,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Corvus.Handlers (handleRequest)
import Corvus.Protocol
import Corvus.Types
import Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Simple.TCP (HostPreference (..), Socket, serve)
import Network.Socket (SockAddr)
import Network.Socket.ByteString (recv, sendAll)

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

-- | Run the TCP server with logging
runServer :: ServerState -> String -> Int -> IO ()
runServer state host port = runStdoutLoggingT $ do
  logInfoN $ "Server starting on " <> T.pack host <> ":" <> T.pack (show port)
  let hostPref = Host host
  liftIO $ serve hostPref (show port) $ \(sock, addr) ->
    runStdoutLoggingT $ handleConnection state sock addr

--------------------------------------------------------------------------------
-- Connection Handling
--------------------------------------------------------------------------------

-- | Handle a client connection with logging
handleConnection :: ServerState -> Socket -> SockAddr -> LoggingT IO ()
handleConnection state sock addr = do
  logInfoN $ "Client connected: " <> T.pack (show addr)
  liftIO $ atomically $ modifyTVar' (ssConnectionCount state) (+ 1)
  handleClient state sock
    `finally` do
      liftIO $ atomically $ modifyTVar' (ssConnectionCount state) (subtract 1)
      logInfoN $ "Client disconnected: " <> T.pack (show addr)

-- | Handle a single client connection (request/response loop)
handleClient :: ServerState -> Socket -> LoggingT IO ()
handleClient state sock = loop
  where
    loop = do
      mReq <- receiveRequest sock
      case mReq of
        Nothing -> logDebugN "Connection closed by client"
        Just (Left err) -> do
          logWarnN $ "Receive error: " <> err
          let resp = RespError err
          logDebugN $ "Response: " <> T.pack (show resp)
          liftIO $ sendResponse sock resp
          loop
        Just (Right req) -> do
          logDebugN $ "Request: " <> T.pack (show req)
          resp <- liftIO $ handleRequest state req
          logDebugN $ "Response: " <> formatResponse resp
          liftIO $ sendResponse sock resp
          -- Continue unless shutdown
          case req of
            ReqShutdown -> logInfoN "Shutdown requested"
            _ -> loop

--------------------------------------------------------------------------------
-- Message I/O
--------------------------------------------------------------------------------

-- | Receive a request from the socket
-- Returns Nothing if connection closed, Left on error, Right on success
receiveRequest :: Socket -> LoggingT IO (Maybe (Either Text Request))
receiveRequest sock = do
  -- Read length prefix (8 bytes for Int64)
  lenBs <- liftIO $ recvExact sock 8
  case lenBs of
    Nothing -> pure Nothing
    Just lenBytes -> do
      case decodeOrFail (BL.fromStrict lenBytes) of
        Left _ -> pure $ Just $ Left "Invalid message length"
        Right (_, _, len) -> do
          -- Read payload
          payloadBs <- liftIO $ recvExact sock (fromIntegral (len :: Int64))
          case payloadBs of
            Nothing -> pure Nothing
            Just payload -> do
              case decodeOrFail (BL.fromStrict payload) of
                Left (_, _, err) -> pure $ Just $ Left $ "Decode error: " <> T.pack err
                Right (_, _, req) -> pure $ Just $ Right req

-- | Receive exactly n bytes from socket
recvExact :: Socket -> Int -> IO (Maybe BS.ByteString)
recvExact sock = go BS.empty
  where
    go acc 0 = pure (Just acc)
    go acc remaining = do
      chunk <- recv sock (min 4096 remaining)
      if BS.null chunk
        then pure Nothing -- Connection closed
        else go (acc <> chunk) (remaining - BS.length chunk)

-- | Send a response to the socket
sendResponse :: Socket -> Response -> IO ()
sendResponse sock resp = do
  let payload = BL.toStrict $ encode resp
      len = fromIntegral (BS.length payload) :: Int64
      lenBytes = BL.toStrict $ encode len
  sendAll sock (lenBytes <> payload)

--------------------------------------------------------------------------------
-- Logging Helpers
--------------------------------------------------------------------------------

-- | Format response for logging (abbreviated for large responses)
formatResponse :: Response -> Text
formatResponse resp = case resp of
  RespVmList vms -> "RespVmList [" <> T.pack (show (length vms)) <> " VMs]"
  RespVmDetails d -> "RespVmDetails { id=" <> T.pack (show (vdId d)) <> ", name=" <> vdName d <> " }"
  other -> T.pack (show other)
