{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server for handling client connections over TCP or Unix sockets.
-- This module handles network communication, message framing,
-- and connection lifecycle. Business logic is delegated to Corvus.Handlers.
module Corvus.Server
  ( runServer
  , handleStartup
  , handleGracefulShutdown
  )
where

import Control.Concurrent (forkFinally)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVarIO, writeTVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.Action (runAction)
import Corvus.Handlers (handleRequest)
import Corvus.Handlers.Lifecycle (GracefulShutdown (..), Startup (..))
import Corvus.Handlers.Resolve (resolveVm)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu.SocketBuffer (relayClient)
import Corvus.Types
import Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (for_)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Word (Word8)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Network.Simple.TCP (HostPreference (..), serve)
import Network.Socket
  ( Family (AF_UNIX)
  , SockAddr (..)
  , Socket
  , SocketType (Stream)
  , accept
  , bind
  , close
  , listen
  , maxListenQueue
  , socket
  )
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removeFile)
import System.IO.Error (catchIOError)
import System.Posix.Process (ProcessStatus, getProcessStatus)
import System.Posix.Signals (sigTERM, signalProcess)

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

-- | Run the server with logging (TCP or Unix socket)
runServer :: ServerState -> ListenAddress -> IO ()
runServer state addr = runServerLogging state $ case addr of
  TcpAddress host port -> do
    logInfoN $ "Server starting on " <> T.pack host <> ":" <> T.pack (show port)
    let hostPref = Host host
    liftIO $ serve hostPref (show port) $ \(sock, sockAddr) ->
      runServerLogging state $ handleConnection state sock sockAddr
  UnixAddress path -> do
    logInfoN $ "Server starting on Unix socket: " <> T.pack path
    liftIO $ runUnixServer state path

-- | Run server on a Unix socket
runUnixServer :: ServerState -> FilePath -> IO ()
runUnixServer state path = do
  -- Remove existing socket file if present
  removeFile path `catchIOError` const (pure ())
  bracket
    (socket AF_UNIX Stream 0)
    ( \sock -> do
        close sock
        removeFile path `catchIOError` const (pure ())
    )
    $ \sock -> do
      bind sock (SockAddrUnix path)
      listen sock maxListenQueue
      forever $ do
        (clientSock, clientAddr) <- accept sock
        void $
          forkFinally
            ( runServerLogging state $
                handleConnection state clientSock clientAddr
            )
            (\_ -> close clientSock)

--------------------------------------------------------------------------------
-- Connection Handling
--------------------------------------------------------------------------------

-- | Handle a client connection with logging
handleConnection :: ServerState -> Socket -> SockAddr -> LoggingT IO ()
handleConnection state sock addr = do
  liftIO $ atomically $ modifyTVar' (ssConnectionCount state) (+ 1)
  handleClient state sock
    `finally` do
      liftIO $ atomically $ modifyTVar' (ssConnectionCount state) (subtract 1)

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
          respResult <- liftIO $ try $ handleRequest state req
          let resp = case respResult of
                Right r -> r
                Left e ->
                  RespError $
                    "Internal error: " <> T.pack (show (e :: SomeException))
          logDebugN $ "Response: " <> formatResponse resp
          liftIO $ sendResponse sock resp
          -- Continue unless shutdown or protocol upgrade
          case (req, resp) of
            (ReqSerialConsole ref, RespSerialConsoleOk) -> do
              logInfoN "Entering serial console relay mode"
              liftIO $ bufferRelay (ssSerialBuffers state) state ref sock
            (ReqHmpMonitor ref, RespHmpMonitorOk) -> do
              logInfoN "Entering HMP monitor relay mode"
              liftIO $ bufferRelay (ssMonitorBuffers state) state ref sock
            (ReqShutdown, _) -> logInfoN "Shutdown requested"
            _ -> loop

--------------------------------------------------------------------------------
-- Message I/O
--------------------------------------------------------------------------------

-- | Receive a request from the socket
-- Returns Nothing if connection closed, Left on error, Right on success
receiveRequest :: Socket -> LoggingT IO (Maybe (Either Text Request))
receiveRequest sock = do
  -- Read version byte (1 byte)
  verBs <- liftIO $ recvExact sock 1
  case verBs of
    Nothing -> pure Nothing
    Just verBytes -> do
      case decodeOrFail (BL.fromStrict verBytes) of
        Left _ -> pure $ Just $ Left "Invalid version byte"
        Right (_, _, ver)
          | (ver :: Word8) /= protocolVersion ->
              pure $
                Just $
                  Left $
                    "Protocol version mismatch: expected "
                      <> T.pack (show protocolVersion)
                      <> ", got "
                      <> T.pack (show ver)
          | otherwise -> do
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
      verBytes = BL.toStrict $ encode protocolVersion
      lenBytes = BL.toStrict $ encode len
  sendAll sock (verBytes <> lenBytes <> payload)

--------------------------------------------------------------------------------
-- Chardev Buffer Relay
--------------------------------------------------------------------------------

-- | Look up the per-VM 'SocketBufferHandle' in the given map and
-- hand the client socket off to 'relayClient'. Silently no-ops if the
-- VM can't be resolved or no buffer thread is registered; the caller
-- has already sent the @Ok@ response, so the only failure mode here
-- is a race with VM shutdown.
bufferRelay
  :: TVar (Map.Map Int64 SocketBufferHandle)
  -> ServerState
  -> Ref
  -> Socket
  -> IO ()
bufferRelay bufferMapVar state ref clientSock = do
  mVmId <- resolveVm ref (ssDbPool state)
  case mVmId of
    Left _ -> pure ()
    Right vmId -> do
      buffers <- readTVarIO bufferMapVar
      for_ (Map.lookup vmId buffers) (relayClient clientSock)

--------------------------------------------------------------------------------
-- Logging Helpers
--------------------------------------------------------------------------------

-- | Format response for logging (abbreviated for large responses)
formatResponse :: Response -> Text
formatResponse resp = case resp of
  RespVmList vms -> "RespVmList [" <> T.pack (show (length vms)) <> " VMs]"
  RespVmDetails d -> "RespVmDetails { id=" <> T.pack (show (vdId d)) <> ", name=" <> vdName d <> " }"
  other -> T.pack (show other)

--------------------------------------------------------------------------------
-- Startup Handler
--------------------------------------------------------------------------------

-- | Run startup tasks: clean stale state, start namespace, autostart networks and VMs.
-- Delegates to the Startup action which records itself as a task with subtasks.
handleStartup :: ServerState -> Int -> IO ()
handleStartup state retentionDays = void $ runAction state (Startup retentionDays)

--------------------------------------------------------------------------------
-- Graceful Shutdown Handler
--------------------------------------------------------------------------------

-- | Gracefully shut down all running VMs and networks.
-- Delegates to the GracefulShutdown action which records itself as a task.
handleGracefulShutdown :: ServerState -> IO ()
handleGracefulShutdown state = void $ runAction state GracefulShutdown
