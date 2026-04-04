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
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO, writeTVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.Handlers (handleRequest)
import Corvus.Handlers.Subtask (SubtaskSpec (..), withOptionalSubtask)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu.Netns (startNamespace)
import Corvus.Qemu.Netns.Manager (destroyBridge, enableIpForwarding, setupNatTable, startPasta, stopDnsmasq, stopPasta, teardownNatTable)
import Corvus.Qemu.Process (killVmProcess)
import Corvus.Qemu.Virtiofsd (killVirtiofsdProcesses)
import Corvus.Types
import Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
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

-- | Run startup tasks: clean stale state, kill orphan processes.
-- Records itself as a task in the DB.
-- Should be called after migrations and before accepting connections.
handleStartup :: ServerState -> Int -> IO ()
handleStartup state retentionDays = do
  let pool = ssDbPool state
  now <- getCurrentTime

  -- Record startup task
  taskKey <-
    runSqlPool
      ( insert
          Task
            { taskParent = Nothing
            , taskStartedAt = now
            , taskFinishedAt = Nothing
            , taskSubsystem = SubSystem
            , taskEntityId = Nothing
            , taskEntityName = Nothing
            , taskCommand = "startup"
            , taskResult = TaskRunning
            , taskMessage = Nothing
            }
      )
      pool

  runServerLogging state $ do
    -- Mark stale "running" and "not_started" tasks as error (daemon crashed while processing)
    liftIO $ runSqlPool (updateWhere [TaskResult <-. [TaskRunning, TaskNotStarted], TaskId !=. taskKey] [TaskResult =. TaskError, TaskMessage =. Just "Daemon restarted"]) pool
    logInfoN "Marked stale running tasks as error"

    -- Kill orphaned QEMU processes and reset stale VMs
    staleVms <- liftIO $ runSqlPool (selectList [M.VmStatus <-. [VmStarting, VmRunning, VmStopping, VmPaused]] []) pool
    liftIO $ forM_ staleVms $ \(Entity vmKey vm) -> do
      case vmPid vm of
        Just pid -> runServerLogging state $ do
          logInfoN $ "Killing orphaned QEMU process for VM " <> vmName vm <> " (PID " <> T.pack (show pid) <> ")"
          _ <- killVmProcess (fromSqlKey vmKey) pid
          -- Kill orphaned virtiofsd processes
          killVirtiofsdProcesses pool (fromSqlKey vmKey)
        Nothing -> pure ()
    liftIO $ runSqlPool (updateWhere [M.VmStatus <-. [VmStarting, VmRunning, VmStopping, VmPaused]] [M.VmStatus =. VmError, M.VmPid =. Nothing, M.VmHealthcheck =. Nothing]) pool
    logInfoN "Reset stale VMs to error state"

    -- Kill orphaned dnsmasq processes, reset network state
    staleNetworks <- liftIO $ runSqlPool (selectList ([M.NetworkRunning ==. True] ||. [M.NetworkDnsmasqPid !=. Nothing]) []) pool
    liftIO $ forM_ staleNetworks $ \(Entity _nwKey nw) ->
      forM_ (networkDnsmasqPid nw) stopDnsmasq
    liftIO $ runSqlPool (updateWhere [M.NetworkRunning ==. True] [M.NetworkRunning =. False, M.NetworkDnsmasqPid =. Nothing]) pool
    logInfoN "Reset stale network state"

    -- Start the global network namespace (subtask)
    logInfoN "Starting network namespace"
    nsResult <- liftIO $ do
      let spec = SubtaskSpec SubSystem "start-namespace" Nothing
      withOptionalSubtask
        pool
        (Just taskKey)
        spec
        (fmap (fmap fromIntegral) startNamespace)
        (Just . fromIntegral)
    case nsResult of
      Left err ->
        logWarnN $ "Failed to start network namespace: " <> err
      Right nsPidInt -> do
        liftIO $ atomically $ writeTVar (ssNamespacePid state) (Just nsPidInt)
        logInfoN $ "Network namespace started (PID " <> T.pack (show nsPidInt) <> ")"

        -- Start pasta for NAT support (subtask)
        pastaResult <- liftIO $ do
          let spec = SubtaskSpec SubSystem "start-pasta" Nothing
          withOptionalSubtask
            pool
            (Just taskKey)
            spec
            (startPasta nsPidInt (ssQemuConfig state))
            (Just . fromIntegral)
        case pastaResult of
          Left err ->
            logWarnN $ "Failed to start pasta (NAT unavailable): " <> err
          Right pastaPid -> do
            liftIO $ atomically $ writeTVar (ssPastaPid state) (Just pastaPid)
            logInfoN $ "pasta started for NAT (PID " <> T.pack (show pastaPid) <> ")"
            -- Enable IP forwarding and setup NAT table (subtask)
            _ <- liftIO $ do
              let spec = SubtaskSpec SubNetwork "setup-nat" Nothing
              withOptionalSubtask
                pool
                (Just taskKey)
                spec
                ( do
                    fwdResult <- enableIpForwarding nsPidInt
                    case fwdResult of
                      Left err -> pure $ Left $ "IP forwarding: " <> err
                      Right () -> setupNatTable nsPidInt (ssQemuConfig state)
                )
                (const Nothing)
            logInfoN "IP forwarding enabled, nftables NAT table created"

    -- Delete old task entries
    when (retentionDays > 0) $ do
      let cutoff = addUTCTime (fromIntegral $ negate $ retentionDays * 86400) now
      liftIO $ runSqlPool (deleteWhere [TaskStartedAt <. cutoff]) pool
      logInfoN $ "Cleaned up tasks older than " <> T.pack (show retentionDays) <> " days"

  -- Mark startup task as completed
  finishTime <- getCurrentTime
  runSqlPool
    ( update
        taskKey
        [ TaskFinishedAt =. Just finishTime
        , TaskResult =. TaskSuccess
        , TaskMessage =. Just "Startup complete"
        ]
    )
    pool

--------------------------------------------------------------------------------
-- Graceful Shutdown Handler
--------------------------------------------------------------------------------

-- | Gracefully shut down all running VMs and networks.
-- Records itself as a task in the DB.
-- Should be called after stopping new connection acceptance, before exit.
handleGracefulShutdown :: ServerState -> IO ()
handleGracefulShutdown state = do
  let pool = ssDbPool state
  now <- getCurrentTime

  -- Record shutdown task
  taskKey <-
    runSqlPool
      ( insert
          Task
            { taskParent = Nothing
            , taskStartedAt = now
            , taskFinishedAt = Nothing
            , taskSubsystem = SubSystem
            , taskEntityId = Nothing
            , taskEntityName = Nothing
            , taskCommand = "shutdown"
            , taskResult = TaskRunning
            , taskMessage = Nothing
            }
      )
      pool

  result <- try $ runServerLogging state $ do
    -- Stop all running/starting VMs
    runningVms <- liftIO $ runSqlPool (selectList [M.VmStatus <-. [VmStarting, VmRunning, VmPaused]] []) pool
    logInfoN $ "Stopping " <> T.pack (show (length runningVms)) <> " running VM(s)"
    liftIO $ forM_ runningVms $ \(Entity vmKey vm) -> do
      case vmPid vm of
        Just pid -> runServerLogging state $ do
          logInfoN $ "Killing VM " <> vmName vm <> " (PID " <> T.pack (show pid) <> ")"
          -- Clear PID first (so process monitor thread doesn't interfere)
          liftIO $ runSqlPool (update vmKey [M.VmPid =. Nothing]) pool
          _ <- killVmProcess (fromSqlKey vmKey) pid
          killVirtiofsdProcesses pool (fromSqlKey vmKey)
          liftIO $ runSqlPool (update vmKey [M.VmStatus =. VmStopped, M.VmHealthcheck =. Nothing]) pool
        Nothing -> pure ()
    -- Also handle VMs in Stopping state (QEMU still running)
    stoppingVms <- liftIO $ runSqlPool (selectList [M.VmStatus ==. VmStopping] []) pool
    liftIO $ forM_ stoppingVms $ \(Entity vmKey vm) -> do
      case vmPid vm of
        Just pid -> runServerLogging state $ do
          logInfoN $ "Force-killing stopping VM " <> vmName vm <> " (PID " <> T.pack (show pid) <> ")"
          liftIO $ runSqlPool (update vmKey [M.VmPid =. Nothing]) pool
          _ <- killVmProcess (fromSqlKey vmKey) pid
          killVirtiofsdProcesses pool (fromSqlKey vmKey)
          liftIO $ runSqlPool (update vmKey [M.VmStatus =. VmStopped, M.VmHealthcheck =. Nothing]) pool
        Nothing -> pure ()

    -- Stop all running networks (dnsmasq + bridges)
    runningNetworks <- liftIO $ runSqlPool (selectList [M.NetworkRunning ==. True] []) pool
    logInfoN $ "Stopping " <> T.pack (show (length runningNetworks)) <> " running network(s)"
    mNsPid <- liftIO $ readTVarIO (ssNamespacePid state)
    liftIO $ forM_ runningNetworks $ \(Entity nwKey nw) -> do
      forM_ (networkDnsmasqPid nw) stopDnsmasq
      case mNsPid of
        Just nsPid -> void $ destroyBridge nsPid (fromSqlKey nwKey)
        Nothing -> pure ()
      runSqlPool (update nwKey [M.NetworkRunning =. False, M.NetworkDnsmasqPid =. Nothing]) pool

    -- Teardown nftables NAT table
    case mNsPid of
      Just nsPid -> do
        _ <- liftIO $ teardownNatTable nsPid (ssQemuConfig state)
        pure ()
      Nothing -> pure ()

    -- Kill pasta if running
    mPastaPid <- liftIO $ readTVarIO (ssPastaPid state)
    case mPastaPid of
      Just pastaPid -> do
        logInfoN $ "Killing pasta (PID " <> T.pack (show pastaPid) <> ")"
        liftIO $ stopPasta pastaPid
        liftIO $ atomically $ writeTVar (ssPastaPid state) Nothing
      Nothing -> pure ()

    -- Kill the network namespace manager
    case mNsPid of
      Just nsPid -> do
        logInfoN $ "Killing namespace manager (PID " <> T.pack (show nsPid) <> ")"
        result' <- liftIO $ try $ signalProcess sigTERM (fromIntegral nsPid)
        case result' of
          Left (_ :: SomeException) -> pure ()
          Right () -> do
            -- Reap the child process to avoid zombie
            _ <- liftIO (try (getProcessStatus True False (fromIntegral nsPid)) :: IO (Either SomeException (Maybe ProcessStatus)))
            pure ()
        liftIO $ atomically $ writeTVar (ssNamespacePid state) Nothing
      Nothing -> pure ()

    -- Mark any remaining running tasks as error
    liftIO $ runSqlPool (updateWhere [TaskResult <-. [TaskRunning, TaskNotStarted], TaskId !=. taskKey] [TaskResult =. TaskError, TaskMessage =. Just "Daemon shutting down"]) pool
    logInfoN "Graceful shutdown complete"

  -- Mark shutdown task as completed
  finishTime <- getCurrentTime
  case result of
    Right () ->
      runSqlPool
        ( update
            taskKey
            [ TaskFinishedAt =. Just finishTime
            , TaskResult =. TaskSuccess
            , TaskMessage =. Just "Shutdown complete"
            ]
        )
        pool
    Left (err :: SomeException) ->
      runSqlPool
        ( update
            taskKey
            [ TaskFinishedAt =. Just finishTime
            , TaskResult =. TaskError
            , TaskMessage =. Just ("Shutdown error: " <> T.pack (show err))
            ]
        )
        pool
