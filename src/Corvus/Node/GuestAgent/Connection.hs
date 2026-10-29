{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Serialized, persistent per-VM QGA connections.
module Corvus.Node.GuestAgent.Connection
  ( GuestAgentConns (..)
  , newGuestAgentConns
  , withPersistentConn
  , withDiscardedConn
  , invalidateConn
  , releaseConn
  , syncGuest
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeAsyncException, SomeException, catch, finally, fromException, mask, mask_, onException, throwIO, try)
import Control.Monad.Logger (LogLevel (..))
import Corvus.Node.GuestAgent.Transport (QgaLogger, QgaSocket (..), closeQga, logQga, newQgaSocket, recvQga, sendQga)
import Corvus.Node.Runtime (getGuestAgentSocket)
import Corvus.Qemu.Config (QemuConfig)
import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO.Exception (IOErrorType (ResourceExhausted))
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), close, connect, defaultProtocol, socket)
import System.IO.Error (ioeGetErrorType)
import System.Random (randomRIO)
import System.Timeout (timeout)

data GuestAgentConns = GuestAgentConns
  { guestAgentSlots :: TVar (Map.Map Int64 (MVar (Maybe QgaSocket)))
  , guestAgentLogger :: QgaLogger
  , guestAgentNextId :: TVar Integer
  }

newGuestAgentConns :: QgaLogger -> IO GuestAgentConns
newGuestAgentConns logger = GuestAgentConns <$> newTVarIO Map.empty <*> pure logger <*> newTVarIO 0

getOrCreateConn :: GuestAgentConns -> Int64 -> IO (MVar (Maybe QgaSocket))
getOrCreateConn cache vmId = do
  let connsVar = guestAgentSlots cache
  mExisting <- atomically $ Map.lookup vmId <$> readTVar connsVar
  case mExisting of
    Just connection -> pure connection
    Nothing -> do
      newConnection <- newMVar Nothing
      atomically $ do
        conns <- readTVar connsVar
        case Map.lookup vmId conns of
          Just existing -> pure existing
          Nothing -> writeTVar connsVar (Map.insert vmId newConnection conns) >> pure newConnection

-- | Run an operation against the cached socket. The MVar both caches and
-- serializes QGA traffic; QEMU's chardev listen backlog is only one deep.
withPersistentConn :: GuestAgentConns -> QemuConfig -> Int64 -> Int -> Int -> (QgaSocket -> IO a) -> IO (Either Text a)
withPersistentConn = withConnection False

-- | Serialize once and discard the socket before releasing the lock. Used
-- for shutdown, which has no success reply and invalidates the guest session.
withDiscardedConn :: GuestAgentConns -> QemuConfig -> Int64 -> Int -> (QgaSocket -> IO a) -> IO (Either Text a)
withDiscardedConn conns config vmId = withConnection True conns config vmId 1

withConnection :: Bool -> GuestAgentConns -> QemuConfig -> Int64 -> Int -> Int -> (QgaSocket -> IO a) -> IO (Either Text a)
withConnection discard connsVar config vmId retries timeoutMicros action = do
  connVar <- getOrCreateConn connsVar vmId
  path <- getGuestAgentSocket config vmId
  go connVar path retries
  where
    go _ _ 0 = pure (Left "Guest agent unavailable after retries")
    go connVar path count = do
      result <- runOnce connVar path
      case result of
        Right value -> pure (Right value)
        Left _ | count > 1 -> threadDelay 1000000 >> go connVar path (count - 1)
        Left err -> pure (Left err)

    runOnce connVar path = mask $ \restore -> do
      mSock <- takeMVar connVar
      sockRef <- newIORef mSock
      result <-
        restore (doWork mSock path sockRef) `onException` do
          (readIORef sockRef >>= closeMaybe "cancelled") `finally` putMVar connVar Nothing
      case result of
        Right (sock, value) -> do
          if discard then closeQga "shutdown_discard" sock `finally` putMVar connVar Nothing else putMVar connVar (Just sock)
          pure (Right value)
        Left (err, reason) -> do
          (readIORef sockRef >>= closeMaybe reason) `finally` putMVar connVar Nothing
          pure (Left err)

    doWork mSock path sockRef = do
      -- Let timeout consume its own exception before handling other failures.
      mResult <- try $ timeout timeoutMicros $ do
        sock <- case mSock of
          Just existing -> logQga existing LevelDebug "reuse" "" >> pure existing
          Nothing -> mask_ $ do
            fresh <- connectWithRetry connsVar vmId 10 path sockRef
            writeIORef sockRef (Just fresh)
            pure fresh
        case mSock of
          Nothing -> syncGuest sock
          Just _ -> pure ()
        value <- action sock
        pure (sock, value)
      case mResult of
        Right (Just result) -> pure (Right result)
        Left (err :: SomeException) ->
          case fromException err :: Maybe SomeAsyncException of
            Just _ -> do
              readIORef sockRef >>= maybe (pure ()) (\sock -> logQga sock LevelWarn "operation_cancelled" "reason=external_cancellation")
              throwIO err
            Nothing -> do
              let category = case fromException err :: Maybe IOError of
                    Just ioErr -> T.pack (show (ioeGetErrorType ioErr))
                    Nothing -> "non_io_exception"
              readIORef sockRef >>= maybe (pure ()) (\sock -> logQga sock LevelWarn "operation_error" ("reason=exception error_type=" <> category))
              pure (Left (T.pack (show err), "exception"))
        Right Nothing -> do
          readIORef sockRef >>= maybe (pure ()) (\sock -> logQga sock LevelWarn "operation_timeout" ("timeout_us=" <> T.pack (show timeoutMicros)))
          pure (Left ("Timed out waiting for guest agent", "timeout"))

-- Each connect attempt owns its socket and ID, including backlog retries.
connectWithRetry :: GuestAgentConns -> Int64 -> Int -> FilePath -> IORef (Maybe QgaSocket) -> IO QgaSocket
connectWithRetry cache vmId count path sockRef = do
  connectionId <- atomically $ do
    n <- readTVar (guestAgentNextId cache)
    writeTVar (guestAgentNextId cache) (n + 1)
    pure (n + 1)
  raw <- socket AF_UNIX Stream defaultProtocol
  sock <- newQgaSocket (guestAgentLogger cache) vmId connectionId raw `onException` close raw
  writeIORef sockRef (Just sock)
  logQga sock LevelInfo "connect_start" ("path=" <> T.pack (show path))
  ( do
      connect raw (SockAddrUnix path)
      logQga sock LevelInfo "connect_complete" ""
      pure sock
    )
    `catch` \(err :: IOError) -> do
      logQga sock LevelWarn "connect_error" ("error_type=" <> T.pack (show (ioeGetErrorType err)))
      closeQga "connect_failed" sock
      writeIORef sockRef Nothing
      if count > 0 && ioeGetErrorType err == ResourceExhausted
        then do
          logQga sock LevelDebug "retry" "delay_ms=300"
          threadDelay 300000
          connectWithRetry cache vmId (count - 1) path sockRef
        else ioError err

closeMaybe :: Text -> Maybe QgaSocket -> IO ()
closeMaybe _ Nothing = pure ()
closeMaybe reason (Just sock) = closeQga reason sock

-- | Drop the map entry before waiting for its holder, avoiding a race where a
-- concurrently completing operation reinstates a socket for a retired VM.
releaseConn :: GuestAgentConns -> Int64 -> IO ()
releaseConn cache vmId = mask_ $ do
  let connsVar = guestAgentSlots cache
  mConnection <- atomically $ do
    conns <- readTVar connsVar
    case Map.lookup vmId conns of
      Nothing -> pure Nothing
      Just connection -> writeTVar connsVar (Map.delete vmId conns) >> pure (Just connection)
  maybe (pure ()) (takeMVar >=> closeMaybe "lifecycle_release") mConnection
  where
    (>=>) f g x = f x >>= g

-- | Sync IDs stay below 2^30 so JSON implementations using doubles retain
-- them exactly. Stale replies are drained until the matching ID arrives.
syncGuest :: QgaSocket -> IO ()
syncGuest sock = do
  syncId <- randomRIO (1, 1073741823 :: Int)
  logQga sock LevelInfo "sync_start" ("sync_id=" <> T.pack (show syncId))
  sendQga sock $ Aeson.object ["execute" .= ("guest-sync" :: Text), "arguments" .= Aeson.object ["id" .= syncId]]
  waitForSync syncId 10
  logQga sock LevelInfo "sync_complete" ("sync_id=" <> T.pack (show syncId))
  where
    waitForSync :: Int -> Int -> IO ()
    waitForSync _ 0 = ioError (userError "guest-sync: matching reply not received")
    waitForSync expected remaining = do
      response <- recvQga sock
      case response of
        Just (Object obj) | KM.lookup "return" obj == Just (Number (fromIntegral expected)) -> pure ()
        _ -> do
          let observed = case response of
                Just (Object obj) | Just (Number n) <- KM.lookup "return" obj -> " observed_sync_id=" <> T.pack (show n)
                _ -> ""
          logQga sock LevelDebug "sync_mismatch" ("expected_sync_id=" <> T.pack (show expected) <> observed)
          waitForSync expected (remaining - 1)

-- | Close a stale session while retaining the per-VM serialization lock.
invalidateConn :: GuestAgentConns -> Int64 -> IO ()
invalidateConn conns vmId = mask_ $ do
  slot <- getOrCreateConn conns vmId
  cached <- takeMVar slot
  closeMaybe "explicit_invalidation" cached `finally` putMVar slot Nothing
