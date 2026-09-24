{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Serialized, persistent per-VM QGA connections.
module Corvus.Node.GuestAgent.Connection
  ( GuestAgentConns
  , withPersistentConn
  , releaseConn
  , syncGuest
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Exception (SomeException, catch, mask, mask_, onException, try)
import Corvus.Node.GuestAgent.Transport (recvJson, sendJson)
import Corvus.Node.Runtime (getGuestAgentSocket)
import Corvus.Qemu.Config (QemuConfig)
import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO.Exception (IOErrorType (ResourceExhausted))
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), close, connect, defaultProtocol, socket)
import System.IO.Error (ioeGetErrorType)
import System.Random (randomRIO)
import System.Timeout (timeout)

type GuestAgentConns = TVar (Map.Map Int64 (MVar (Maybe Socket)))

getOrCreateConn :: GuestAgentConns -> Int64 -> IO (MVar (Maybe Socket))
getOrCreateConn connsVar vmId = do
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
withPersistentConn :: GuestAgentConns -> QemuConfig -> Int64 -> Int -> Int -> (Socket -> IO a) -> IO (Either Text a)
withPersistentConn connsVar config vmId retries timeoutMicros action = do
  connVar <- getOrCreateConn connsVar vmId
  path <- getGuestAgentSocket config vmId
  go connVar path retries
  where
    go _ _ 0 = pure (Left "Guest agent unavailable after retries")
    go connVar path count = do
      result <- runOnce connVar path
      case result of
        Right value -> pure (Right value)
        Left err | count > 1 -> threadDelay 1000000 >> go connVar path (count - 1)
        Left err -> pure (Left err)

    runOnce connVar path = mask $ \restore -> do
      mSock <- takeMVar connVar
      sockRef <- newIORef mSock
      result <-
        restore (doWork mSock path sockRef) `onException` do
          readIORef sockRef >>= closeMaybe
          putMVar connVar Nothing
      case result of
        Right (sock, value) -> putMVar connVar (Just sock) >> pure (Right value)
        Left err -> do
          readIORef sockRef >>= closeMaybe
          putMVar connVar Nothing
          pure (Left err)

    doWork mSock path sockRef = do
      mResult <- timeout timeoutMicros $ try $ do
        sock <- case mSock of
          Just existing -> pure existing
          Nothing -> mask_ $ do
            fresh <- connectWithRetry 10 path
            writeIORef sockRef (Just fresh)
            pure fresh
        case mSock of
          Nothing -> syncGuest sock
          Just _ -> pure ()
        value <- action sock
        pure (sock, value)
      case mResult of
        Just (Right result) -> pure (Right result)
        Just (Left (err :: SomeException)) -> pure (Left (T.pack (show err)))
        Nothing -> pure (Left "Timed out waiting for guest agent")

connectWithRetry :: Int -> FilePath -> IO Socket
connectWithRetry 0 path = socket AF_UNIX Stream defaultProtocol >>= \sock -> connect sock (SockAddrUnix path) >> pure sock
connectWithRetry count path = do
  sock <- socket AF_UNIX Stream defaultProtocol
  (connect sock (SockAddrUnix path) >> pure sock) `catch` \(err :: IOError) ->
    if ioeGetErrorType err == ResourceExhausted
      then close sock >> threadDelay 300000 >> connectWithRetry (count - 1) path
      else close sock >> ioError err

closeSafe :: Socket -> IO ()
closeSafe sock = close sock `catch` \(_ :: SomeException) -> pure ()

closeMaybe :: Maybe Socket -> IO ()
closeMaybe Nothing = pure ()
closeMaybe (Just sock) = closeSafe sock

-- | Drop the map entry before waiting for its holder, avoiding a race where a
-- concurrently completing operation reinstates a socket for a retired VM.
releaseConn :: GuestAgentConns -> Int64 -> IO ()
releaseConn connsVar vmId = do
  mConnection <- atomically $ do
    conns <- readTVar connsVar
    case Map.lookup vmId conns of
      Nothing -> pure Nothing
      Just connection -> writeTVar connsVar (Map.delete vmId conns) >> pure (Just connection)
  maybe (pure ()) (takeMVar >=> closeMaybe) mConnection
  where
    (>=>) f g x = f x >>= g

-- | Sync IDs stay below 2^30 so JSON implementations using doubles retain
-- them exactly. Stale replies are drained until the matching ID arrives.
syncGuest :: Socket -> IO ()
syncGuest sock = do
  syncId <- randomRIO (1, 1073741823 :: Int)
  sendJson sock $ Aeson.object ["execute" .= ("guest-sync" :: Text), "arguments" .= Aeson.object ["id" .= syncId]]
  waitForSync syncId 10
  where
    waitForSync _ 0 = pure ()
    waitForSync expected remaining = do
      response <- recvJson sock
      case response of
        Just (Object obj) | KM.lookup "return" obj == Just (Number (fromIntegral expected)) -> pure ()
        _ -> waitForSync expected (remaining - 1)
