{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Serialized, persistent per-VM QGA connections.
module Corvus.Node.GuestAgent.Connection
  ( GuestAgentConns (..)
  , newGuestAgentConns
  , tryPersistentConnWhen
  , withPersistentConn
  , withPreparedConn
  , withDiscardedConn
  , invalidateConn
  , releaseConn
  , syncGuest
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (Exception, SomeAsyncException, SomeException, catch, evaluate, finally, fromException, mask, mask_, onException, throwIO, try)
import Control.Monad.Logger (LogLevel (..))
import Corvus.Node.GuestAgent.Transport (QgaLogger, QgaSocket (..), closeQga, logQga, logQgaEvent, newQgaSocket, recvQga, sendQga)
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
import GHC.Clock (getMonotonicTimeNSec)
import GHC.IO.Exception (IOErrorType (ResourceExhausted))
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), close, connect, defaultProtocol, socket)
import System.IO.Error (ioeGetErrorType)
import System.Mem.StableName (hashStableName, makeStableName)
import System.Random (randomRIO)
import System.Timeout (timeout)

data GuestAgentConns = GuestAgentConns
  { guestAgentSlots :: TVar (Map.Map Int64 (MVar (Maybe QgaSocket)))
  , guestAgentStartupReadiness :: Bool
  , guestAgentLogger :: QgaLogger
  , guestAgentNextOperationId :: TVar Integer
  , guestAgentNextId :: TVar Integer
  }

newGuestAgentConns :: QgaLogger -> IO GuestAgentConns
newGuestAgentConns logger = GuestAgentConns <$> newTVarIO Map.empty <*> pure False <*> pure logger <*> newTVarIO 0 <*> newTVarIO 0

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
withPersistentConn :: (HasCallStack) => GuestAgentConns -> QemuConfig -> Int64 -> Int -> Int -> (QgaSocket -> IO a) -> IO (Either Text a)
withPersistentConn conns config vmId retries budget action = withConnection (pure True) True False conns config vmId retries budget (RetryWhole action)

-- | Serialize once and discard the socket before releasing the lock. Used
-- for shutdown, which has no success reply and invalidates the guest session.
withDiscardedConn :: (HasCallStack) => GuestAgentConns -> QemuConfig -> Int64 -> Int -> (QgaSocket -> IO a) -> IO (Either Text a)
withDiscardedConn conns config vmId budget action = withConnection (pure True) True True conns config vmId 1 budget (RetryWhole action)

-- | Retry only preparation. The returned action may mutate the guest and is
-- run exactly once, with its own budget, while retaining the same socket lock.
withPreparedConn :: (HasCallStack) => GuestAgentConns -> QemuConfig -> Int64 -> Int -> Int -> Int -> (QgaSocket -> IO (IO a)) -> IO (Either Text a)
withPreparedConn conns config vmId retries prepareBudget dispatchBudget prepare =
  withConnection (pure True) True False conns config vmId retries prepareBudget (PrepareThenDispatch dispatchBudget prepare)

data QgaWork a
  = RetryWhole (QgaSocket -> IO a)
  | PrepareThenDispatch Int (QgaSocket -> IO (IO a))

withConnection :: (HasCallStack) => IO Bool -> Bool -> Bool -> GuestAgentConns -> QemuConfig -> Int64 -> Int -> Int -> QgaWork a -> IO (Either Text a)
withConnection allowed blocking discard connsVar config vmId retries timeoutMicros action = do
  operationId <- atomically $ do
    n <- readTVar (guestAgentNextOperationId connsVar)
    writeTVar (guestAgentNextOperationId connsVar) (n + 1)
    pure (n + 1)
  started <- getMonotonicTimeNSec
  let purpose
        | guestAgentStartupReadiness connsVar = "startup_readiness"
        | not blocking = "health_probe"
        | otherwise = "operation"
      context = "purpose=" <> purpose <> " operation_id=" <> tshow operationId <> " caller=" <> tshow (prettyCallStack callStack)
      event level name fields = logQgaEvent (guestAgentLogger connsVar) (if not blocking && level == LevelWarn then LevelDebug else level) name ("vm_id=" <> tshow vmId <> " " <> context <> " " <> fields)
      finish outcome = do
        now <- getMonotonicTimeNSec
        event LevelDebug "request_end" ("result=" <> outcome <> " duration_ms=" <> tshow ((now - started) `div` 1000000))
  event LevelDebug "request_start" ("timeout_us=" <> tshow timeoutMicros <> " timeout_scope=after_lock retries=" <> tshow retries <> " discard=" <> tshow discard)
  ( do
      connVar <- getOrCreateConn connsVar vmId
      path <- getGuestAgentSocket config vmId
      result <- go event context connVar path retries
      finish (either (const "error") (const "success") result)
      pure result
    )
    `onException` finish "exception_or_cancellation"
  where
    go _ _ _ _ 0 = pure (Left "Guest agent unavailable after retries")
    go event context connVar path count = do
      result <-
        runOnce event context connVar path (retries - count + 1)
          `catch` (\QgaRetired -> pure (Left ("Guest agent connection retired by VM lifecycle change", False)))
      event LevelDebug "attempt_end" ("attempt=" <> tshow (retries - count + 1) <> " result=" <> either (const "error") (const "success") result)
      case result of
        Right value -> pure (Right value)
        Left (_, True) | count > 1 -> do
          event LevelDebug "operation_retry" "delay_ms=1000"
          threadDelay 1000000
          go event context connVar path (count - 1)
        Left (err, _) -> pure (Left err)

    runOnce event context connVar path attempt = mask $ \restore -> do
      waiting <- getMonotonicTimeNSec
      slotIdentity <- hashStableName <$> (evaluate connVar >>= makeStableName)
      let attemptFields = "attempt=" <> tshow attempt <> " slot_identity=" <> tshow slotIdentity
      event LevelDebug "lock_wait" attemptFields :: IO ()
      mSock <-
        (if blocking then takeMVar connVar else tryTakeMVar connVar >>= maybe (throwIO QgaBusy) pure)
          `onException` event LevelDebug "lock_wait_cancelled" attemptFields
      acquired <- getMonotonicTimeNSec
      sockRef <- newIORef mSock
      result <-
        restore
          ( do
              event LevelDebug "lock_acquired" (attemptFields <> " wait_ms=" <> tshow ((acquired - waiting) `div` 1000000) <> " cached_connection_id=" <> maybe "none" (tshow . qgaConnectionId) mSock)
              current <- isCurrentSlot connsVar vmId connVar
              if current then pure () else throwIO QgaRetired
              eligible <- allowed
              if eligible then pure () else throwIO QgaBusy
              doWork event (context <> " slot_identity=" <> tshow slotIdentity) connVar mSock path sockRef
          )
          `onException` do
            (readIORef sockRef >>= closeMaybe "cancelled") `finally` putMVar connVar Nothing
      case result of
        Right (sock, value) -> do
          current <- isCurrentSlot connsVar vmId connVar
          if discard || not current then closeQga (if discard then "shutdown_discard" else "lifecycle_release") sock `finally` putMVar connVar Nothing else putMVar connVar (Just sock)
          pure (Right value)
        Left (err, reason, retryable) -> do
          (readIORef sockRef >>= closeMaybe reason) `finally` putMVar connVar Nothing
          pure (Left (err, retryable))

    doWork event context connVar mSock path sockRef = do
      let ensureCurrent = do
            current <- isCurrentSlot connsVar vmId connVar
            if current then pure () else throwIO QgaRetired
      committed <- newIORef False
      -- Preparation and dispatch have separate deadlines. The commit marker is
      -- set before invoking send, including writes that fail after partial delivery.
      mResult <- try $ do
        prepared <- timeout timeoutMicros $ do
          sock <- case mSock of
            Just existing -> writeIORef (qgaOperation existing) context >> logQga existing LevelDebug "reuse" "" >> pure existing
            Nothing -> mask_ $ do
              fresh <- connectWithRetry ensureCurrent connsVar vmId context 10 path sockRef
              writeIORef sockRef (Just fresh)
              pure fresh
          case mSock of
            Nothing -> syncGuest sock
            Just _ -> pure ()
          case action of
            RetryWhole run -> do
              ensureCurrent
              value <- run sock
              pure (sock, pure value)
            PrepareThenDispatch _ prepare -> (sock,) <$> prepare sock
        case prepared of
          Nothing -> pure Nothing
          Just (sock, dispatch) -> case action of
            RetryWhole _ -> Just . (sock,) <$> dispatch
            PrepareThenDispatch budget _ -> do
              ensureCurrent
              writeIORef committed True
              fmap (sock,) <$> timeout budget dispatch
      dispatched <- readIORef committed
      let failure err reason = Left (if dispatched then "Execution outcome unknown; command may have started; not retried: " <> err else err, reason, not dispatched)
      case mResult of
        Right (Just result) -> pure (Right result)
        Left err | Just QgaRetired <- fromException err -> pure (Left ("Guest agent connection retired by VM lifecycle change", "retired", False))
        Left (err :: SomeException) ->
          case fromException err :: Maybe SomeAsyncException of
            Just _ -> do
              readIORef sockRef >>= maybe (pure ()) (\sock -> logQga sock LevelDebug "operation_cancelled" "reason=external_cancellation")
              throwIO err
            Nothing -> do
              let category = case fromException err :: Maybe IOError of
                    Just ioErr -> T.pack (show (ioeGetErrorType ioErr))
                    Nothing -> "non_io_exception"
              event LevelWarn "attempt_exception" ("error_type=" <> category) :: IO ()
              readIORef sockRef >>= maybe (pure ()) (\sock -> logQga sock LevelWarn "operation_error" ("reason=exception error_type=" <> category))
              pure (failure (T.pack (show err)) "exception")
        Right Nothing -> do
          readIORef sockRef >>= maybe (pure ()) (\sock -> logQga sock LevelWarn "operation_timeout" ("timeout_us=" <> T.pack (show timeoutMicros)))
          pure (failure "Timed out waiting for guest agent" "timeout")

-- Each connect attempt owns its socket and ID, including backlog retries.
connectWithRetry :: IO () -> GuestAgentConns -> Int64 -> Text -> Int -> FilePath -> IORef (Maybe QgaSocket) -> IO QgaSocket
connectWithRetry ensureCurrent cache vmId context count path sockRef = do
  ensureCurrent
  connectionId <- atomically $ do
    n <- readTVar (guestAgentNextId cache)
    writeTVar (guestAgentNextId cache) (n + 1)
    pure (n + 1)
  raw <- socket AF_UNIX Stream defaultProtocol
  sock <- newQgaSocket (guestAgentLogger cache) vmId connectionId raw `onException` close raw
  writeIORef sockRef (Just sock)
  writeIORef (qgaOperation sock) context
  logQga sock LevelDebug "connect_start" ("path=" <> T.pack (show path))
  ( do
      connect raw (SockAddrUnix path)
      logQga sock LevelDebug "connect_complete" ""
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
          connectWithRetry ensureCurrent cache vmId context (count - 1) path sockRef
        else ioError err

closeMaybe :: Text -> Maybe QgaSocket -> IO ()
closeMaybe _ Nothing = pure ()
closeMaybe reason (Just sock) = closeQga reason sock

-- | Drop the map entry before waiting for its holder, avoiding a race where a
-- concurrently completing operation reinstates a socket for a retired VM.
releaseConn :: (HasCallStack) => GuestAgentConns -> Int64 -> IO ()
releaseConn cache vmId = mask_ $ do
  let connsVar = guestAgentSlots cache
  lifecycleEvent cache vmId "release_start" ("caller=" <> tshow (prettyCallStack callStack))
  mConnection <- atomically $ do
    conns <- readTVar connsVar
    case Map.lookup vmId conns of
      Nothing -> pure Nothing
      Just connection -> writeTVar connsVar (Map.delete vmId conns) >> pure (Just connection)
  case mConnection of
    Nothing -> pure ()
    Just slot -> do
      slotIdentity <- hashStableName <$> (evaluate slot >>= makeStableName)
      lifecycleEvent cache vmId "release_lock_wait" ("slot_identity=" <> tshow slotIdentity)
      cached <- takeMVar slot
      ( lifecycleEvent cache vmId "release_lock_acquired" ("slot_identity=" <> tshow slotIdentity)
          `finally` closeMaybe "lifecycle_release" cached
        )
        `finally` putMVar slot Nothing
  lifecycleEvent cache vmId "release_complete" ""

-- | Slot identity is the lifecycle token: a removed or replaced entry is retired.
-- Old waiters retain their slot, but must never reconnect to a new VM runtime.
isCurrentSlot :: GuestAgentConns -> Int64 -> MVar (Maybe QgaSocket) -> IO Bool
isCurrentSlot cache vmId slot = atomically $ do
  current <- Map.lookup vmId <$> readTVar (guestAgentSlots cache)
  pure (current == Just slot)

data QgaRetired = QgaRetired deriving stock (Show)

instance Exception QgaRetired

-- | Sync IDs stay below 2^30 so JSON implementations using doubles retain
-- them exactly. Stale replies are drained until the matching ID arrives.
syncGuest :: QgaSocket -> IO ()
syncGuest sock = do
  syncId <- randomRIO (1, 1073741823 :: Int)
  logQga sock LevelDebug "sync_start" ("sync_id=" <> T.pack (show syncId))
  sendQga sock $ Aeson.object ["execute" .= ("guest-sync" :: Text), "arguments" .= Aeson.object ["id" .= syncId]]
  waitForSync syncId 10
  logQga sock LevelDebug "sync_complete" ("sync_id=" <> T.pack (show syncId))
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
invalidateConn :: (HasCallStack) => GuestAgentConns -> Int64 -> IO ()
invalidateConn conns vmId = mask_ $ do
  lifecycleEvent conns vmId "invalidate_start" ("caller=" <> tshow (prettyCallStack callStack))
  slot <- getOrCreateConn conns vmId
  slotIdentity <- hashStableName <$> (evaluate slot >>= makeStableName)
  lifecycleEvent conns vmId "invalidate_lock_wait" ("slot_identity=" <> tshow slotIdentity)
  cached <- takeMVar slot
  ( do
      lifecycleEvent conns vmId "invalidate_lock_acquired" ("slot_identity=" <> tshow slotIdentity)
        `onException` closeMaybe "explicit_invalidation" cached
      closeMaybe "explicit_invalidation" cached
    )
    `finally` putMVar slot Nothing
  lifecycleEvent conns vmId "invalidate_complete" ""

-- | Identifiers and source locations only; never format guest payloads.
tshow :: (Show a) => a -> Text
tshow = T.pack . show

lifecycleEvent :: GuestAgentConns -> Int64 -> Text -> Text -> IO ()
lifecycleEvent cache vmId event fields =
  logQgaEvent (guestAgentLogger cache) LevelDebug event ("vm_id=" <> tshow vmId <> " " <> fields)

-- | Health observations must never queue behind an interactive guest operation.
data QgaBusy = QgaBusy deriving stock (Show)

instance Exception QgaBusy

-- | Eligibility is checked while holding the VM connection lock.
tryPersistentConnWhen :: (HasCallStack) => IO Bool -> GuestAgentConns -> QemuConfig -> Int64 -> Int -> (QgaSocket -> IO a) -> IO (Maybe (Either Text a))
tryPersistentConnWhen allowed conns cfg vmId deadline action =
  (Just <$> withConnection allowed False False conns cfg vmId 1 deadline (RetryWhole action))
    `catch` (\QgaBusy -> pure Nothing)
