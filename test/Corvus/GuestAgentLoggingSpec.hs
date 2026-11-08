{-# LANGUAGE OverloadedStrings #-}

module Corvus.GuestAgentLoggingSpec (spec) where

import qualified Control.Concurrent
import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (AsyncException (ThreadKilled), bracket, throwIO)
import qualified Control.Exception
import Control.Monad (replicateM_, void, when)
import Control.Monad.Logger (LogLevel (..))
import Corvus.Node.GuestAgent.Connection
import Corvus.Node.GuestAgent.Control (guestHealthProbe, guestPing, guestShutdown)
import Corvus.Node.GuestAgent.Transport
import Corvus.Node.Runtime (getGuestAgentSocket)
import Corvus.Qemu.Config (QemuConfig (..), defaultQemuConfig)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), accept, bind, close, defaultProtocol, listen, socket, socketPair)
import qualified Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

-- Bound both the implementation and scripted peer, so failed expectations
-- cannot leave this spec hanging on a socket or synchronization primitive.
within :: IO () -> IO ()
within action = timeout 4000000 action `shouldReturn` Just ()

withFixture :: (GuestAgentConns -> QemuConfig -> Socket -> IO [(LogLevel, Text)] -> IO ()) -> IO ()
withFixture action = within $ withSystemTempDirectory "qga-log" $ \dir -> do
  let config = defaultQemuConfig {qcRuntimeDir = Just dir}
  path <- getGuestAgentSocket config 42
  createDirectoryIfMissing True (takeDirectory path)
  entries <- newIORef []
  cache <- newGuestAgentConns (\level message -> atomicModifyIORef' entries (\xs -> (xs ++ [(level, message)], ())))
  bracket (socket AF_UNIX Stream defaultProtocol) close $ \listener -> do
    bind listener (SockAddrUnix path)
    listen listener 1
    action cache config listener (readIORef entries) `Control.Exception.finally` releaseConn cache 42

readJson :: Socket -> IO (Maybe Value)
readJson sock = fst <$> recvFrame sock

handshake :: Socket -> IO ()
handshake peer = do
  request <- readJson peer
  case request of
    Just (Object obj)
      | Just (Object args) <- KM.lookup "arguments" obj
      , Just ident <- KM.lookup "id" args ->
          sendJson peer (object ["return" .= ident])
    _ -> expectationFailure "expected guest-sync"

pingReply :: Socket -> IO ()
pingReply peer = do
  readJson peer `shouldReturn` Just (object ["execute" .= ("guest-ping" :: Text)])
  sendJson peer (object ["return" .= object []])

messages :: [(LogLevel, Text)] -> Text
messages = T.unlines . map snd

contains :: Text -> [(LogLevel, Text)] -> Bool
contains needle = T.isInfixOf needle . messages

spec :: Spec
spec = describe "GuestAgent connection logs" $ do
  it "defers a health probe without cancelling an operation holding the connection" $ withFixture $ \cache config listener _ -> do
    held <- newEmptyMVar
    release <- newEmptyMVar
    withAsync (bracket (fst <$> accept listener) close handshake) $ \server ->
      withAsync (withPersistentConn cache config 42 1 2000000 (\_ -> putMVar held () >> takeMVar release)) $ \operation -> do
        takeMVar held
        timeout 100000 (guestHealthProbe cache config 42) `shouldReturn` Just Nothing
        putMVar release ()
        wait operation `shouldReturn` Right ()
        wait server

  it "classifies cached-socket health failures by the current operation purpose" $ withFixture $ \cache config listener entries ->
    withAsync
      ( bracket (fst <$> accept listener) close $ \peer -> do
          handshake peer
          pingReply peer
          readJson peer `shouldReturn` Just (object ["execute" .= ("guest-ping" :: Text)])
      )
      $ \server -> do
        guestPing cache config 42 `shouldReturn` True
        result <- guestHealthProbe cache config 42
        wait server
        result `shouldSatisfy` maybe False (either (const True) (const False))
        logs <- entries
        let healthLogs = filter (T.isInfixOf "purpose=health_probe " . snd) logs
        healthLogs `shouldSatisfy` (not . null)
        healthLogs `shouldSatisfy` all ((== LevelDebug) . fst)

  it "logs expected startup synchronization timeouts at Debug" $ withFixture $ \cache config listener entries ->
    withAsync (bracket (fst <$> accept listener) close (\_ -> Control.Concurrent.threadDelay 2000000)) $ \_ -> do
      _ <- withPersistentConn (cache {guestAgentStartupReadiness = True}) config 42 1 50000 (\_ -> pure ())
      logs <- entries
      logs `shouldSatisfy` contains "event=operation_timeout"
      filter (T.isInfixOf "event=operation_timeout" . snd) logs `shouldSatisfy` all ((== LevelDebug) . fst)

  it "correlates synchronization, reuse, commands and lifecycle release" $ withFixture $ \cache config listener entries ->
    withAsync (bracket (fst <$> accept listener) close $ \peer -> handshake peer >> replicateM_ 2 (pingReply peer)) $ \server -> do
      replicateM_ 2 (guestPing cache config 42 `shouldReturn` True)
      releaseConn cache 42
      wait server
      logs <- entries
      mapM_
        (\event -> logs `shouldSatisfy` contains event)
        ["event=connect_start", "event=connect_complete", "event=sync_start", "event=sync_complete", "event=reuse", "sequence=3 command=guest-ping", "reason=lifecycle_release", "phase_elapsed_ms="]
      logs `shouldSatisfy` all ((== LevelDebug) . fst)
      filter ((>= LevelInfo) . fst) logs `shouldSatisfy` (not . contains "event=send_start")
      logs `shouldSatisfy` all (T.isInfixOf "vm_id=42" . snd)
      let socketLogs = filter (contains " connection_id=" . pure) logs
      socketLogs `shouldSatisfy` all (T.isInfixOf "vm_id=42 connection_id=1" . snd)
      logs `shouldSatisfy` contains "operation_id=1"
      logs `shouldSatisfy` contains "operation_id=2"
      logs `shouldSatisfy` contains "GuestAgentLoggingSpec.hs"
      logs `shouldSatisfy` all (T.isInfixOf "thread_id=" . snd)
      logs `shouldSatisfy` all (T.isInfixOf "monotonic_ns=" . snd)

  it "allocates another connection ID after invalidation" $ withFixture $ \cache config listener entries ->
    withAsync (replicateM_ 2 (bracket (fst <$> accept listener) close $ \peer -> handshake peer >> pingReply peer)) $ \server -> do
      guestPing cache config 42 `shouldReturn` True
      invalidateConn cache 42
      guestPing cache config 42 `shouldReturn` True
      wait server
      logs <- entries
      logs `shouldSatisfy` contains "reason=explicit_invalidation"
      logs `shouldSatisfy` contains "connection_id=2"

  it "records the command and budget when synchronization stalls" $ withFixture $ \cache config listener entries ->
    withAsync (bracket (fst <$> accept listener) close $ \peer -> void (readJson peer) >> (recv peer 1 `shouldReturn` "")) $ \server -> do
      result <- withPersistentConn cache config 42 1 30000 (const (pure ()))
      result `shouldSatisfy` isLeft
      wait server
      logs <- entries
      logs `shouldSatisfy` contains "event=operation_timeout"
      filter (contains "event=operation_timeout" . pure) logs `shouldSatisfy` all ((== LevelDebug) . fst)
      let failures = filter ((== LevelWarn) . fst) logs
      length failures `shouldBe` 1
      failures `shouldSatisfy` contains "event=operation_failed"
      logs `shouldSatisfy` contains "command=guest-sync phase=receive"
      logs `shouldSatisfy` contains "timeout_us=30000"
      logs `shouldSatisfy` contains "reason=timeout"
      logs `shouldSatisfy` (not . contains "event=sync_complete")

  it "identifies a blocked send without logging its payload" $ withFixture $ \cache config listener entries -> do
    completed <- newEmptyMVar
    withAsync (bracket (fst <$> accept listener) close $ \peer -> handshake peer >> takeMVar completed) $ \server -> do
      result <- withPersistentConn cache config 42 1 500000 $ \sock ->
        sendQga sock (object ["execute" .= ("guest-exec" :: Text), "arguments" .= object ["input-data" .= T.replicate 1000000 "SECRET"]])
      putMVar completed ()
      result `shouldSatisfy` isLeft
      wait server
      logs <- entries
      logs `shouldSatisfy` contains "event=operation_timeout"
      filter (contains "event=operation_timeout" . pure) logs `shouldSatisfy` all ((== LevelDebug) . fst)
      let failures = filter ((== LevelWarn) . fst) logs
      length failures `shouldBe` 1
      failures `shouldSatisfy` contains "event=operation_failed"
      logs `shouldSatisfy` contains "command=guest-exec phase=send"
      filter (contains "command=guest-exec" . pure) logs `shouldSatisfy` (not . contains "event=send_complete")
      logs `shouldSatisfy` (not . contains "SECRET")

  it "distinguishes cancellation from timeout and restores the slot" $ withFixture $ \cache config listener entries -> do
    started <- newEmptyMVar
    withAsync (bracket (fst <$> accept listener) close $ \peer -> handshake peer >> void (readJson peer) >> (recv peer 1 `shouldReturn` "")) $ \server -> do
      withAsync
        ( withPersistentConn cache config 42 1 1000000 $ \sock -> do
            sendQga sock (object ["execute" .= ("guest-ping" :: Text)])
            putMVar started ()
            recvQga sock
        )
        $ \worker -> do
          takeMVar started
          cancel worker
      wait server
      slots <- readTVarIO (guestAgentSlots cache)
      case Map.lookup 42 slots of
        Just slot -> readMVar slot `shouldReturn` Nothing
        Nothing -> expectationFailure "serialization slot disappeared"
      logs <- entries
      logs `shouldSatisfy` contains "event=operation_cancelled"
      logs `shouldSatisfy` contains "reason=cancelled"
      logs `shouldSatisfy` (not . contains "event=operation_timeout")

  it "attributes cancellation while queued without closing the holder's socket" $ withFixture $ \cache config listener entries -> do
    held <- newEmptyMVar
    resume <- newEmptyMVar
    withAsync (bracket (fst <$> accept listener) close $ \peer -> handshake peer >> pingReply peer) $ \server ->
      withAsync
        (withPersistentConn cache config 42 1 2000000 $ \_ -> putMVar held () >> takeMVar resume)
        $ \holder -> do
          takeMVar held
          -- The outer deadline expires before the operation acquires the lock;
          -- its own timeout has not started and the holder must be unaffected.
          timeout 30000 (guestPing cache config 42) `shouldReturn` Nothing
          logs <- entries
          let cancelled = filter (contains "event=lock_wait_cancelled" . pure) logs
          cancelled `shouldSatisfy` (not . null)
          cancelled `shouldSatisfy` contains "operation_id=2"
          logs `shouldSatisfy` (not . contains "event=close_start")
          putMVar resume ()
          wait holder `shouldReturn` Right ()
          guestPing cache config 42 `shouldReturn` True
          wait server
          finalLogs <- entries
          finalLogs `shouldSatisfy` contains "cached_connection_id=1"
          finalLogs `shouldSatisfy` (not . contains "connection_id=2")

  it "retires queued callers without dispatching them or stranding their slot" $ withFixture $ \cache config listener entries -> do
    held <- newEmptyMVar
    resume <- newEmptyMVar
    queued <- newEmptyMVar
    retired <- newEmptyMVar
    let logger level message = do
          guestAgentLogger cache level message
          when ("event=lock_wait " `T.isInfixOf` message && "operation_id=2 " `T.isInfixOf` message) $ putMVar queued ()
          when ("event=release_lock_wait " `T.isInfixOf` message) $ putMVar retired ()
        observed = cache {guestAgentLogger = logger}
    withAsync (bracket (fst <$> accept listener) close $ \peer -> handshake peer >> (recv peer 1 `shouldReturn` "")) $ \server ->
      withAsync (withPersistentConn observed config 42 1 2000000 $ \_ -> putMVar held () >> takeMVar resume) $ \holder -> do
        takeMVar held
        slots <- readTVarIO (guestAgentSlots cache)
        oldSlot <- maybe (expectationFailure "missing held slot" >> fail "missing held slot") pure (Map.lookup 42 slots)
        withAsync (withPersistentConn observed config 42 2 2000000 $ \_ -> expectationFailure "retired request dispatched") $ \waiter -> do
          takeMVar queued
          withAsync (releaseConn observed 42) $ \releaser -> do
            takeMVar retired
            putMVar resume ()
            wait holder `shouldReturn` Right ()
            wait waiter `shouldReturn` Left "Guest agent connection retired by VM lifecycle change"
            wait releaser
            readMVar oldSlot `shouldReturn` Nothing
        wait server
        logs <- entries
        filter ((>= LevelWarn) . fst) logs `shouldBe` []

  it "does not dispatch prepared work after a cancelled lifecycle release" $ withFixture $ \cache config listener _ -> do
    prepared <- newEmptyMVar
    resume <- newEmptyMVar
    retired <- newEmptyMVar
    let logger level message = do
          guestAgentLogger cache level message
          when ("event=release_lock_wait " `T.isInfixOf` message) $ putMVar retired ()
        observed = cache {guestAgentLogger = logger}
    withAsync (bracket (fst <$> accept listener) close $ \peer -> handshake peer >> (recv peer 1 `shouldReturn` "")) $ \server ->
      withAsync
        ( withPreparedConn observed config 42 2 2000000 2000000 $ \_ -> do
            putMVar prepared ()
            takeMVar resume
            pure (expectationFailure "retired prepared command dispatched")
        )
        $ \worker -> do
          takeMVar prepared
          slots <- readTVarIO (guestAgentSlots cache)
          oldSlot <- maybe (fail "missing held slot") pure (Map.lookup 42 slots)
          withAsync (releaseConn observed 42) $ \releaser -> do
            takeMVar retired
            cancel releaser
          putMVar resume ()
          wait worker `shouldReturn` Left "Guest agent connection retired by VM lifecycle change"
          readMVar oldSlot `shouldReturn` Nothing
          wait server

  it "does not reconnect a retry after its slot is retired" $ withFixture $ \cache config _ entries -> do
    retrying <- newEmptyMVar
    resume <- newEmptyMVar
    let logger level message = do
          guestAgentLogger cache level message
          when ("event=operation_retry " `T.isInfixOf` message) $ putMVar retrying () >> takeMVar resume
        observed = cache {guestAgentLogger = logger}
    withAsync (withPersistentConn observed config {qcRuntimeDir = Just "/nonexistent-corvus-qga-test"} 42 2 100000 (const (pure ()))) $ \worker -> do
      takeMVar retrying
      releaseConn observed 42
      putMVar resume ()
      wait worker `shouldReturn` Left "Guest agent connection retired by VM lifecycle change"
      Map.member 42 <$> readTVarIO (guestAgentSlots cache) `shouldReturn` False
      logs <- entries
      filter ((>= LevelWarn) . fst) logs `shouldBe` []
      logs `shouldSatisfy` contains "event=operation_failed"

  it "attributes external cancellation during synchronization and restores the slot" $ withFixture $ \cache config listener entries -> do
    syncing <- newEmptyMVar
    withAsync (bracket (fst <$> accept listener) close $ \peer -> void (readJson peer) >> putMVar syncing () >> (recv peer 1 `shouldReturn` "")) $ \server -> do
      withAsync (withPersistentConn cache config 42 1 2000000 (const (pure ()))) $ \worker -> do
        takeMVar syncing
        cancel worker
      wait server
      logs <- entries
      let cancelled = filter (contains "event=operation_cancelled" . pure) logs
      cancelled `shouldSatisfy` contains "operation_id=1"
      cancelled `shouldSatisfy` contains "command=guest-sync phase=receive"
      logs `shouldSatisfy` contains "event=request_end"
      logs `shouldSatisfy` (not . contains "event=operation_timeout")
      slots <- readTVarIO (guestAgentSlots cache)
      case Map.lookup 42 slots of
        Just slot -> readMVar slot `shouldReturn` Nothing
        Nothing -> expectationFailure "serialization slot disappeared"

  it "records connect errors and distinct attempts without exception payloads" $ withFixture $ \cache config _ entries -> do
    let missing = config {qcRuntimeDir = Just "/nonexistent-corvus-qga-test"}
    result <- withPersistentConn cache missing 42 2 100000 (const (pure ()))
    result `shouldSatisfy` isLeft
    logs <- entries
    logs `shouldSatisfy` contains "event=connect_error"
    logs `shouldSatisfy` contains "reason=connect_failed"
    logs `shouldSatisfy` contains "connection_id=2"
    let failures = filter ((== LevelWarn) . fst) logs
    length failures `shouldBe` 1
    failures `shouldSatisfy` contains "event=operation_failed"
    failures `shouldSatisfy` contains "error_type="
    failures `shouldSatisfy` (not . contains "/nonexistent-corvus-qga-test")

  it "logs a recovered transport retry only at Debug" $ withFixture $ \cache config listener entries ->
    withAsync
      ( do
          bracket (fst <$> accept listener) close $ \peer -> void (readJson peer)
          bracket (fst <$> accept listener) close $ \peer -> handshake peer >> pingReply peer
      )
      $ \server -> do
        result <- withPersistentConn cache config 42 2 500000 $ \sock -> do
          sendQga sock (object ["execute" .= ("guest-ping" :: Text)])
          recvQgaWithin 100000 sock "guest-ping"
        result `shouldBe` Right (Just (object ["return" .= object []]))
        wait server
        logs <- entries
        logs `shouldSatisfy` contains "event=operation_retry"
        filter ((>= LevelWarn) . fst) logs `shouldBe` []

  it "defers failed startup readiness diagnostics to its coordinator" $ withFixture $ \cache config _ entries -> do
    let readiness = cache {guestAgentStartupReadiness = True}
        missing = config {qcRuntimeDir = Just "/nonexistent-corvus-qga-test"}
    result <- withPersistentConn readiness missing 42 1 100000 (const (pure ()))
    result `shouldSatisfy` isLeft
    logs <- entries
    logs `shouldSatisfy` contains "event=operation_failed"
    logs `shouldSatisfy` contains "purpose=startup_readiness"
    filter ((>= LevelWarn) . fst) logs `shouldBe` []

  it "defers failed shutdown signalling diagnostics to the lifecycle caller" $ withFixture $ \cache config _ entries -> do
    result <- guestShutdown cache config {qcRuntimeDir = Just "/nonexistent-corvus-qga-test"} 42 "powerdown"
    result `shouldSatisfy` isLeft
    logs <- entries
    logs `shouldSatisfy` contains "event=operation_failed"
    filter ((>= LevelWarn) . fst) logs `shouldBe` []

  it "logs stale synchronization IDs before the matching reply" $ withFixture $ \cache config listener entries ->
    withAsync
      ( bracket (fst <$> accept listener) close $ \peer -> do
          request <- readJson peer
          sendJson peer (object ["return" .= (0 :: Int)])
          case request of
            Just (Object obj)
              | Just (Object args) <- KM.lookup "arguments" obj
              , Just ident <- KM.lookup "id" args -> do
                  -- Wait for a second receive; separate frames must not coalesce
                  -- because the existing transport intentionally keeps no tail buffer.
                  let awaitMismatch = do
                        logs <- entries
                        if contains "event=sync_mismatch" logs then pure () else Control.Concurrent.threadDelay 1000 >> awaitMismatch
                  awaitMismatch
                  sendJson peer (object ["return" .= ident])
            _ -> expectationFailure "expected guest-sync"
          pingReply peer
      )
      $ \server -> do
        guestPing cache config 42 `shouldReturn` True
        wait server
        logs <- entries
        logs `shouldSatisfy` contains "event=sync_mismatch"
        logs `shouldSatisfy` contains "observed_sync_id=0"

  it "keeps expected shutdown EOF at debug and logs discard" $ withFixture $ \cache config listener entries ->
    withAsync (bracket (fst <$> accept listener) close $ \peer -> handshake peer >> void (readJson peer)) $ \server -> do
      guestShutdown cache config 42 "powerdown" `shouldReturn` Right ()
      wait server
      logs <- entries
      logs `shouldSatisfy` contains "reason=shutdown_discard"
      filter (contains "result=eof" . pure) logs `shouldSatisfy` all ((== LevelDebug) . fst)

  it "summarizes malformed replies and EOF without recording raw data" $
    within $
      bracket (socketPair AF_UNIX Stream defaultProtocol) (\(a, b) -> close a >> close b) $ \(client, peer) -> do
        entries <- newIORef []
        sock <- newQgaSocket (\level message -> atomicModifyIORef' entries (\xs -> (xs ++ [(level, message)], ()))) 42 7 client
        sendAll peer "SECRET-RAW-REPLY\n"
        recvQga sock `shouldReturn` Nothing
        Network.Socket.shutdown peer Network.Socket.ShutdownSend
        recvQga sock `shouldReturn` Nothing
        logs <- readIORef entries
        logs `shouldSatisfy` contains "result=malformed"
        logs `shouldSatisfy` contains "result=eof"
        filter (contains "result=malformed" . pure) logs `shouldSatisfy` all ((== LevelWarn) . fst)
        filter (contains "result=eof" . pure) logs `shouldSatisfy` all ((== LevelDebug) . fst)
        logs `shouldSatisfy` (not . contains "SECRET")

  it "logs receive timeouts and redacts payloads and error descriptions" $ withFixture $ \cache config listener entries ->
    withAsync
      ( bracket (fst <$> accept listener) close $ \peer -> do
          handshake peer
          void (readJson peer)
          sendJson peer (object ["error" .= object ["class" .= ("CommandNotFound" :: Text), "desc" .= ("SECRET-ERROR" :: Text)]])
          void (readJson peer)
          recv peer 1 `shouldReturn` ""
      )
      $ \server -> do
        result <- withPersistentConn cache config 42 1 500000 $ \sock -> do
          sendQga sock (object ["execute" .= ("guest-exec" :: Text), "arguments" .= object ["input-data" .= ("SECRET-STDIN" :: Text), "arg" .= ["SECRET-SCRIPT" :: Text]]])
          void (recvQga sock)
          sendQga sock (object ["execute" .= ("guest-file-read" :: Text)])
          recvQgaWithin 30000 sock "guest-file-read"
        result `shouldSatisfy` isLeft
        wait server
        logs <- entries
        logs `shouldSatisfy` contains "error_class=CommandNotFound"
        logs `shouldSatisfy` contains "event=receive_timeout"
        logs `shouldSatisfy` (not . contains "SECRET")

  it "does not let a synchronous logger exception break successful commands" $
    within $
      bracket (socketPair AF_UNIX Stream defaultProtocol) (\(a, b) -> close a >> close b) $ \(client, peer) -> do
        sock <- newQgaSocket (\_ _ -> ioError (userError "SECRET-LOGGER")) 42 1 client
        withAsync (pingReply peer) $ \server -> do
          sendQga sock (object ["execute" .= ("guest-ping" :: Text)])
          recvQga sock `shouldReturn` Just (object ["return" .= object []])
          wait server

  it "propagates asynchronous logger exceptions while closing and restoring the slot" $ withFixture $ \cache config listener _ -> do
    let logger _ message
          | "event=sync_start " `T.isInfixOf` message = throwIO ThreadKilled
          | otherwise = pure ()
        broken = cache {guestAgentLogger = logger}
    withAsync (bracket (fst <$> accept listener) close $ \peer -> recv peer 1 `shouldReturn` "") $ \server -> do
      withPersistentConn broken config 42 1 500000 (const (pure ())) `shouldThrow` (== ThreadKilled)
      wait server
      slots <- readTVarIO (guestAgentSlots cache)
      case Map.lookup 42 slots of
        Just slot -> readMVar slot `shouldReturn` Nothing
        Nothing -> expectationFailure "serialization slot disappeared"
