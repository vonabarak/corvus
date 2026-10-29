{-# LANGUAGE OverloadedStrings #-}

module Corvus.GuestAgentLoggingSpec (spec) where

import qualified Control.Concurrent
import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (AsyncException (ThreadKilled), bracket, throwIO)
import qualified Control.Exception
import Control.Monad (replicateM_, void)
import Control.Monad.Logger (LogLevel (..))
import Corvus.Node.GuestAgent (guestPing, guestShutdown)
import Corvus.Node.GuestAgent.Connection
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
  it "correlates synchronization, reuse, commands and lifecycle release" $ withFixture $ \cache config listener entries ->
    withAsync (bracket (fst <$> accept listener) close $ \peer -> handshake peer >> replicateM_ 2 (pingReply peer)) $ \server -> do
      replicateM_ 2 (guestPing cache config 42 `shouldReturn` True)
      releaseConn cache 42
      wait server
      logs <- entries
      mapM_
        (\event -> logs `shouldSatisfy` contains event)
        ["event=connect_start", "event=connect_complete", "event=sync_start", "event=sync_complete", "event=reuse", "sequence=3 command=guest-ping", "reason=lifecycle_release", "phase_elapsed_ms="]
      filter (contains "event=send_start" . pure) logs `shouldSatisfy` all ((== LevelDebug) . fst)
      filter ((>= LevelInfo) . fst) logs `shouldSatisfy` (not . contains "event=send_start")
      logs `shouldSatisfy` all (T.isInfixOf "vm_id=42 connection_id=1" . snd)

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

  it "records connect errors and distinct attempts without exception payloads" $ withFixture $ \cache config _ entries -> do
    let missing = config {qcRuntimeDir = Just "/nonexistent-corvus-qga-test"}
    result <- withPersistentConn cache missing 42 2 100000 (const (pure ()))
    result `shouldSatisfy` isLeft
    logs <- entries
    logs `shouldSatisfy` contains "event=connect_error"
    logs `shouldSatisfy` contains "reason=connect_failed"
    logs `shouldSatisfy` contains "connection_id=2"

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
    let logger level _
          | level == LevelDebug = throwIO ThreadKilled
          | otherwise = pure ()
        broken = cache {guestAgentLogger = logger}
    withAsync (bracket (fst <$> accept listener) close $ \peer -> recv peer 1 `shouldReturn` "") $ \server -> do
      withPersistentConn broken config 42 1 500000 (const (pure ())) `shouldThrow` (== ThreadKilled)
      wait server
      slots <- readTVarIO (guestAgentSlots cache)
      case Map.lookup 42 slots of
        Just slot -> readMVar slot `shouldReturn` Nothing
        Nothing -> expectationFailure "serialization slot disappeared"
