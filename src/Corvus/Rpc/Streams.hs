{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Server-side Cap'n Proto streaming sinks.
--
-- Hosts the daemon-side implementations of the sink caps declared
-- in @schema/streams.capnp@ that need to be exported from a
-- subsystem cap (serial console / HMP monitor input, guest-agent
-- status, task progress). Outbound sink calls (i.e. the daemon
-- pushing data through a client's sink) go through the local
-- 'callSink' helper.
module Corvus.Rpc.Streams
  ( -- * Bidirectional byte-pipe relay
    runByteSinkRelay

    -- * Server-side ByteSink (client → daemon direction)
  , QemuByteSink (..)

    -- * Server-side ByteSink: line-split adapter
  , LineBufferSink (..)
  , newLineBufferSink
  , feedLineBuffer
  , flushLineBuffer

    -- * Subscription handle
  , EmptyHandle (..)

    -- * Internal helper: call a method on a peer cap
  , callSink
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Streams as CGS
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Corvus.Node.SocketBuffer (readBufferFrom, stripTerminalQueries, waitForData)
import Corvus.Rpc.Common (handleParsed)
import Corvus.Types (SocketBufferHandle (..))
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as NSB
import Supervisors (Supervisor)

-- ---------------------------------------------------------------------
-- Outbound (daemon → client) helper
-- ---------------------------------------------------------------------

-- | Call a method on a peer cap (client-supplied 'ByteSink' /
-- 'BuildEventSink' / etc.). Mirrors the @callOn@ helper on the
-- client side; the daemon uses it to push events through caps it
-- has been handed.
callSink
  :: ( C.IsCap iface
     , C.IsStruct params
     , C.IsStruct results
     , C.Parse params (C.Parsed params)
     , C.Parse results (C.Parsed results)
     )
  => C.Method iface params results
  -> C.Parsed params
  -> C.Client iface
  -> IO ()
callSink method p client = do
  _ <- (client & C.callP method p) >>= C.waitPipeline
  pure ()

-- ---------------------------------------------------------------------
-- Inbound (client → daemon) sink: a server-side ByteSink that
-- forwards bytes onto a QEMU chardev socket.
-- ---------------------------------------------------------------------

-- | Server-side 'ByteSink' impl that forwards client writes onto
-- a QEMU chardev socket. The TVar mirrors 'SocketBufferHandle's
-- @sbhQemuSock@ so the sink picks up @Nothing@ once the chardev
-- shuts down.
data QemuByteSink = QemuByteSink
  { qbsQemuSock :: !(TVar (Maybe Socket))
  -- ^ Tracks the QEMU chardev socket; clears to @Nothing@ when
  -- QEMU exits, at which point further writes become no-ops.
  , qbsRelayAsync :: !(TVar (Maybe (Async ())))
  -- ^ Async pushing buffered bytes to the client sink; cancelled
  -- when the client side calls 'end' or disconnects.
  }

instance SomeServer QemuByteSink

instance CGS.ByteSink'server_ QemuByteSink where
  byteSink'write (QemuByteSink qsVar _) =
    handleParsed $ \CGS.ByteSink'write'params {CGS.chunk = chunk} -> do
      mSock <- readTVarIO qsVar
      case mSock of
        Nothing -> pure ()
        Just qSock -> do
          _ <-
            try (NSB.sendAll qSock chunk)
              :: IO (Either SomeException ())
          pure ()
      pure CGS.ByteSink'write'results

  byteSink'end (QemuByteSink _ relayVar) =
    handleParsed $ \_ -> do
      mRelay <- readTVarIO relayVar
      for_ mRelay cancel
      pure CGS.ByteSink'end'results

-- ---------------------------------------------------------------------
-- Line-split ByteSink
-- ---------------------------------------------------------------------

-- | Server-side 'ByteSink' impl that converts incoming raw byte
-- chunks into UTF-8 'Text' lines and fires a caller-supplied
-- callback per complete line. Partial trailing data is retained
-- between writes and flushed when 'byteSink'end' fires.
--
-- The MVar serialises both the partial-line buffer mutation AND
-- the @onLine@ callback dispatch: Cap'n Proto can dispatch
-- @write@ calls concurrently when multiple sinks share a thread
-- pool, and even a single sink's @write@ followed by @end@ may
-- race if the agent pipelines them.
data LineBufferSink = LineBufferSink
  { lbsState :: !(MVar BS.ByteString)
  -- ^ Pending partial-line bytes since the last newline.
  , lbsOnLine :: !(Text -> IO ())
  -- ^ Invoked once per complete line (without the trailing @\n@).
  }

instance SomeServer LineBufferSink

instance CGS.ByteSink'server_ LineBufferSink where
  byteSink'write lbs =
    handleParsed $ \CGS.ByteSink'write'params {CGS.chunk = chunk} -> do
      feedLineBuffer lbs chunk
      pure CGS.ByteSink'write'results

  byteSink'end lbs =
    handleParsed $ \_ -> do
      flushLineBuffer lbs
      pure CGS.ByteSink'end'results

-- | Allocate a fresh 'LineBufferSink' whose complete lines are
-- delivered to the supplied callback. Decoding is UTF-8 with the
-- lenient policy (invalid sequences become @U+FFFD@) so binary
-- noise in build output doesn't crash the daemon.
newLineBufferSink :: (Text -> IO ()) -> IO LineBufferSink
newLineBufferSink onLine = do
  v <- newMVar BS.empty
  pure LineBufferSink {lbsState = v, lbsOnLine = onLine}

-- | Feed a raw chunk into the buffer. Complete lines (terminated
-- by @\\n@) trigger the callback; any trailing partial is
-- retained for the next call. Exposed for tests.
feedLineBuffer :: LineBufferSink -> BS.ByteString -> IO ()
feedLineBuffer LineBufferSink {lbsState = stateVar, lbsOnLine = onLine} chunk =
  modifyMVar_ stateVar $ \prev -> do
    let combined = prev <> chunk
        (complete, trailing) = splitOnNewline combined
    mapM_ (onLine . decodeLine) complete
    pure trailing

-- | Flush any retained partial line through the callback. Idempotent.
-- Called by 'byteSink'end' once the peer signals end-of-stream.
flushLineBuffer :: LineBufferSink -> IO ()
flushLineBuffer LineBufferSink {lbsState = stateVar, lbsOnLine = onLine} =
  modifyMVar_ stateVar $ \prev -> do
    unless (BS.null prev) (onLine (decodeLine prev))
    pure BS.empty

-- | Split a byte string on @\\n@. Returns (completeLines, trailingPartial)
-- where each complete line is **without** the trailing newline and the
-- partial is whatever follows the last @\\n@ (empty if the input ended
-- in one). Mirrors 'Corvus.Node.GuestAgent.splitLines' but exposes
-- raw bytes (decoding happens at the line boundary).
--
-- A stray @\\r@ at the end of a line (i.e. @\\r\\n@ → "Windows
-- newline") is trimmed so consumers see the line content cleanly.
splitOnNewline :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitOnNewline bs =
  let nl = fromIntegral (fromEnum '\n') :: Word8
      pieces = BS.split nl bs
   in case reverse pieces of
        [] -> ([], BS.empty)
        (lastPiece : rest) -> (reverse (map stripCR rest), lastPiece)
  where
    cr = fromIntegral (fromEnum '\r') :: Word8
    stripCR s
      | not (BS.null s) && BS.last s == cr = BS.init s
      | otherwise = s

decodeLine :: BS.ByteString -> Text
decodeLine = TE.decodeUtf8With lenientDecode

-- ---------------------------------------------------------------------
-- Subscription handle (empty cap)
-- ---------------------------------------------------------------------

-- | Placeholder 'Handle' cap returned by every @subscribe@-style
-- method. The 'Handle' schema has no methods; clients use this
-- cap purely as a lifetime token: when they drop it, the server's
-- next push to the corresponding sink fails and the subscriber is
-- pruned. We re-export it from a single module so every
-- subscription endpoint shares one implementation.
data EmptyHandle = EmptyHandle

instance SomeServer EmptyHandle

instance CGS.Handle'server_ EmptyHandle

-- ---------------------------------------------------------------------
-- Bidirectional relay setup
-- ---------------------------------------------------------------------

-- | Wire a 'SocketBufferHandle' to a client-supplied 'ByteSink':
--
--   * Replay the current ring-buffer contents through @sink.write@.
--   * Spawn a relay async that streams live data from the buffer
--     to the sink. Exits if the sink dies (a write call throws),
--     if QEMU closes its end, or if 'cancel' is invoked.
--   * Build a 'QemuByteSink' that forwards client writes onto the
--     QEMU chardev socket, and export it through the daemon
--     supervisor so it can be handed back as the @input@ cap.
runByteSinkRelay
  :: Supervisor
  -> SocketBufferHandle
  -> C.Client CGS.ByteSink
  -- ^ Client-supplied output sink.
  -> IO (C.Client CGS.ByteSink)
  -- ^ Server-side input sink to hand back to the caller.
runByteSinkRelay sup sbh clientSink = do
  let buf = sbhBuffer sbh
  -- Replay the existing scrollback first so a reconnecting client
  -- sees recent output. Strip terminal query/response CSI
  -- sequences (DSR / DA / CPR) from the replay — replaying them
  -- prompts the client's terminal to answer stale queries, and
  -- the guest's TTY (in cooked-mode with echo) bounces the
  -- answers back as visible junk like `^[[65;1R` at the prompt.
  -- Live data after the initial replay is passed through
  -- unchanged so ncurses-style apps inside the guest can still
  -- query and receive cursor positions normally.
  -- Failures are swallowed: if the client cap is already dead,
  -- the live-stream loop below will exit on its own.
  (buffered, pos0) <- readBufferFrom buf 0
  _ <-
    try (pushChunk clientSink (stripTerminalQueries buffered))
      :: IO (Either SomeException ())
  -- Live data streamer.
  relayBox <- newTVarIO Nothing
  relayAsync <- async (streamLive buf pos0 clientSink (sbhShutdown sbh))
  atomically (writeTVar relayBox (Just relayAsync))
  -- Inbound sink: wraps the QEMU socket TVar from the handle so
  -- it observes shutdown the same way the read loop does.
  let qbs = QemuByteSink {qbsQemuSock = sbhQemuSock sbh, qbsRelayAsync = relayBox}
  C.export @CGS.ByteSink sup qbs
  where
    pushChunk client chunk
      | BS.null chunk = pure ()
      | otherwise =
          callSink
            #write
            CGS.ByteSink'write'params {CGS.chunk = chunk}
            client

    streamLive buf pos client shutdownVar = do
      shutdown <- readTVarIO shutdownVar
      if shutdown
        then pure ()
        else do
          (newData, newPos) <- waitForData buf pos
          if BS.null newData
            then do
              shutdown' <- readTVarIO shutdownVar
              if shutdown' then pure () else streamLive buf newPos client shutdownVar
            else do
              r <-
                try (pushChunk client newData)
                  :: IO (Either SomeException ())
              case r of
                Left _ -> pure ()
                Right () -> streamLive buf newPos client shutdownVar
