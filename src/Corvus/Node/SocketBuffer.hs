{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Ring-buffered relay for QEMU Unix-socket chardevs.
--
-- The daemon holds a persistent connection to QEMU's socket, tails
-- output into a per-VM ring buffer, and lets RPC clients relay their
-- own socket into and out of that buffer. Serves both the headless
-- serial console and the HMP monitor; other socket-based QEMU
-- chardevs can be added by wiring a new map + socket-path + label
-- into 'startSocketBufferThread'.
module Corvus.Node.SocketBuffer
  ( -- * Buffer operations
    newSocketBuffer
  , writeBuffer
  , readBufferFrom
  , waitForData
  , flushBuffer

    -- * Background thread
  , startSocketBufferThread

    -- * Replay sanitisation

  -- Called from 'Corvus.Rpc.Streams.runByteSinkRelay'; also
  -- exposed for tests.
  , stripTerminalQueries
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, asyncWithUnmask)
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException, SomeException, bracket, finally, fromException, mask, mask_, throwIO, try)
import Control.Monad (when)
import Control.Monad.Logger (LogLevel (..))
import Corvus.Node.GuestAgent.Transport (QgaLogger)
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Types (SocketBuffer (..), SocketBufferHandle (..))
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.Error (Errno (..), eCONNREFUSED)
import GHC.IO.Exception (IOException (ioe_errno))
import Network.Socket (Family (..), SockAddr (..), Socket)
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv)
import System.IO.Error (isDoesNotExistError)

--------------------------------------------------------------------------------
-- Ring Buffer Operations
--------------------------------------------------------------------------------

-- | Create an empty ring buffer with the given capacity (bytes).
newSocketBuffer :: Int -> IO SocketBuffer
newSocketBuffer capacity = do
  d <- newTVarIO BS.empty
  tw <- newTVarIO 0
  pure
    SocketBuffer
      { sbData = d
      , sbTotalWritten = tw
      , sbCapacity = capacity
      }

-- | Append data to the ring buffer. Truncates to capacity from the front.
writeBuffer :: SocketBuffer -> BS.ByteString -> IO ()
writeBuffer sb chunk = atomically $ do
  old <- readTVar (sbData sb)
  let new = old <> chunk
      trimmed =
        if BS.length new > sbCapacity sb
          then BS.drop (BS.length new - sbCapacity sb) new
          else new
  writeTVar (sbData sb) trimmed
  modifyTVar' (sbTotalWritten sb) (+ fromIntegral (BS.length chunk))

-- | Read buffer contents starting from a given absolute position.
-- Returns (data, newPosition). If the requested position has been
-- overwritten (buffer wrapped), returns data from the oldest available byte.
readBufferFrom :: SocketBuffer -> Int64 -> IO (BS.ByteString, Int64)
readBufferFrom sb fromPos = atomically $ readBufferFromSTM sb fromPos

readBufferFromSTM :: SocketBuffer -> Int64 -> STM (BS.ByteString, Int64)
readBufferFromSTM sb fromPos = do
  buf <- readTVar (sbData sb)
  total <- readTVar (sbTotalWritten sb)
  let bufStart = total - fromIntegral (BS.length buf)
      effectiveFrom = max fromPos bufStart
      skipBytes = fromIntegral (effectiveFrom - bufStart)
      result = BS.drop skipBytes buf
  pure (result, total)

-- | Block until new data is available past the given position, then return it.
waitForData :: SocketBuffer -> Int64 -> TVar Bool -> IO (BS.ByteString, Int64)
waitForData sb fromPos shutdown = atomically $ do
  total <- readTVar (sbTotalWritten sb)
  stopped <- readTVar shutdown
  check (total > fromPos || stopped)
  readBufferFromSTM sb fromPos

-- | Clear the ring buffer. New writes continue from the current position.
flushBuffer :: SocketBuffer -> IO ()
flushBuffer sb = atomically $ writeTVar (sbData sb) BS.empty

-- | Drop CSI terminal queries and their responses from a byte
-- string: @\ESC[…n@ (DSR), @\ESC[…c@ (DA, including @\ESC[?…c@ and
-- @\ESC[>c@), @\ESC[…R@ (CPR reports), and @\ESC[…t@ (XTWINOPS,
-- with numeric/semicolon parameters and no intermediates). All of these are
-- control-channel chatter that only makes sense between a live app
-- and its terminal; replaying them at reconnect either triggers a
-- stale query/response loop or renders as garbage.
stripTerminalQueries :: BS.ByteString -> BS.ByteString
stripTerminalQueries = BS.pack . go . BS.unpack
  where
    -- CSI parameter bytes: digits plus @;@ @:@ @<@ @=@ @>@ @?@.
    isParam c = c >= 0x30 && c <= 0x3f
    -- Intermediate bytes (rare in practice but legal in CSI): @ ! " # $ % & ' ( ) * + , - . /@.
    isInter c = c >= 0x20 && c <= 0x2f
    -- Standard XTWINOPS uses only decimal parameters separated by semicolons.
    -- Other CSI commands ending in @t@ have private or intermediate bytes.
    isWindowParam c = (c >= 0x30 && c <= 0x39) || c == 0x3b

    go [] = []
    go (0x1b : 0x5b : rest) =
      let (params, afterParams) = span isParam rest
          (inters, afterInters) = span isInter afterParams
       in case afterInters of
            (final : rest')
              | final `elem` [0x6e, 0x63, 0x52] -> go rest' -- drop DSR / DA / CPR
              | final == 0x74 && null inters && all isWindowParam params -> go rest' -- drop XTWINOPS
              | otherwise -> 0x1b : 0x5b : params ++ inters ++ final : go rest'
            [] -> 0x1b : 0x5b : params ++ inters
    go (b : rest) = b : go rest

--------------------------------------------------------------------------------
-- Background Thread
--------------------------------------------------------------------------------

-- | Start the per-VM socket-buffer thread.
--
-- Connects to QEMU's chardev at @sockPath@, reads output into a ring
-- buffer of the requested capacity, and registers a handle in
-- @bufferMap@. The thread exits when QEMU closes the socket (VM
-- stops). @label@ appears in log lines to distinguish serial vs
-- monitor threads.
startSocketBufferThread
  :: QemuConfig
  -> Int64
  -- ^ VM id
  -> FilePath
  -- ^ Path to QEMU's Unix socket (serial, monitor, …)
  -> TVar (Map.Map Int64 SocketBufferHandle)
  -- ^ Map to register the handle in
  -> Int
  -- ^ Ring buffer capacity in bytes
  -> Text
  -- ^ Label for log messages (e.g. @"serial"@ or @"monitor"@)
  -> TVar [Async ()]
  -> STM Bool
  -- ^ Current runtime, still running and not stopping.
  -> QgaLogger
  -> IO ()
startSocketBufferThread _config vmId sockPath bufferMap capacity label workers eligible logger = mask_ $ do
  gate <- newEmptyTMVarIO
  worker <- asyncWithUnmask $ \unmask -> unmask $ do
    atomically $ takeTMVar gate
    result <- try $ connectLoop (50 :: Int)
    case result of
      Left (e :: SomeException) -> case fromException e :: Maybe SomeAsyncException of
        Just _ -> logger LevelDebug (prefix <> "retired") >> throwIO e
        Nothing -> do
          current <- atomically eligible
          logger (if current then LevelWarn else LevelDebug) (prefix <> "failed: " <> T.pack (show e))
      Right () -> logger LevelDebug (prefix <> "exited")
  atomically $ modifyTVar' workers (worker :) >> putTMVar gate ()
  where
    prefix = label <> " buffer for VM " <> T.pack (show vmId) <> " "
    connectLoop attempts = do
      current <- atomically eligible
      when current $ do
        -- Own the descriptor before connect: a failed connect must close it.
        result <- bracket (NS.socket AF_UNIX NS.Stream NS.defaultProtocol) NS.close $ \sock -> do
          connected <- try $ NS.connect sock (SockAddrUnix sockPath)
          case connected of
            Left err -> pure (Left err)
            Right () -> runReader vmId bufferMap capacity eligible sock >> pure (Right ())
        case result of
          Right () -> pure ()
          Left (err :: IOError)
            | attempts > 0 && (isDoesNotExistError err || ioe_errno err == Just refused) -> threadDelay 100000 >> connectLoop (attempts - 1)
            | otherwise -> ioError err
    Errno refused = eCONNREFUSED

-- | QEMU reader loop. Registers the buffer handle, reads until EOF, then cleans up.
runReader :: Int64 -> TVar (Map.Map Int64 SocketBufferHandle) -> Int -> STM Bool -> Socket -> IO ()
runReader vmId bufferMap capacity eligible qemuSock = mask $ \restore -> do
  buf <- newSocketBuffer capacity
  qemuSockVar <- newTVarIO (Just qemuSock)
  shutdownVar <- newTVarIO False
  let handle =
        SocketBufferHandle
          { sbhBuffer = buf
          , sbhQemuSock = qemuSockVar
          , sbhShutdown = shutdownVar
          }
  -- Register in map
  registered <- atomically $ do
    current <- eligible
    when current $ modifyTVar' bufferMap (Map.insert vmId handle)
    pure current
  -- Read loop. The cleanup runs under 'finally' so the map entry +
  -- shutdown signal land even if 'recv' throws (which it does on a
  -- hard-killed QEMU — the kernel resets the socket instead of
  -- sending the clean EOF the normal-exit path delivers). Without
  -- it, a SIGKILL'd VM leaves a dead 'SocketBufferHandle' in the
  -- map for the agent's lifetime.
  let loop = do
        chunk <- recv qemuSock 4096
        if BS.null chunk
          then pure () -- EOF: QEMU exited
          else do
            writeBuffer buf chunk
            loop
      cleanup = atomically $ do
        writeTVar shutdownVar True
        writeTVar qemuSockVar Nothing
        modifyTVar' bufferMap (Map.update (\current -> if sbhShutdown current == shutdownVar then Nothing else Just current) vmId)
  restore (when registered loop) `finally` cleanup
