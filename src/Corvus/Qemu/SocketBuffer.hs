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
module Corvus.Qemu.SocketBuffer
  ( -- * Buffer operations
    newSocketBuffer
  , writeBuffer
  , readBufferFrom
  , waitForData
  , flushBuffer

    -- * Background thread
  , startSocketBufferThread

    -- * Client relay
  , relayClient

    -- * Replay sanitisation (exposed for tests)
  , stripTerminalQueries
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel, LoggingT, filterLogger, logDebugN, logWarnN, runStdoutLoggingT)
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Types (SocketBuffer (..), SocketBufferHandle (..))
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..))
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)

--------------------------------------------------------------------------------
-- Ring Buffer Operations
--------------------------------------------------------------------------------

-- | Create an empty ring buffer with the given capacity (bytes).
newSocketBuffer :: Int -> IO SocketBuffer
newSocketBuffer capacity = do
  d <- newTVarIO BS.empty
  tw <- newTVarIO 0
  n <- newEmptyTMVarIO
  pure
    SocketBuffer
      { sbData = d
      , sbTotalWritten = tw
      , sbNotify = n
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
  void $ tryPutTMVar (sbNotify sb) ()

-- | Read buffer contents starting from a given absolute position.
-- Returns (data, newPosition). If the requested position has been
-- overwritten (buffer wrapped), returns data from the oldest available byte.
readBufferFrom :: SocketBuffer -> Int64 -> IO (BS.ByteString, Int64)
readBufferFrom sb fromPos = atomically $ do
  buf <- readTVar (sbData sb)
  total <- readTVar (sbTotalWritten sb)
  let bufStart = total - fromIntegral (BS.length buf)
      effectiveFrom = max fromPos bufStart
      skipBytes = fromIntegral (effectiveFrom - bufStart)
      result = BS.drop skipBytes buf
  pure (result, total)

-- | Block until new data is available past the given position, then return it.
waitForData :: SocketBuffer -> Int64 -> IO (BS.ByteString, Int64)
waitForData sb fromPos = do
  atomically $ takeTMVar (sbNotify sb)
  readBufferFrom sb fromPos

-- | Clear the ring buffer. New writes continue from the current position.
flushBuffer :: SocketBuffer -> IO ()
flushBuffer sb = atomically $ writeTVar (sbData sb) BS.empty

-- | Drop CSI terminal queries and their responses from a byte
-- string: @\ESC[…n@ (DSR), @\ESC[…c@ (DA, including @\ESC[?…c@ and
-- @\ESC[>c@), and @\ESC[…R@ (CPR reports). All of these are
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

    go [] = []
    go (0x1b : 0x5b : rest) =
      let (params, afterParams) = span isParam rest
          (inters, afterInters) = span isInter afterParams
       in case afterInters of
            (final : rest')
              | final `elem` [0x6e, 0x63, 0x52] -> go rest' -- drop DSR / DA / CPR
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
  -> LogLevel
  -> IO ()
startSocketBufferThread _config vmId sockPath bufferMap capacity label logLevel =
  void $ forkIO $ runFiltered logLevel $ do
    -- Wait for QEMU to create the socket
    liftIO $ threadDelay 1000000
    logDebugN $ label <> " buffer connecting to " <> T.pack sockPath <> " for VM " <> T.pack (show vmId)
    result <-
      liftIO $
        try $
          bracket
            ( do
                sock <- NS.socket AF_UNIX NS.Stream NS.defaultProtocol
                NS.connect sock (SockAddrUnix sockPath)
                pure sock
            )
            NS.close
            (runReader vmId bufferMap capacity)
    case result of
      Left (e :: SomeException) ->
        logWarnN $ label <> " buffer for VM " <> T.pack (show vmId) <> " failed: " <> T.pack (show e)
      Right () ->
        logDebugN $ label <> " buffer for VM " <> T.pack (show vmId) <> " exited"
  where
    runFiltered :: LogLevel -> LoggingT IO a -> IO a
    runFiltered minLvl = runStdoutLoggingT . filterLogger (\_ lvl -> lvl >= minLvl)

-- | QEMU reader loop. Registers the buffer handle, reads until EOF, then cleans up.
runReader :: Int64 -> TVar (Map.Map Int64 SocketBufferHandle) -> Int -> Socket -> IO ()
runReader vmId bufferMap capacity qemuSock = do
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
  atomically $ modifyTVar' bufferMap (Map.insert vmId handle)
  -- Read loop
  let loop = do
        chunk <- recv qemuSock 4096
        if BS.null chunk
          then pure () -- EOF: QEMU exited
          else do
            writeBuffer buf chunk
            loop
  loop
  -- Cleanup: mark shutdown, remove from map
  atomically $ do
    writeTVar shutdownVar True
    writeTVar qemuSockVar Nothing
    modifyTVar' bufferMap (Map.delete vmId)
    void $ tryPutTMVar (sbNotify buf) ()

--------------------------------------------------------------------------------
-- Client Relay
--------------------------------------------------------------------------------

-- | Relay raw bytes between an RPC client socket and a VM's socket
-- buffer. Sends the current buffer contents first so reconnecting
-- clients see recent scrollback, then streams live data
-- bidirectionally. Blocks until either the client disconnects or
-- QEMU closes its side.
--
-- Safe for multiple concurrent clients against the same handle: each
-- call holds its own read position and its own per-client input
-- forker.
relayClient :: Socket -> SocketBufferHandle -> IO ()
relayClient clientSock handle = do
  let buf = sbhBuffer handle
  -- Send buffered output to client. Strip terminal query/response
  -- CSI sequences from the replay so the client's terminal doesn't
  -- reply to stale queries — those replies get echoed by the VM's
  -- TTY and show up as visible junk (e.g. ";1R;124R") at the user's
  -- prompt. Live data after the initial replay is passed through
  -- unchanged so real applications can still query/receive normally.
  (buffered, pos) <- readBufferFrom buf 0
  unless (BS.null buffered) $
    sendAll clientSock (stripTerminalQueries buffered)
  exitVar <- newEmptyMVar
  -- QEMU → client: stream new data from the ring buffer.
  _ <- forkIO $ do
    let loop curPos = do
          shutdown <- readTVarIO (sbhShutdown handle)
          if shutdown
            then putMVar exitVar ()
            else do
              (newData, newPos) <- waitForData buf curPos
              if BS.null newData
                then do
                  shutdown' <- readTVarIO (sbhShutdown handle)
                  if shutdown' then putMVar exitVar () else loop newPos
                else do
                  result <- try $ sendAll clientSock newData
                  case result of
                    Left (_ :: SomeException) -> putMVar exitVar ()
                    Right () -> loop newPos
    result <- try $ loop pos
    case result of
      Left (_ :: SomeException) -> putMVar exitVar ()
      Right () -> pure ()
  -- Client → QEMU: forward client input to QEMU's socket.
  _ <- forkIO $ do
    let loop = do
          chunk <- recv clientSock 4096
          if BS.null chunk
            then putMVar exitVar ()
            else do
              mQemuSock <- readTVarIO (sbhQemuSock handle)
              case mQemuSock of
                Nothing -> putMVar exitVar ()
                Just qSock -> do
                  result <- try $ sendAll qSock chunk
                  case result of
                    Left (_ :: SomeException) -> putMVar exitVar ()
                    Right () -> loop
    result <- try loop
    case result of
      Left (_ :: SomeException) -> putMVar exitVar ()
      Right () -> pure ()
  takeMVar exitVar
