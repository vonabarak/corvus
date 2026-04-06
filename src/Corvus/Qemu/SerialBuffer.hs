{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Serial console ring buffer and QEMU reader thread.
-- Maintains a 1MB scrollback buffer for headless VMs so that
-- reconnecting clients see recent serial output.
module Corvus.Qemu.SerialBuffer
  ( -- * Buffer operations
    newSerialBuffer
  , writeBuffer
  , readBufferFrom
  , waitForData

    -- * Background thread
  , startSerialBufferThread
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, try)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel, LoggingT, filterLogger, logDebugN, logWarnN, runStdoutLoggingT)
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Qemu.Runtime (getSerialSocket)
import Corvus.Types (SerialBuffer (..), SerialBufferHandle (..))
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..))
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv)

--------------------------------------------------------------------------------
-- Ring Buffer Operations
--------------------------------------------------------------------------------

-- | Default buffer capacity (1MB).
defaultCapacity :: Int
defaultCapacity = 1048576

-- | Create a new empty serial buffer.
newSerialBuffer :: IO SerialBuffer
newSerialBuffer = do
  d <- newTVarIO BS.empty
  tw <- newTVarIO 0
  n <- newEmptyTMVarIO
  pure
    SerialBuffer
      { sbData = d
      , sbTotalWritten = tw
      , sbNotify = n
      , sbCapacity = defaultCapacity
      }

-- | Append data to the ring buffer. Truncates to capacity from the front.
writeBuffer :: SerialBuffer -> BS.ByteString -> IO ()
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
readBufferFrom :: SerialBuffer -> Int64 -> IO (BS.ByteString, Int64)
readBufferFrom sb fromPos = atomically $ do
  buf <- readTVar (sbData sb)
  total <- readTVar (sbTotalWritten sb)
  let bufStart = total - fromIntegral (BS.length buf)
      effectiveFrom = max fromPos bufStart
      skipBytes = fromIntegral (effectiveFrom - bufStart)
      result = BS.drop skipBytes buf
  pure (result, total)

-- | Block until new data is available past the given position, then return it.
waitForData :: SerialBuffer -> Int64 -> IO (BS.ByteString, Int64)
waitForData sb fromPos = do
  atomically $ takeTMVar (sbNotify sb)
  readBufferFrom sb fromPos

--------------------------------------------------------------------------------
-- Background Thread
--------------------------------------------------------------------------------

-- | Start the serial buffer thread for a headless VM.
-- Connects to QEMU's serial.sock, reads output into a ring buffer,
-- and registers a SerialBufferHandle in the provided map.
-- The thread exits when the QEMU socket closes (VM stops).
startSerialBufferThread
  :: QemuConfig
  -> Int64
  -> TVar (Map.Map Int64 SerialBufferHandle)
  -> LogLevel
  -> IO ()
startSerialBufferThread config vmId bufferMap logLevel = void $ forkIO $ runFiltered logLevel $ do
  -- Wait for QEMU to create the serial socket
  liftIO $ threadDelay 1000000
  serialSockPath <- liftIO $ getSerialSocket config vmId
  logDebugN $ "Serial buffer connecting to " <> T.pack serialSockPath <> " for VM " <> T.pack (show vmId)
  result <-
    liftIO $
      try $
        bracket
          ( do
              sock <- NS.socket AF_UNIX NS.Stream NS.defaultProtocol
              NS.connect sock (SockAddrUnix serialSockPath)
              pure sock
          )
          NS.close
          (runReader vmId bufferMap)
  case result of
    Left (e :: SomeException) ->
      logWarnN $ "Serial buffer for VM " <> T.pack (show vmId) <> " failed: " <> T.pack (show e)
    Right () ->
      logDebugN $ "Serial buffer for VM " <> T.pack (show vmId) <> " exited"
  where
    runFiltered :: LogLevel -> LoggingT IO a -> IO a
    runFiltered minLvl = runStdoutLoggingT . filterLogger (\_ lvl -> lvl >= minLvl)

-- | QEMU reader loop. Registers the buffer handle, reads until EOF, then cleans up.
runReader :: Int64 -> TVar (Map.Map Int64 SerialBufferHandle) -> Socket -> IO ()
runReader vmId bufferMap qemuSock = do
  buf <- newSerialBuffer
  qemuSockVar <- newTVarIO (Just qemuSock)
  shutdownVar <- newTVarIO False
  let handle =
        SerialBufferHandle
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
