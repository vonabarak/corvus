{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Serial console interaction helpers for headless VM integration tests.
-- Provides an expect-like interface over the QEMU serial console Unix socket.
module Test.VM.Console
  ( SerialConsole
  , connectSerialConsole
  , consoleSend
  , consoleExpect
  , consoleDrain
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, swapTVar, writeTVar)
import Control.Exception (SomeException, bracket, try)
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Qemu.Runtime (getSerialSocket)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..), close, defaultProtocol, socket)
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)

-- | A handle to an active serial console connection with background reader.
data SerialConsole = SerialConsole
  { scSocket :: !Socket
  , scBuffer :: !(TVar BS.ByteString)
  }

-- | Connect to a headless VM's serial console and run an action.
-- The serial socket path is derived from the VM ID.
-- A background reader thread continuously accumulates console output
-- into a buffer so no data is lost between send/expect calls.
connectSerialConsole :: QemuConfig -> Int64 -> (SerialConsole -> IO a) -> IO a
connectSerialConsole config vmId action = do
  serialSock <- getSerialSocket config vmId
  putStrLn $ "[console] Connecting to serial socket: " <> serialSock
  bracket
    (waitForSocket serialSock 30)
    close
    $ \sock -> do
      buf <- newTVarIO BS.empty
      let console = SerialConsole {scSocket = sock, scBuffer = buf}
      -- Background reader: recv from socket, append to buffer
      reader <- async $ do
        let loop = do
              chunk <- recv sock 4096
              if BS.null chunk
                then putStrLn "[console] Reader: socket closed"
                else do
                  atomically $ do
                    old <- readTVar buf
                    writeTVar buf (old <> chunk)
                  loop
        _ <- try loop :: IO (Either SomeException ())
        pure ()
      -- Run action, then cancel reader
      result <- try (action console)
      cancel reader
      case result of
        Left (e :: SomeException) -> do
          -- Dump buffer on failure for debugging
          bufContents <- readTVarIO buf
          putStrLn $ "[console] Buffer at failure:\n" <> T.unpack (T.decodeUtf8Lenient bufContents)
          fail $ "Serial console action failed: " <> show e
        Right a -> pure a

-- | Wait for a Unix socket to become available, retrying every 500ms.
waitForSocket :: FilePath -> Int -> IO Socket
waitForSocket sockPath timeoutSec = go (timeoutSec * 2)
  where
    go 0 = fail $ "waitForSocket: timed out waiting for " <> sockPath
    go n = do
      result <- try $ do
        sock <- socket AF_UNIX Stream defaultProtocol
        NS.connect sock (SockAddrUnix sockPath)
        pure sock
      case result of
        Right sock -> do
          putStrLn "[console] Connected to serial socket"
          pure sock
        Left (_ :: SomeException) -> do
          threadDelay 500000
          go (n - 1)

-- | Send a line to the serial console (appends \\r\\n).
consoleSend :: SerialConsole -> Text -> IO ()
consoleSend console text = do
  putStrLn $ "[console] Sending: " <> T.unpack text
  sendAll (scSocket console) (T.encodeUtf8 text <> "\r\n")

-- | Wait until a pattern appears in the console output buffer.
-- Returns all accumulated output when the pattern is found.
-- Fails with the buffer contents if the timeout (in seconds) expires.
consoleExpect :: SerialConsole -> Text -> Int -> IO Text
consoleExpect console pattern' timeoutSec = do
  putStrLn $ "[console] Expecting: " <> show pattern' <> " (timeout: " <> show timeoutSec <> "s)"
  let pollIntervalUs = 200000 :: Int -- 200ms
      maxPolls = (timeoutSec * 1000000) `div` pollIntervalUs
      go 0 = do
        bufContents <- readTVarIO (scBuffer console)
        let txt = T.decodeUtf8Lenient bufContents
        fail $
          "consoleExpect: timed out waiting for "
            <> show pattern'
            <> " after "
            <> show timeoutSec
            <> "s\nBuffer contents:\n"
            <> T.unpack txt
      go n = do
        bufContents <- readTVarIO (scBuffer console)
        let txt = T.decodeUtf8Lenient bufContents
        if T.isInfixOf pattern' txt
          then do
            putStrLn $ "[console] Found: " <> show pattern'
            pure txt
          else do
            threadDelay pollIntervalUs
            go (n - 1)
  go maxPolls

-- | Drain and return all buffered console output, clearing the buffer.
consoleDrain :: SerialConsole -> IO Text
consoleDrain console = do
  threadDelay 200000 -- 200ms to let pending data arrive
  bufContents <- atomically $ swapTVar (scBuffer console) BS.empty
  pure $ T.decodeUtf8Lenient bufContents
