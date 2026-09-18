{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unix-socket transport and reply framing for QMP.
module Corvus.Node.Qmp.Transport
  ( sendQmpCommand
  , sendQmpRaw
  , extractReplyLine
  , classifyQmpResponse
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, bracket, catch, try)
import Corvus.Node.Qmp.Types (QmpResult (..))
import Corvus.Node.QmpQQ (qmpQQ)
import Corvus.Node.Runtime (getQmpSocket)
import Corvus.Qemu.Config (QemuConfig)
import qualified Data.ByteString as BSWide
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO.Exception (IOErrorType (..))
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..), close, connect, defaultProtocol, socket)
import Network.Socket.ByteString (recv, sendAll)
import System.IO.Error (ioeGetErrorType)

-- | Raw QMP send that returns the response bytes without coercing it to a
-- command result. Structured-reply commands decode this themselves.
sendQmpRaw :: QemuConfig -> Int64 -> BS.ByteString -> IO (Either Text BS.ByteString)
sendQmpRaw config vmId cmd = do
  qmpSock <- getQmpSocket config vmId
  result <- try $ withUnixSocket qmpSock $ \sock -> do
    _ <- recv sock 4096
    sendAll sock [qmpQQ| { "execute": "qmp_capabilities" } |]
    _ <- drainUntilReply sock BS.empty
    sendAll sock cmd
    drainUntilReply sock BS.empty
  pure $ case result of
    Left (e :: SomeException) -> Left (T.pack (show e))
    Right response -> Right response

-- | Send a QMP command to a VM.
sendQmpCommand :: QemuConfig -> Int64 -> BS.ByteString -> IO QmpResult
sendQmpCommand config vmId cmd = do
  qmpSock <- getQmpSocket config vmId
  result <- try $ withUnixSocket qmpSock $ \sock -> do
    _ <- recv sock 4096
    sendAll sock [qmpQQ| { "execute": "qmp_capabilities" } |]
    _ <- drainUntilReply sock BS.empty
    sendAll sock cmd
    drainUntilReply sock BS.empty
  pure $ case result of
    Left (e :: SomeException) -> QmpConnectionFailed $ T.pack $ show e
    Right response -> classifyQmpResponse response

-- | Pick the final command-reply line, ignoring interleaved QMP events.
extractReplyLine :: BS.ByteString -> Either Text BS.ByteString
extractReplyLine bs =
  case reverse (filter isReply (BS.lines bs)) of
    (x : _) -> Right x
    [] -> Left "no QMP reply line in response"
  where
    isReply line =
      BSWide.isInfixOf "\"return\"" line
        || BSWide.isInfixOf "\"error\"" line

-- | Read until QMP has sent a complete command reply. A reply may be preceded
-- by events and may span several reads, so the trailing newline is required.
drainUntilReply :: Socket -> BS.ByteString -> IO BS.ByteString
drainUntilReply sock acc = do
  chunk <- recv sock 4096
  if BS.null chunk
    then pure acc
    else do
      let combined = acc <> chunk
          hasReply = BS.isInfixOf "\"return\"" combined || BS.isInfixOf "\"error\"" combined
          messageComplete = case BS.unsnoc combined of
            Just (_, '\n') -> True
            _ -> False
      if hasReply && messageComplete
        then pure combined
        else drainUntilReply sock combined

-- | Classify a raw QMP response payload as success or error.
classifyQmpResponse :: BS.ByteString -> QmpResult
classifyQmpResponse response
  | BS.isInfixOf "\"return\"" response = QmpSuccess
  | otherwise = QmpError $ T.pack $ BS.unpack response

-- | Connect to a QMP Unix socket, retrying QEMU's one-slot listen backlog.
withUnixSocket :: FilePath -> (Socket -> IO a) -> IO a
withUnixSocket path = bracket (connectWithRetry 10) close
  where
    connectWithRetry :: Int -> IO Socket
    connectWithRetry 0 = do
      sock <- socket AF_UNIX Stream defaultProtocol
      connect sock (SockAddrUnix path)
      pure sock
    connectWithRetry n = do
      sock <- socket AF_UNIX Stream defaultProtocol
      (connect sock (SockAddrUnix path) >> pure sock)
        `catch` \(e :: IOException) ->
          if ioeGetErrorType e == ResourceExhausted
            then close sock >> threadDelay 300000 >> connectWithRetry (n - 1)
            else close sock >> ioError e
