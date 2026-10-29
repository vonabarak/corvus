{-# LANGUAGE OverloadedStrings #-}

-- | QGA's newline-delimited JSON transport and byte-level helpers.
module Corvus.Node.GuestAgent.Transport
  ( QgaSocket (..)
  , QgaLogger
  , newQgaSocket
  , logQga
  , sendQga
  , recvQga
  , recvQgaWithin
  , timeoutQgaReceive
  , closeQga
  , parseQgaFrame
  , sendJson
  , recvFrame
  , decodeBase64Bytes
  )
where

import Control.Exception (SomeAsyncException, SomeException, catch, finally, fromException, throwIO)
import Control.Monad.Logger (LogLevel (..))
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Word (Word64, Word8)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket (Socket, close)
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)

-- | The callback is the nodeagent's existing filtered logger.
type QgaLogger = LogLevel -> Text -> IO ()

-- | Metadata follows a socket across cache reuse. Traffic remains serialized
-- by the connection cache; the exchange state is diagnostic only.
data QgaSocket = QgaSocket
  { qgaSocket :: !Socket
  , qgaConnectionId :: !Integer
  , qgaVmId :: !Int64
  , qgaLogger :: QgaLogger
  , qgaExchange :: IORef (Integer, Text, Text)
  , qgaPhaseStarted :: IORef Word64
  , qgaStarted :: !Word64
  }

instance Show QgaSocket where
  show sock = "QgaSocket " <> show (qgaConnectionId sock)

instance Eq QgaSocket where
  a == b = qgaSocket a == qgaSocket b

newQgaSocket :: QgaLogger -> Int64 -> Integer -> Socket -> IO QgaSocket
newQgaSocket logger vmId connectionId sock = do
  now <- getMonotonicTimeNSec
  QgaSocket sock connectionId vmId logger <$> newIORef (0, "none", "connect") <*> newIORef now <*> pure now

-- | Never let a synchronous logger failure change a QGA operation. In
-- particular, never print arbitrary exception text, which can contain payloads.
logQga :: QgaSocket -> LogLevel -> Text -> Text -> IO ()
logQga sock level event fields = do
  utc <- getCurrentTime
  now <- getMonotonicTimeNSec
  phaseStarted <- readIORef (qgaPhaseStarted sock)
  (sequenceId, command, phase) <- readIORef (qgaExchange sock)
  let message =
        "qga event="
          <> event
          <> " utc="
          <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" utc)
          <> " vm_id="
          <> shown (qgaVmId sock)
          <> " connection_id="
          <> shown (qgaConnectionId sock)
          <> " sequence="
          <> shown sequenceId
          <> " command="
          <> command
          <> " phase="
          <> phase
          <> " elapsed_ms="
          <> shown ((now - qgaStarted sock) `div` 1000000)
          <> " phase_elapsed_ms="
          <> shown ((now - phaseStarted) `div` 1000000)
          <> " "
          <> fields
  qgaLogger sock level message `catch` \err ->
    case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO (err :: SomeException)
      Nothing -> pure ()
  where
    shown :: (Show a) => a -> Text
    shown = T.pack . show

sendQga :: QgaSocket -> Value -> IO ()
sendQga sock value = do
  let command = case value of
        Object obj -> case KM.lookup "execute" obj of
          Just (String name) | T.all (\c -> isAsciiLower c || c == '-') name -> name
          _ -> "unknown"
        _ -> "unknown"
  atomicModifyIORef' (qgaExchange sock) $ \(n, _, _) -> ((n + 1, command, "send"), ())
  getMonotonicTimeNSec >>= writeIORef (qgaPhaseStarted sock)
  logQga sock LevelDebug "send_start" ""
  sendJson (qgaSocket sock) value
  logQga sock LevelDebug "send_complete" ""
  writePhase sock "receive"

writePhase :: QgaSocket -> Text -> IO ()
writePhase sock phase = do
  atomicModifyIORef' (qgaExchange sock) $ \(n, command, _) -> ((n, command, phase), ())
  getMonotonicTimeNSec >>= writeIORef (qgaPhaseStarted sock)

recvQga :: QgaSocket -> IO (Maybe Value)
recvQga sock = do
  writePhase sock "receive"
  (response, eof) <- recvFrame (qgaSocket sock)
  let summary = case response of
        Just (Object obj) | KM.member "return" obj -> "result=success"
        Just (Object obj)
          | Just (Object err) <- KM.lookup "error" obj ->
              "result=qga_error error_class=" <> case KM.lookup "class" err of
                Just (String name) | T.length name <= 80 && T.all (\c -> isAsciiUpper c || isAsciiLower c) name -> name
                _ -> "unknown"
        _ | eof -> "result=eof"
        _ -> "result=malformed"
  (_, command, _) <- readIORef (qgaExchange sock)
  let level
        | "result=qga_error" `T.isPrefixOf` summary || summary == "result=malformed" || summary == "result=eof" && command /= "guest-shutdown" = LevelWarn
        | otherwise = LevelDebug
  logQga sock level "reply" summary
  writePhase sock "idle"
  pure response

recvQgaWithin :: Int -> QgaSocket -> Text -> IO (Maybe Value)
recvQgaWithin micros sock callLabel = do
  result <- timeoutQgaReceive micros sock
  case result of
    Just response -> pure response
    Nothing -> ioError . userError $ T.unpack callLabel <> ": no reply within " <> show (micros `div` 1000000) <> "s"

-- | Preserve each caller's timeout result and error policy while recording the
-- missing reply. A quiet shutdown is expected and stays at debug level.
timeoutQgaReceive :: Int -> QgaSocket -> IO (Maybe (Maybe Value))
timeoutQgaReceive micros sock = do
  result <- timeout micros (recvQga sock)
  case result of
    Nothing -> do
      (_, command, _) <- readIORef (qgaExchange sock)
      logQga
        sock
        (if command == "guest-shutdown" then LevelDebug else LevelWarn)
        "receive_timeout"
        ("timeout_us=" <> T.pack (show micros))
    Just _ -> pure ()
  pure result

closeQga :: Text -> QgaSocket -> IO ()
closeQga reason sock = do
  logQga sock LevelInfo "close_start" ("reason=" <> reason)
    `finally` ( close (qgaSocket sock) `catch` \err ->
                  case fromException err :: Maybe SomeAsyncException of
                    Just _ -> throwIO (err :: SomeException)
                    Nothing -> logQga sock LevelWarn "close_error" ("reason=" <> reason)
              )
  logQga sock LevelInfo "close_complete" ("reason=" <> reason)

-- | Parse the first valid JSON value in a QGA frame. QGA messages are
-- newline-delimited; @0xff@ delimiters may be left by guest-sync-delimited.
parseQgaFrame :: BS.ByteString -> Maybe Value
parseQgaFrame = firstParse . filter (not . BS.null) . BS.split nlByte . BS.filter (/= 0xFF)
  where
    firstParse [] = Nothing
    firstParse (line : rest) = case Aeson.decodeStrict line of
      Just value -> Just value
      Nothing -> firstParse rest

nlByte :: Word8
nlByte = fromIntegral (fromEnum '\n')

sendJson :: Socket -> Value -> IO ()
sendJson sock value = sendAll sock (BL.toStrict (Aeson.encode value) <> "\n")

-- | Receive until a complete line is available. Buffering intentionally stays
-- local to a request: QGA sends exactly one reply for each command.
recvFrame :: Socket -> IO (Maybe Value, Bool)
recvFrame sock = go BS.empty
  where
    go acc = do
      chunk <- recv sock 65536
      if BS.null chunk
        then pure (if BS.null acc then Nothing else parseQgaFrame acc, BS.null acc)
        else
          let acc' = acc <> chunk
           in if BS.elem nlByte acc' then pure (parseQgaFrame acc', False) else go acc'

decodeBase64Bytes :: Text -> BS.ByteString
decodeBase64Bytes text
  | T.null text = BS.empty
  | otherwise = case B64.decode (encodeUtf8 text) of
      Right decoded -> decoded
      Left _ -> encodeUtf8 text
