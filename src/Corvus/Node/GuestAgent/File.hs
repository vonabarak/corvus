{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bounded binary transfers through native QGA file commands.
module Corvus.Node.GuestAgent.File
  ( guestWriteFile
  , guestReadFile
  , uploadFile
  , downloadFile
  , socketRequest
  ) where

import Control.Exception (SomeException, mask, throwIO, try)
import Control.Monad (unless, when)
import Corvus.Node.GuestAgent.Connection (GuestAgentConns, withPersistentConn)
import Corvus.Node.GuestAgent.Transport (recvJson, sendJson)
import Corvus.Qemu.Config (QemuConfig)
import Data.Aeson (Value, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import Network.Socket (Socket)
import System.Timeout (timeout)

type Request = Text -> [Pair] -> IO Value

-- | Bind a transfer to the first socket used. Never replay file commands on
-- a replacement connection: its handles may belong to a restarted guest.
fileSession :: GuestAgentConns -> QemuConfig -> Int64 -> IO Request
fileSession conns config vmId = do
  original <- newIORef Nothing
  pure $ \command args -> do
    result <- bounded 15000000 $ withPersistentConn conns config vmId 1 15000000 $ \sock -> do
      previous <- readIORef original
      case previous of
        Just old | old /= sock -> pure (Left "guest file session changed")
        _ -> do
          writeIORef original (Just sock)
          Right <$> socketRequest sock command args
    either failText (either failText pure) result

-- | One command, serialized by the caller. A timed-out transport is discarded.
socketRequest :: Socket -> Request
socketRequest sock command args = do
  sendJson sock $ Aeson.object ["execute" .= command, "arguments" .= Aeson.object args]
  response <- bounded 5000000 (recvJson sock)
  maybe (failText (command <> ": connection closed or invalid JSON")) pure response

parseReply :: Text -> (Value -> AT.Parser a) -> Value -> IO a
parseReply command parser response =
  case AT.parseEither (AT.withObject "QGA reply" (\obj -> obj .: "return" >>= parser)) response of
    Right value -> pure value
    Left err -> failText (command <> ": " <> T.pack err <> "; " <> T.pack (show response))

request :: Request -> Text -> [Pair] -> (Value -> AT.Parser a) -> IO a
request send command args parser = send command args >>= parseReply command parser

guestWriteFile :: GuestAgentConns -> QemuConfig -> Int64 -> Text -> (Int -> IO (BS.ByteString, Bool)) -> IO Word64
guestWriteFile conns config vmId path source = do
  send <- fileSession conns config vmId
  uploadFile send path source

guestReadFile :: GuestAgentConns -> QemuConfig -> Int64 -> Text -> (BS.ByteString -> IO ()) -> IO Word64
guestReadFile conns config vmId path sink = do
  send <- fileSession conns config vmId
  downloadFile send path sink

withFile :: Request -> Text -> Text -> (Int64 -> IO a) -> IO a
withFile send path mode body = do
  when (T.null path || T.any (== '\0') path) $ failText "guest file path must be nonempty and contain no NUL"
  mask $ \restore -> do
    handle <- request send "guest-file-open" ["path" .= path, "mode" .= mode] AT.parseJSON
    outcome <- try (restore (body handle))
    closed <- try (bounded 15000000 (request send "guest-file-close" ["handle" .= handle] (const (pure ()))))
    case outcome of
      Left (err :: SomeException) -> throwIO err
      Right value -> either (throwIO :: SomeException -> IO a) (const (pure value)) closed

-- | Await one source chunk at a time, including an explicit final reply.
uploadFile :: Request -> Text -> (Int -> IO (BS.ByteString, Bool)) -> IO Word64
uploadFile send path source = withFile send path "wb" $ \handle -> do
  let writeAll chunk
        | BS.null chunk = pure ()
        | otherwise = do
            count <- request send "guest-file-write" ["handle" .= handle, "buf-b64" .= TE.decodeUtf8 (B64.encode chunk)] $ AT.withObject "GuestFileWrite" (.: "count")
            when (count <= 0 || count > BS.length chunk) $ failText "guest-file-write: invalid byte count"
            writeAll (BS.drop count chunk)
      loop total = do
        (chunk, eof) <- bounded 60000000 (source 32768)
        when (BS.length chunk > 32768 || (BS.null chunk && not eof)) $ failText "invalid guest file source chunk"
        writeAll chunk
        let next = total + fromIntegral (BS.length chunk)
        if eof then pure next else loop next
  total <- loop 0
  request send "guest-file-flush" ["handle" .= handle] (const (pure ()))
  pure total

-- | Strict base64/count checks prevent malformed replies becoming file data.
downloadFile :: Request -> Text -> (BS.ByteString -> IO ()) -> IO Word64
downloadFile send path sink = withFile send path "rb" $ \handle -> do
  let loop total = do
        (count, encoded, eof) <- request send "guest-file-read" ["handle" .= handle, "count" .= (32768 :: Int)] $ AT.withObject "GuestFileRead" $ \obj -> (,,) <$> obj .: "count" <*> obj .: "buf-b64" <*> obj .: "eof"
        chunk <- either (failText . T.pack) pure (B64.decode (TE.encodeUtf8 encoded))
        unless (count == BS.length chunk && count <= 32768 && (count > 0 || eof)) $ failText "guest-file-read: invalid byte count or no progress"
        unless (BS.null chunk) $ bounded 60000000 (sink chunk)
        let next = total + fromIntegral count
        if eof then pure next else loop next
  loop 0

bounded :: Int -> IO a -> IO a
bounded micros action = timeout micros action >>= maybe (failText "guest file transfer timed out") pure

failText :: Text -> IO a
failText = ioError . userError . T.unpack
