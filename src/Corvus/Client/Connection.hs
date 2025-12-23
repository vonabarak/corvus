{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Connection handling for the Corvus client.
module Corvus.Client.Connection
  ( -- * Connection type
    Connection (..),
    ConnectionError (..),

    -- * Connecting
    withConnection,
    withTcpConnection,
    withUnixConnection,

    -- * Low-level communication
    sendRequest,
  )
where

import Control.Exception (Exception, SomeException, bracket, try)
import Corvus.Protocol (Request, Response)
import Corvus.Types (ListenAddress (..))
import Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Simple.TCP (connect)
import Network.Socket
  ( Family (AF_UNIX),
    Socket,
    SocketType (Stream),
    close,
    socket,
  )
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)

-- | Connection handle
newtype Connection = Connection Socket

-- | Connection errors
data ConnectionError
  = ConnectFailed !Text
  | SendFailed !Text
  | RecvFailed !Text
  | DecodeFailed !Text
  | ServerError !Text
  deriving (Eq, Show)

instance Exception ConnectionError

-- | Connect to the server and run an action with the connection
withConnection :: ListenAddress -> (Connection -> IO a) -> IO (Either ConnectionError a)
withConnection addr action = case addr of
  TcpAddress host port -> withTcpConnection host port action
  UnixAddress path -> withUnixConnection path action

-- | Connect via TCP
withTcpConnection :: String -> Int -> (Connection -> IO a) -> IO (Either ConnectionError a)
withTcpConnection host port action = do
  result <- try $ connect host (show port) $ \(sock, _addr) ->
    action (Connection sock)
  case result of
    Left (e :: SomeException) -> pure $ Left $ ConnectFailed $ T.pack $ show e
    Right a -> pure $ Right a

-- | Connect via Unix socket
withUnixConnection :: FilePath -> (Connection -> IO a) -> IO (Either ConnectionError a)
withUnixConnection path action = do
  result <- try $ bracket (socket AF_UNIX Stream 0) close $ \sock -> do
    NS.connect sock (NS.SockAddrUnix path)
    action (Connection sock)
  case result of
    Left (e :: SomeException) -> pure $ Left $ ConnectFailed $ T.pack $ show e
    Right a -> pure $ Right a

-- | Send a request and receive a response
sendRequest :: Connection -> Request -> IO (Either ConnectionError Response)
sendRequest (Connection sock) req = do
  -- Encode and send request
  let payload = BL.toStrict $ encode req
      len = fromIntegral (BS.length payload) :: Int64
      lenBytes = BL.toStrict $ encode len

  sendResult <- try $ sendAll sock (lenBytes <> payload)
  case sendResult of
    Left (e :: SomeException) -> pure $ Left $ SendFailed $ T.pack $ show e
    Right () -> do
      -- Receive response
      recvResult <- try $ recvResponse sock
      case recvResult of
        Left (e :: SomeException) -> pure $ Left $ RecvFailed $ T.pack $ show e
        Right resp -> pure resp

-- | Receive a response from the socket
recvResponse :: Socket -> IO (Either ConnectionError Response)
recvResponse sock = do
  -- Read length prefix (8 bytes for Int64)
  lenBs <- recvExact sock 8
  case lenBs of
    Nothing -> pure $ Left $ RecvFailed "Connection closed"
    Just lenBytes -> do
      case decodeOrFail (BL.fromStrict lenBytes) of
        Left (_, _, err) -> pure $ Left $ DecodeFailed $ T.pack err
        Right (_, _, len) -> do
          -- Read payload
          payloadBs <- recvExact sock (fromIntegral (len :: Int64))
          case payloadBs of
            Nothing -> pure $ Left $ RecvFailed "Connection closed during payload"
            Just payload -> do
              case decodeOrFail (BL.fromStrict payload) of
                Left (_, _, err) -> pure $ Left $ DecodeFailed $ T.pack err
                Right (_, _, resp) -> pure $ Right resp

-- | Receive exactly n bytes
recvExact :: Socket -> Int -> IO (Maybe BS.ByteString)
recvExact sock = go BS.empty
  where
    go acc 0 = pure (Just acc)
    go acc remaining = do
      chunk <- recv sock (min 4096 remaining)
      if BS.null chunk
        then pure Nothing
        else go (acc <> chunk) (remaining - BS.length chunk)
