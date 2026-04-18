{-# LANGUAGE OverloadedStrings #-}

-- | JSON marshalling helpers for the Python-facing FFI boundary.
--
-- The heavy lifting — parsing a 'Request' and serialising a 'Response' —
-- is done by the 'FromJSON' / 'ToJSON' instances in "Corvus.Protocol".
-- This module only contains the envelope types, a small error shape,
-- and the shared 'ConnectionError' → 'CallError' translation.
module Corvus.Python.Marshal
  ( OneShotEnvelope (..)
  , Transport (..)
  , CallError (..)
  , decodeOneShot
  , decodeRequestOnly
  , decodeTransport
  , encodeOk
  , encodeErr
  , encodeEnvelopeError
  , connToError
  )
where

import Corvus.Client.Connection (ConnectionError (..))
import Corvus.Protocol (Request)
import Data.Aeson (FromJSON (..), Value, eitherDecodeStrict, encode, object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)

-- | Transport for the one-shot entry point.
data Transport
  = TransportUnix !FilePath
  | TransportTcp !String !Int
  deriving (Eq, Show)

instance FromJSON Transport where
  parseJSON = withObject "Transport" $ \o -> do
    munix <- o .:? "unix"
    mtcp <- o .:? "tcp"
    case (munix, mtcp) of
      (Just p, Nothing) -> pure (TransportUnix p)
      (Nothing, Just (host, port)) -> pure (TransportTcp host port)
      (Just _, Just _) -> fail "transport must have exactly one of 'unix' or 'tcp'"
      (Nothing, Nothing) -> fail "transport missing 'unix' or 'tcp'"

-- | Envelope for one-shot calls (via 'corvusCall'). Wraps a transport
-- and an embedded 'Request' (which uses its own FromJSON for decoding).
data OneShotEnvelope = OneShotEnvelope
  { oseTransport :: !Transport
  , oseRequest :: !Request
  }
  deriving (Eq, Show)

instance FromJSON OneShotEnvelope where
  parseJSON = withObject "OneShotEnvelope" $ \o -> do
    tr <- o .: "transport"
    req <- o .: "request"
    pure (OneShotEnvelope tr req)

-- | Structured error surfaced to Python. 'ceKind' drives exception class
-- selection on the Python side; 'ceDetails' is surfaced as exception args.
data CallError = CallError
  { ceKind :: !Text
  , ceDetails :: !Value
  }
  deriving (Eq, Show)

-- | Parse a one-shot envelope (transport + request).
decodeOneShot :: BS.ByteString -> Either String OneShotEnvelope
decodeOneShot = eitherDecodeStrict

-- | Parse a bare 'Request' (used by the persistent-connection entry —
-- no transport needed).
decodeRequestOnly :: BS.ByteString -> Either String Request
decodeRequestOnly = eitherDecodeStrict

-- | Parse a bare 'Transport' (used when opening a persistent connection).
decodeTransport :: BS.ByteString -> Either String Transport
decodeTransport bs = do
  v <- eitherDecodeStrict bs
  AT.parseEither parseJSON v

-- | @{"ok": value}@
encodeOk :: Value -> BS.ByteString
encodeOk v = BL.toStrict (encode (object [("ok", v)]))

-- | @{"err": {"kind": ..., "details": ...}}@
encodeErr :: CallError -> BS.ByteString
encodeErr (CallError k d) =
  BL.toStrict (encode (object [("err", object [("kind", AT.toJSON k), ("details", d)])]))

-- | Envelope-level error (malformed input).
encodeEnvelopeError :: Text -> BS.ByteString
encodeEnvelopeError msg =
  encodeErr (CallError "bad_envelope" (object ["message" .= msg]))

-- | Translate a 'ConnectionError' to the structured 'CallError' shape
-- the Python layer expects. Used by both the one-shot and persistent
-- call paths.
connToError :: ConnectionError -> CallError
connToError (DecodeFailed t) = CallError "protocol" (object ["message" .= t])
connToError (ConnectFailed t) = CallError "connect" (object ["message" .= t])
connToError (SendFailed t) = CallError "connect" (object ["message" .= t])
connToError (RecvFailed t) = CallError "connect" (object ["message" .= t])
connToError (ServerError t) = CallError "server" (object ["message" .= t])
