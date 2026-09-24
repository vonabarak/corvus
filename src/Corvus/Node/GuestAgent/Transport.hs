{-# LANGUAGE OverloadedStrings #-}

-- | QGA's newline-delimited JSON transport and byte-level helpers.
module Corvus.Node.GuestAgent.Transport
  ( parseQgaFrame
  , sendJson
  , recvJson
  , recvJsonWithin
  , decodeBase64
  , decodeBase64Bytes
  )
where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)

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
recvJson :: Socket -> IO (Maybe Value)
recvJson sock = go BS.empty
  where
    go acc = do
      chunk <- recv sock 65536
      if BS.null chunk
        then pure $ if BS.null acc then Nothing else parseQgaFrame acc
        else
          let acc' = acc <> chunk
           in if BS.elem nlByte acc' then pure (parseQgaFrame acc') else go acc'

-- | Time out by throwing, so callers discard the socket rather than reusing a
-- connection whose eventual response would desynchronise the next request.
recvJsonWithin :: Int -> Socket -> Text -> IO (Maybe Value)
recvJsonWithin micros sock callLabel = do
  mResponse <- timeout micros (recvJson sock)
  case mResponse of
    Just response -> pure response
    Nothing -> ioError . userError $ T.unpack callLabel <> ": no reply within " <> show (micros `div` 1000000) <> "s"

decodeBase64 :: Text -> Text
decodeBase64 text
  | T.null text = ""
  | otherwise = case B64.decode (encodeUtf8 text) of
      Right decoded -> decodeUtf8With lenientDecode decoded
      Left _ -> text

decodeBase64Bytes :: Text -> BS.ByteString
decodeBase64Bytes text
  | T.null text = BS.empty
  | otherwise = case B64.decode (encodeUtf8 text) of
      Right decoded -> decoded
      Left _ -> encodeUtf8 text
