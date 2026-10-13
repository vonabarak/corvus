{-# LANGUAGE OverloadedStrings #-}

-- | Binary guest file uploads, independent of guest shells and stdin pipes.
module Corvus.Node.GuestAgent.File (guestWriteFile, writeFileOnSocket) where

import Control.Exception (bracket)
import Control.Monad (join)
import Corvus.Node.GuestAgent.Connection (GuestAgentConns, withPersistentConn)
import Corvus.Node.GuestAgent.Transport (recvJson, sendJson)
import Corvus.Qemu.Config (QemuConfig)
import Data.Aeson (Value, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Socket (Socket)
import System.Timeout (timeout)

guestWriteFile :: GuestAgentConns -> QemuConfig -> Int64 -> Text -> BS.ByteString -> IO (Either Text ())
guestWriteFile conns config vmId path bytes =
  withPersistentConn conns config vmId 1 60000000 $ \sock -> writeFileOnSocket sock path bytes

-- | Validate every acknowledged byte count, including short writes. Handles
-- are closed on failure; an interrupted upload is never reported as complete.
writeFileOnSocket :: Socket -> Text -> BS.ByteString -> IO ()
writeFileOnSocket sock path bytes =
  bracket
    (request "guest-file-open" ["path" .= path, "mode" .= ("wb" :: Text)] AT.parseJSON)
    (\handle -> request "guest-file-close" ["handle" .= (handle :: Int64)] (const (pure ())))
    ( \handle -> do
        writeAll handle bytes
        request "guest-file-flush" ["handle" .= handle] (const (pure ()))
    )
  where
    writeAll _ remaining | BS.null remaining = pure ()
    writeAll handle remaining = do
      let chunk = BS.take 32768 remaining
      count <- request "guest-file-write" ["handle" .= handle, "buf-b64" .= TE.decodeUtf8 (B64.encode chunk)] $ AT.withObject "GuestFileWrite" (.: "count")
      if count <= 0 || count > BS.length chunk
        then ioError (userError "guest-file-write: invalid byte count")
        else writeAll handle (BS.drop count remaining)
    request :: Text -> [Pair] -> (Value -> AT.Parser a) -> IO a
    request command args parser = do
      sendJson sock $ Aeson.object ["execute" .= (command :: Text), "arguments" .= Aeson.object args]
      response <- timeout 5000000 (recvJson sock)
      let parseReturn = AT.withObject "QGA reply" $ \object -> object .: "return" >>= parser
      case join response >>= AT.parseMaybe parseReturn of
        Just value -> pure value
        Nothing -> ioError (userError (T.unpack command <> ": failed or malformed QGA reply"))
