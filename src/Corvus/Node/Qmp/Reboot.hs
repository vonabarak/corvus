{-# LANGUAGE OverloadedStrings #-}

-- | A scoped QMP observer established before dispatching a guest reboot.
module Corvus.Node.Qmp.Reboot (withRebootObserver, isGuestRebootEvent) where

import Corvus.Node.Qmp.Transport (withUnixSocket)
import Corvus.Node.Runtime (getQmpSocket)
import Corvus.Qemu.Config (QemuConfig)
import Data.Aeson (Value (..), eitherDecodeStrict', (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Text (Text)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

-- | True means QEMU exits for rebootQuirk; False means an in-process reset.
isGuestRebootEvent :: Value -> Maybe Bool
isGuestRebootEvent (Object fields) = case KM.lookup "data" fields of
  Just (Object eventData) -> case KM.lookup "event" fields of
    Just (String "RESET") | KM.lookup "guest" eventData == Just (Bool True) -> Just False
    Just (String "SHUTDOWN") | KM.lookup "reason" eventData == Just (String "guest-reset") -> Just True
    _ -> Nothing
  _ -> Nothing
isGuestRebootEvent _ = Nothing

withRebootObserver :: QemuConfig -> Int64 -> (IO Bool -> IO a) -> IO a
withRebootObserver config vmId action = do
  path <- getQmpSocket config vmId
  withUnixSocket path $ \sock -> do
    buffer <- newIORef BS.empty
    _ <- nextFrame sock buffer
    sendAll sock $ LBS.toStrict (A.encode (A.object ["execute" .= ("qmp_capabilities" :: Text)])) <> "\n"
    negotiate sock buffer
    action (awaitReboot sock buffer)
  where
    negotiate sock buffer = do
      ready <- nextFrame sock buffer
      case ready of
        Object fields | KM.member "return" fields -> pure ()
        Object fields | KM.member "event" fields -> negotiate sock buffer
        _ -> ioError (userError "QMP capability negotiation failed")
    awaitReboot sock buffer = do
      frame <- nextFrame sock buffer
      maybe (awaitReboot sock buffer) pure (isGuestRebootEvent frame)

-- Keep unread frames: RESET can share a packet with a command reply.
nextFrame :: Socket -> IORef BS.ByteString -> IO Value
nextFrame sock buffer = do
  bytes <- readIORef buffer
  case BS.elemIndex 10 bytes of
    Just offset -> do
      writeIORef buffer (BS.drop (offset + 1) bytes)
      either (ioError . userError) pure (eitherDecodeStrict' (BS.take offset bytes))
    Nothing -> do
      chunk <- recv sock 4096
      if BS.null chunk
        then ioError (userError "QMP closed before guest reboot confirmation")
        else writeIORef buffer (bytes <> chunk) >> nextFrame sock buffer
