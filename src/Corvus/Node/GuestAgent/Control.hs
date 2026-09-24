{-# LANGUAGE OverloadedStrings #-}

module Corvus.Node.GuestAgent.Control
  ( guestPing
  , guestShutdown
  , guestFsFreeze
  , guestFsThaw
  , guestSetTime
  )
where

import Corvus.Node.GuestAgent.Connection (GuestAgentConns, withPersistentConn)
import Corvus.Node.GuestAgent.Exec (pollRecvTimeoutMicros)
import Corvus.Node.GuestAgent.Transport (recvJson, recvJsonWithin, sendJson)
import Corvus.Qemu.Config (QemuConfig)
import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

-- | Ping is deliberately single-attempt: callers already poll. Its bounded
-- receive prevents a dead QGA from delaying the health-check cadence.
guestPing :: GuestAgentConns -> QemuConfig -> Int64 -> IO Bool
guestPing conns config vmId = do
  result <- withPersistentConn conns config vmId 1 15000000 $ \sock -> do
    sendJson sock $ Aeson.object ["execute" .= ("guest-ping" :: Text)]
    response <- recvJsonWithin pollRecvTimeoutMicros sock "guest-ping"
    pure $ case response of
      Just (Object object) -> KM.member "return" object
      _ -> False
  pure $ either (const False) id result

-- | Request a graceful in-guest powerdown. QGA normally closes before
-- replying, so no reply within the short window is accepted as success.
guestShutdown :: GuestAgentConns -> QemuConfig -> Int64 -> IO Bool
guestShutdown conns config vmId = do
  result <- withPersistentConn conns config vmId 3 15000000 $ \sock -> do
    sendJson sock $ Aeson.object ["execute" .= ("guest-shutdown" :: Text), "arguments" .= Aeson.object ["mode" .= ("powerdown" :: Text)]]
    response <- timeout 3000000 (recvJson sock)
    pure $ case response of
      Just (Just (Object object)) -> not (KM.member "error" object)
      _ -> True
  pure $ either (const True) id result

-- | Freeze writable guest filesystems. Every caller must arrange a matching
-- 'guestFsThaw' with bracket-style finalisation: leaving the guest frozen
-- wedges its I/O. A zero result means no writable filesystem supports freeze.
guestFsFreeze :: GuestAgentConns -> QemuConfig -> Int64 -> IO (Either Text Int)
guestFsFreeze = fsCommand "guest-fsfreeze-freeze"

-- | Thaw previously frozen filesystems. QGA makes this idempotent, so it is
-- safe to invoke on all snapshot failure paths.
guestFsThaw :: GuestAgentConns -> QemuConfig -> Int64 -> IO (Either Text Int)
guestFsThaw = fsCommand "guest-fsfreeze-thaw"

fsCommand :: Text -> GuestAgentConns -> QemuConfig -> Int64 -> IO (Either Text Int)
fsCommand command conns config vmId = do
  result <- withPersistentConn conns config vmId 1 10000000 $ \sock -> do
    sendJson sock $ Aeson.object ["execute" .= command]
    response <- recvJsonWithin 10000000 sock command
    pure $ case response of
      Just (Object object) -> case KM.lookup "return" object of
        Just (Number number) -> Right (truncate number)
        _ -> Left (qgaErrText object)
      _ -> Left (command <> ": malformed reply")
  pure $ either (Left . T.pack . show) id result

-- | Resync guest wall time from the host RTC after a vmstate restore.
-- Failure is intentionally reported to the caller rather than undoing restore.
guestSetTime :: GuestAgentConns -> QemuConfig -> Int64 -> IO (Either Text ())
guestSetTime conns config vmId = do
  result <- withPersistentConn conns config vmId 1 10000000 $ \sock -> do
    sendJson sock $ Aeson.object ["execute" .= ("guest-set-time" :: Text)]
    response <- recvJsonWithin 10000000 sock "guest-set-time"
    pure $ case response of
      Just (Object object) -> case KM.lookup "return" object of
        Just _ -> Right ()
        Nothing -> Left (qgaErrText object)
      _ -> Left "guest-set-time: malformed reply"
  pure $ either (Left . T.pack . show) id result

qgaErrText :: AT.Object -> Text
qgaErrText object = case KM.lookup "error" object of
  Just (Object err) ->
    let value key = case KM.lookup key err of Just (String text) -> text; _ -> ""
        desc = value "desc"
        cls = value "class"
     in if not (T.null cls) && not (T.null desc) then cls <> ": " <> desc else if not (T.null desc) then desc else "QGA error (no description)"
  _ -> "QGA reply lacks both 'return' and 'error' fields"
