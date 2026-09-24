{-# LANGUAGE OverloadedStrings #-}

-- | Streaming QGA command execution using a guest-side log file on POSIX.
module Corvus.Node.GuestAgent.Exec.Stream
  ( guestExecStream
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Corvus.Node.GuestAgent.Connection (GuestAgentConns, withPersistentConn)
import Corvus.Node.GuestAgent.Exec (detectGuestShell, parseExecStatus, parsePid, pollRecvTimeoutMicros, pollStatus)
import Corvus.Node.GuestAgent.Transport (decodeBase64Bytes, recvJson, sendJson)
import Corvus.Node.GuestAgent.Types
import Corvus.Qemu.Config (QemuConfig)
import Data.Aeson (Value, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.Socket (Socket)
import System.Timeout (timeout)

guestExecStream :: GuestAgentConns -> QemuConfig -> Int64 -> Text -> Maybe BS.ByteString -> Int -> ChunkSink -> ChunkSink -> IO GuestExecResult
guestExecStream conns config vmId command mStdin maxPolls onOut onErr = do
  let pollConnTimeoutMicros = max 15000000 (maxPolls * 100000 + 30000000)
  dispatched <- withPersistentConn conns config vmId 5 30000000 $ \sock -> do
    (shellPath, shellArgs) <- detectGuestShell sock
    if shellPath == "cmd.exe"
      then dispatchWindowsExec sock shellPath shellArgs command mStdin
      else dispatchLogTailExec sock shellPath shellArgs command mStdin
  case dispatched of
    Left err -> pure $ GuestExecConnectionFailed err
    Right (Left err) -> pure $ GuestExecError err
    Right (Right dispatchedExec) -> do
      result <- withPersistentConn conns config vmId 1 pollConnTimeoutMicros $ \sock ->
        case dispatchedExec of
          DispatchedWindows pid -> pollStatus sock pid 0 maxPolls onOut onErr
          DispatchedPosix pid logHandle -> do
            polled <- pollWithLogTail sock pid logHandle 0 maxPolls onOut
            _ <- guestFileClose sock logHandle
            pure polled
      pure $ either GuestExecConnectionFailed id result

data DispatchedExec
  = DispatchedPosix !Int !Int
  | DispatchedWindows !Int

dispatchLogTailExec :: Socket -> Text -> [Text] -> Text -> Maybe BS.ByteString -> IO (Either Text DispatchedExec)
dispatchLogTailExec sock shellPath shellArgs command mStdin = do
  let logPath = "/tmp/.corvus-build-step.log" :: Text
  truncated <- guestFileOpenForTruncate sock logPath
  case truncated of
    Left err -> pure $ Left ("guest-file-open(truncate): " <> err)
    Right handle -> do
      _ <- guestFileClose sock handle
      opened <- guestFileOpenForRead sock logPath
      case opened of
        Left err -> pure $ Left ("guest-file-open(read): " <> err)
        Right readHandle -> do
          let wrappedBody = "exec >>" <> shellQuote logPath <> " 2>&1\n" <> command
          execResponse <- dispatchExec sock shellPath (shellArgs ++ [wrappedBody]) mStdin
          case parsePid execResponse of
            Nothing -> do
              _ <- guestFileClose sock readHandle
              pure $ Left $ "Failed to parse guest-exec response " <> T.pack (show execResponse)
            Just pid -> pure $ Right (DispatchedPosix pid readHandle)

dispatchWindowsExec :: Socket -> Text -> [Text] -> Text -> Maybe BS.ByteString -> IO (Either Text DispatchedExec)
dispatchWindowsExec sock shellPath shellArgs command mStdin = do
  execResponse <- dispatchExec sock shellPath (shellArgs ++ [command]) mStdin
  pure $
    case parsePid execResponse of
      Nothing -> Left $ "Failed to parse guest-exec response " <> T.pack (show execResponse)
      Just pid -> Right $ DispatchedWindows pid

dispatchExec :: Socket -> Text -> [Text] -> Maybe BS.ByteString -> IO (Maybe Value)
dispatchExec sock shellPath args mStdin = do
  let stdinField = case mStdin of
        Nothing -> []
        Just bytes -> ["input-data" .= decodeUtf8 (B64.encode bytes)]
  sendJson sock $
    Aeson.object
      [ "execute" .= ("guest-exec" :: Text)
      , "arguments" .= Aeson.object (["path" .= shellPath, "arg" .= args, "capture-output" .= True] ++ stdinField)
      ]
  recvJson sock

pollWithLogTail :: Socket -> Int -> Int -> Int -> Int -> ChunkSink -> IO GuestExecResult
pollWithLogTail sock pid logHandle attempts maxAttempts onOut
  | attempts > maxAttempts = pure $ GuestExecError "guest-exec timed out waiting for process to exit"
  | otherwise = do
      drainLog sock logHandle onOut
      sendJson sock $ Aeson.object ["execute" .= ("guest-exec-status" :: Text), "arguments" .= Aeson.object ["pid" .= pid]]
      mResponse <- timeout pollRecvTimeoutMicros (recvJson sock)
      response <- maybe (ioError (userError "guest agent stopped responding mid-exec")) pure mResponse
      case parseExecStatus response of
        Just (True, exitcode, _, _) -> do
          drainLog sock logHandle onOut
          pure $ GuestExecSuccess exitcode T.empty T.empty
        Just (False, _, _, _) -> threadDelay 100000 >> pollWithLogTail sock pid logHandle (attempts + 1) maxAttempts onOut
        Nothing -> pure $ GuestExecError "Failed to parse guest-exec-status response"

drainLog :: Socket -> Int -> ChunkSink -> IO ()
drainLog sock logHandle onOut = do
  bytes <- collect BS.empty
  unless (BS.null bytes) (onOut bytes)
  where
    collect accumulated = do
      result <- guestFileRead sock logHandle 65536
      case result of
        Left _ -> pure accumulated
        Right (bytes, eof) ->
          let accumulated' = accumulated <> bytes
           in if eof then pure accumulated' else collect accumulated'

guestFileOpenForTruncate :: Socket -> Text -> IO (Either Text Int)
guestFileOpenForTruncate = guestFileOpenMode "w+"

guestFileOpenForRead :: Socket -> Text -> IO (Either Text Int)
guestFileOpenForRead = guestFileOpenMode "r"

guestFileOpenMode :: Text -> Socket -> Text -> IO (Either Text Int)
guestFileOpenMode mode sock path = do
  sendJson sock $ Aeson.object ["execute" .= ("guest-file-open" :: Text), "arguments" .= Aeson.object ["path" .= path, "mode" .= mode]]
  response <- recvJson sock
  pure $ case response >>= AT.parseMaybe (AT.withObject "response" $ \object -> object .: "return") of
    Just handle -> Right handle
    Nothing -> Left ("guest-file-open: " <> T.pack (show response))

guestFileRead :: Socket -> Int -> Int -> IO (Either Text (BS.ByteString, Bool))
guestFileRead sock handle count = do
  sendJson sock $ Aeson.object ["execute" .= ("guest-file-read" :: Text), "arguments" .= Aeson.object ["handle" .= handle, "count" .= count]]
  mResponse <- timeout pollRecvTimeoutMicros (recvJson sock)
  response <- maybe (ioError (userError "guest agent stopped responding mid-exec")) pure mResponse
  pure $ case response >>= AT.parseMaybe parser of
    Just result -> Right result
    Nothing -> Left ("guest-file-read: " <> T.pack (show response))
  where
    parser = AT.withObject "response" $ \object -> do
      returned <- object .: "return"
      encoded <- returned .:? "buf-b64" AT..!= ""
      eof <- returned .:? "eof" AT..!= False
      pure (decodeBase64Bytes encoded, eof)

guestFileClose :: Socket -> Int -> IO (Either Text ())
guestFileClose sock handle = do
  sendJson sock $ Aeson.object ["execute" .= ("guest-file-close" :: Text), "arguments" .= Aeson.object ["handle" .= handle]]
  _ <- recvJson sock
  pure (Right ())

shellQuote :: Text -> Text
shellQuote text = "'" <> T.replace "'" "'\\''" text <> "'"
