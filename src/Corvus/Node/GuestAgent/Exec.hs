{-# LANGUAGE OverloadedStrings #-}

-- | QGA command execution and status polling.
module Corvus.Node.GuestAgent.Exec
  ( guestExec
  , guestExecWithTimeout
  , guestExecWithStdin
  , splitLines
  , pollStatus
  , pollRecvTimeoutMicros
  , detectGuestShell
  , parsePid
  , parseExecStatus
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Corvus.Node.GuestAgent.Connection (GuestAgentConns, withPersistentConn)
import Corvus.Node.GuestAgent.Transport (decodeBase64Bytes, recvJson, sendJson)
import Corvus.Node.GuestAgent.Types
import Corvus.Qemu.Config (QemuConfig)
import Data.Aeson (Value, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8)
import Network.Socket (Socket)
import System.Timeout (timeout)

-- | Per-receive timeout for QGA commands whose stale reply must never be
-- reused by a later request.
pollRecvTimeoutMicros :: Int
pollRecvTimeoutMicros = 5000000

guestExec :: GuestAgentConns -> QemuConfig -> Int64 -> Text -> IO GuestExecResult
guestExec conns config vmId command = guestExecImpl conns config vmId command Nothing 600

guestExecWithTimeout :: GuestAgentConns -> QemuConfig -> Int64 -> Text -> Int -> IO GuestExecResult
guestExecWithTimeout conns config vmId command = guestExecImpl conns config vmId command Nothing

guestExecWithStdin :: GuestAgentConns -> QemuConfig -> Int64 -> Text -> BS.ByteString -> Int -> IO GuestExecResult
guestExecWithStdin conns config vmId command stdinBs = guestExecImpl conns config vmId command (Just stdinBs)

splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines bytes =
  let newline = fromIntegral (fromEnum '\n') :: Word8
      pieces = BS.split newline bytes
   in case reverse pieces of
        [] -> ([], BS.empty)
        (lastPiece : rest) -> (reverse rest, lastPiece)

guestExecImpl :: GuestAgentConns -> QemuConfig -> Int64 -> Text -> Maybe BS.ByteString -> Int -> IO GuestExecResult
guestExecImpl conns config vmId command mStdin maxPolls = do
  outRef <- newIORef BS.empty
  errRef <- newIORef BS.empty
  let onOut bytes = modifyIORef' outRef (<> bytes)
      onErr bytes = modifyIORef' errRef (<> bytes)
  result <- runGuestExecWithSinks conns config vmId command mStdin maxPolls onOut onErr
  case result of
    GuestExecSuccess code _ _ -> do
      outBytes <- readIORef outRef
      errBytes <- readIORef errRef
      pure $ GuestExecSuccess code (decodeUtf8With lenientDecode outBytes) (decodeUtf8With lenientDecode errBytes)
    other -> pure other

runGuestExecWithSinks :: GuestAgentConns -> QemuConfig -> Int64 -> Text -> Maybe BS.ByteString -> Int -> ChunkSink -> ChunkSink -> IO GuestExecResult
runGuestExecWithSinks conns config vmId command mStdin maxPolls onOut onErr = do
  let pollConnTimeoutMicros = max 15000000 (maxPolls * 100000 + 30000000)
  dispatched <- withPersistentConn conns config vmId 5 30000000 $ \sock -> do
    (shellPath, shellArgs) <- detectGuestShell sock
    let stdinField = case mStdin of
          Nothing -> []
          Just bytes -> ["input-data" .= decodeUtf8 (B64.encode bytes)]
    sendJson sock $
      Aeson.object
        [ "execute" .= ("guest-exec" :: Text)
        , "arguments" .= Aeson.object (["path" .= shellPath, "arg" .= (shellArgs ++ [command]), "capture-output" .= True] ++ stdinField)
        ]
    response <- recvJson sock
    pure $ maybe (Left ("Failed to parse guest-exec response: " <> T.pack (show response))) Right (parsePid response)
  case dispatched of
    Left err -> pure $ GuestExecConnectionFailed err
    Right (Left err) -> pure $ GuestExecError err
    Right (Right pid) -> do
      result <- withPersistentConn conns config vmId 1 pollConnTimeoutMicros $ \sock ->
        pollStatus sock pid 0 maxPolls onOut onErr
      pure $ either GuestExecConnectionFailed id result

detectGuestShell :: Socket -> IO (Text, [Text])
detectGuestShell sock = do
  sendJson sock $ Aeson.object ["execute" .= ("guest-get-osinfo" :: Text)]
  response <- recvJson sock
  case response >>= parseOsId of
    Just osId | "mswindows" `T.isPrefixOf` osId -> pure ("cmd.exe", ["/c"])
    _ -> pure ("/bin/sh", ["-c"])
  where
    parseOsId value =
      AT.parseMaybe
        ( AT.withObject "response" $ \object -> do
            returned <- object .: "return"
            returned .:? "id" AT..!= ("" :: Text)
        )
        value

parsePid :: Maybe Value -> Maybe Int
parsePid mValue = mValue >>= AT.parseMaybe (AT.withObject "response" $ \object -> object .: "return" >>= (.: "pid"))

pollStatus :: Socket -> Int -> Int -> Int -> ChunkSink -> ChunkSink -> IO GuestExecResult
pollStatus sock pid attempts maxAttempts onOut onErr
  | attempts > maxAttempts = pure $ GuestExecError "guest-exec timed out waiting for process to exit"
  | otherwise = do
      sendJson sock $ Aeson.object ["execute" .= ("guest-exec-status" :: Text), "arguments" .= Aeson.object ["pid" .= pid]]
      mResponse <- timeout pollRecvTimeoutMicros (recvJson sock)
      response <- maybe (ioError (userError "guest agent stopped responding mid-exec")) pure mResponse
      case parseExecStatus response of
        Just (exited, exitcode, outBytes, errBytes) -> do
          unless (BS.null outBytes) (onOut outBytes)
          unless (BS.null errBytes) (onErr errBytes)
          if exited
            then pure $ GuestExecSuccess exitcode T.empty T.empty
            else threadDelay 100000 >> pollStatus sock pid (attempts + 1) maxAttempts onOut onErr
        Nothing -> pure $ GuestExecError "Failed to parse guest-exec-status response"

parseExecStatus :: Maybe Value -> Maybe (Bool, Int, BS.ByteString, BS.ByteString)
parseExecStatus mValue =
  mValue
    >>= AT.parseMaybe
      ( AT.withObject "response" $ \object -> do
          returned <- object .: "return"
          exited <- returned .: "exited"
          exitcode <- returned .:? "exitcode" AT..!= 1
          outData <- returned .:? "out-data" AT..!= ""
          errData <- returned .:? "err-data" AT..!= ""
          pure (exited, exitcode, decodeBase64Bytes outData, decodeBase64Bytes errData)
      )
