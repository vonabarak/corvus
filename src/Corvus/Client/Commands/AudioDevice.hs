{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Corvus.Client.Commands.AudioDevice
  ( handleAudioDeviceAdd
  , handleAudioDeviceEdit
  , handleAudioDeviceRemove
  , handleAudioDeviceList
  ) where

import Control.Exception (SomeException, try)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output (Align (..), Column (..), TableOpts, emitError, emitOk, emitOkWith, emitResult, emitRpcError, printTable)
import Corvus.Client.Types (OutputFormat)
import Corvus.Model (AudioBackend, EnumText (..))
import Corvus.Protocol.Vm (AudioDeviceInfo (..))
import Corvus.Wire.Common (entityRefFromText)
import Data.Aeson (toJSON)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T

parseBackend :: OutputFormat -> Text -> IO (Maybe AudioBackend)
parseBackend fmt value = case enumFromText value of
  Right backend -> pure (Just backend)
  Left err -> do
    emitError fmt "invalid_audio_backend" err $ putStrLn (T.unpack err)
    pure Nothing

handleAudioDeviceAdd :: OutputFormat -> CapnpConnection -> Text -> Text -> Text -> IO Bool
handleAudioDeviceAdd fmt conn vmRef backendText options = do
  mBackend <- parseBackend fmt backendText
  case mBackend of
    Nothing -> pure False
    Just backend -> do
      result <- try @SomeException (CR.rpcAudioDeviceAdd conn (entityRefFromText vmRef) backend options)
      case result of
        Right aid -> emitOkWith fmt [("id", toJSON aid)] (putStrLn ("Audio device added with ID: " ++ show aid)) >> pure True
        Left err -> emitRpcError fmt err (print err) >> pure False

handleAudioDeviceEdit :: OutputFormat -> CapnpConnection -> Text -> Int64 -> Text -> Text -> IO Bool
handleAudioDeviceEdit fmt conn vmRef aid backendText options = do
  mBackend <- parseBackend fmt backendText
  case mBackend of
    Nothing -> pure False
    Just backend -> do
      result <- try @SomeException (CR.rpcAudioDeviceEdit conn (entityRefFromText vmRef) aid backend options)
      case result of
        Right () -> emitOk fmt (putStrLn "Audio device updated.") >> pure True
        Left err -> emitRpcError fmt err (print err) >> pure False

handleAudioDeviceRemove :: OutputFormat -> CapnpConnection -> Text -> Int64 -> IO Bool
handleAudioDeviceRemove fmt conn vmRef aid = do
  result <- try @SomeException (CR.rpcAudioDeviceRemove conn (entityRefFromText vmRef) aid)
  case result of
    Right () -> emitOk fmt (putStrLn "Audio device removed.") >> pure True
    Left err -> emitRpcError fmt err (print err) >> pure False

handleAudioDeviceList :: OutputFormat -> TableOpts -> CapnpConnection -> Text -> IO Bool
handleAudioDeviceList fmt tableOpts conn vmRef = do
  result <- try @SomeException (CR.rpcAudioDeviceList conn (entityRefFromText vmRef))
  case result of
    Right devices -> do
      emitResult fmt devices $ if null devices then putStrLn "No audio devices found for this VM." else printTable tableOpts columns devices
      pure True
    Left err -> emitRpcError fmt err (print err) >> pure False
  where
    columns :: [Column AudioDeviceInfo]
    columns =
      [ Column "ID" RightAlign (show . adiId)
      , Column "BACKEND" LeftAlign (T.unpack . enumToText . adiBackend)
      , Column "OPTIONS" LeftAlign (T.unpack . adiOptions)
      ]
