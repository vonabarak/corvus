{-# LANGUAGE OverloadedStrings #-}

-- | VM audio devices. Changes take effect at the next VM start.
module Corvus.Handlers.AudioDevice
  ( AudioDeviceAdd (..)
  , AudioDeviceEdit (..)
  , AudioDeviceRemove (..)
  , handleAudioDeviceList
  , validateAudioOptions
  ) where

import Corvus.Action (Action (..), ActionContext (..))
import Corvus.Model
import Corvus.Protocol
import Corvus.Types (ServerState (..))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (SelectOpt (Asc), delete, get, insert, selectList, update, (=.), (==.))
import Database.Persist.Sql (runSqlPool)

data AudioDeviceAdd = AudioDeviceAdd Int64 AudioBackend AudioDeviceModel Text
data AudioDeviceEdit = AudioDeviceEdit Int64 Int64 AudioBackend (Maybe AudioDeviceModel) Text
data AudioDeviceRemove = AudioDeviceRemove Int64 Int64

instance Action AudioDeviceAdd where
  actionSubsystem _ = SubVm
  actionCommand _ = "add-audio-device"
  actionEntityId (AudioDeviceAdd vmId _ _ _) = Just (fromIntegral vmId)
  actionExecute ctx (AudioDeviceAdd vmId backend model options) =
    changeAudioDevice (acState ctx) vmId backend (Just model) options Nothing

instance Action AudioDeviceEdit where
  actionSubsystem _ = SubVm
  actionCommand _ = "edit-audio-device"
  actionEntityId (AudioDeviceEdit vmId _ _ _ _) = Just (fromIntegral vmId)
  actionExecute ctx (AudioDeviceEdit vmId audioId backend model options) =
    changeAudioDevice (acState ctx) vmId backend model options (Just audioId)

instance Action AudioDeviceRemove where
  actionSubsystem _ = SubVm
  actionCommand _ = "remove-audio-device"
  actionEntityId (AudioDeviceRemove vmId _) = Just (fromIntegral vmId)
  actionExecute ctx (AudioDeviceRemove vmId audioId) = do
    let pool = ssDbPool (acState ctx)
        vmKey = toSqlKey vmId :: VmId
        audioKey = toSqlKey audioId :: AudioDeviceId
    mVm <- runSqlPool (get vmKey) pool
    case mVm of
      Nothing -> pure RespVmNotFound
      Just vm | vmStatus vm /= VmStopped -> pure RespVmMustBeStopped
      Just _ -> do
        mAudio <- runSqlPool (get audioKey) pool
        case mAudio of
          Just audio | audioDeviceVmId audio == vmKey -> do
            runSqlPool (delete audioKey) pool
            pure RespAudioDeviceOk
          _ -> pure RespAudioDeviceNotFound

changeAudioDevice :: ServerState -> Int64 -> AudioBackend -> Maybe AudioDeviceModel -> Text -> Maybe Int64 -> IO Response
changeAudioDevice state vmId backend mModel options mAudioId =
  case validateAudioOptions options of
    Left err -> pure (RespError err)
    Right () -> do
      let pool = ssDbPool state
          vmKey = toSqlKey vmId :: VmId
      mVm <- runSqlPool (get vmKey) pool
      case mVm of
        Nothing -> pure RespVmNotFound
        Just vm | vmStatus vm /= VmStopped -> pure RespVmMustBeStopped
        Just vm | backend == AudioSpice && vmHeadless vm -> pure (RespError "SPICE audio requires a graphical VM")
        Just _ -> case mAudioId of
          Nothing -> do
            audioId <- runSqlPool (insert (AudioDevice vmKey backend (fromMaybe AudioVirtioSound mModel) options)) pool
            pure (RespAudioDeviceAdded (fromSqlKey audioId))
          Just audioId -> do
            let audioKey = toSqlKey audioId :: AudioDeviceId
            mAudio <- runSqlPool (get audioKey) pool
            case mAudio of
              Just audio | audioDeviceVmId audio == vmKey -> do
                runSqlPool (update audioKey ([AudioDeviceBackend =. backend, AudioDeviceOptions =. options] ++ maybe [] (\model -> [AudioDeviceModel =. model]) mModel)) pool
                pure RespAudioDeviceOk
              _ -> pure RespAudioDeviceNotFound

handleAudioDeviceList :: ServerState -> Int64 -> IO Response
handleAudioDeviceList state vmId = do
  let pool = ssDbPool state
      vmKey = toSqlKey vmId :: VmId
  mVm <- runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just _ -> do
      devices <- runSqlPool (selectList [AudioDeviceVmId ==. vmKey] [Asc AudioDeviceId]) pool
      pure $
        RespAudioDeviceList
          [ AudioDeviceInfo (fromSqlKey audioId) (audioDeviceBackend audioDevice) (audioDeviceModel audioDevice) (audioDeviceOptions audioDevice)
          | Entity audioId audioDevice <- devices
          ]

validateAudioOptions :: Text -> Either Text ()
validateAudioOptions options
  | T.null options = Right ()
  | otherwise = mapM_ validatePair (T.splitOn "," options)
  where
    validatePair pair = case T.breakOn "=" pair of
      (key, value)
        | T.null value -> Left "Audio options must be comma-separated key=value pairs"
        | key == "id" || key == "driver" -> Left "Audio options cannot override id or driver"
        | T.null key || not (T.all validKeyChar key) -> Left "Invalid audio option name"
        | T.any (`elem` ['\NUL', '\n', '\r']) value -> Left "Invalid audio option value"
        | otherwise -> Right ()
    validKeyChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("._-" :: String)
