{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cloud-init configuration handlers.
-- Handles CRUD operations for custom cloud-init configs per VM.
module Corvus.Handlers.CloudInit
  ( handleCloudInitSet
  , handleCloudInitGet
  , handleCloudInitDelete
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN)
import Corvus.Handlers.SshKey (regenerateCloudInitIso)
import Corvus.Model
import Corvus.Protocol
import Corvus.Types
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (toSqlKey)

-- | Set or update cloud-init config for a VM
handleCloudInitSet :: ServerState -> Int64 -> Maybe Text -> Maybe Text -> Bool -> IO Response
handleCloudInitSet state vmId mUserData mNetworkConfig injectKeys = runServerLogging state $ do
  logInfoN $ "Setting cloud-init config for VM " <> T.pack (show vmId)

  let pool = ssDbPool state
  let vmKey = toSqlKey vmId :: VmId

  -- Check VM exists and has cloud-init enabled
  mVm <- liftIO $ runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm
      | not (vmCloudInit vm) ->
          pure $ RespError "Cloud-init is not enabled on this VM"
      | otherwise -> do
          -- Upsert the cloud-init config
          liftIO $
            runSqlPool
              ( upsertBy
                  (UniqueCloudInitVm vmKey)
                  CloudInit
                    { cloudInitVmId = vmKey
                    , cloudInitUserData = mUserData
                    , cloudInitNetworkConfig = mNetworkConfig
                    , cloudInitInjectSshKeys = injectKeys
                    }
                  [ CloudInitUserData =. mUserData
                  , CloudInitNetworkConfig =. mNetworkConfig
                  , CloudInitInjectSshKeys =. injectKeys
                  ]
              )
              pool
          -- Regenerate ISO with new config
          ciResult <-
            liftIO $
              regenerateCloudInitIso
                (ssQemuConfig state)
                pool
                vmId
                (vmName vm)
                (ssLogLevel state)
                Nothing
          case ciResult of
            Left err -> pure $ RespError $ "Cloud-init ISO regeneration failed: " <> err
            Right _ -> pure RespCloudInitOk

-- | Get cloud-init config for a VM
handleCloudInitGet :: ServerState -> Int64 -> IO Response
handleCloudInitGet state vmId = do
  let pool = ssDbPool state
  let vmKey = toSqlKey vmId :: VmId

  mVm <- runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just _ -> do
      mConfig <- runSqlPool (getBy (UniqueCloudInitVm vmKey)) pool
      case mConfig of
        Nothing -> pure $ RespCloudInitConfig Nothing
        Just (Entity _ ci) ->
          pure $
            RespCloudInitConfig $
              Just
                CloudInitInfo
                  { ciiUserData = cloudInitUserData ci
                  , ciiNetworkConfig = cloudInitNetworkConfig ci
                  , ciiInjectSshKeys = cloudInitInjectSshKeys ci
                  }

-- | Delete custom cloud-init config for a VM (revert to defaults)
handleCloudInitDelete :: ServerState -> Int64 -> IO Response
handleCloudInitDelete state vmId = runServerLogging state $ do
  logInfoN $ "Deleting cloud-init config for VM " <> T.pack (show vmId)

  let pool = ssDbPool state
  let vmKey = toSqlKey vmId :: VmId

  mVm <- liftIO $ runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm
      | not (vmCloudInit vm) ->
          pure $ RespError "Cloud-init is not enabled on this VM"
      | otherwise -> do
          -- Delete the custom config
          liftIO $ runSqlPool (deleteBy (UniqueCloudInitVm vmKey)) pool
          -- Regenerate ISO with defaults
          ciResult <-
            liftIO $
              regenerateCloudInitIso
                (ssQemuConfig state)
                pool
                vmId
                (vmName vm)
                (ssLogLevel state)
                Nothing
          case ciResult of
            Left err -> pure $ RespError $ "Cloud-init ISO regeneration failed: " <> err
            Right _ -> pure RespCloudInitOk
