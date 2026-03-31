{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Shared directory management handlers.
-- Handles adding, removing, and listing shared directories for VMs.
module Corvus.Handlers.SharedDir
  ( -- * Handlers
    handleSharedDirAdd
  , handleSharedDirRemove
  , handleSharedDirList
  )
where

import Control.Monad.Logger (logDebugN, logInfoN)
import Corvus.Handlers.Resolve (validateName)
import Corvus.Model
import Corvus.Protocol
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Qemu.Virtiofsd (startVirtiofsdProcesses)
import Corvus.Types
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

--------------------------------------------------------------------------------
-- Shared Directory Handlers
--------------------------------------------------------------------------------

-- | Add a shared directory to a VM
handleSharedDirAdd
  :: ServerState
  -> Int64
  -> Text
  -> Text
  -> SharedDirCache
  -> Bool
  -> IO Response
handleSharedDirAdd state vmId path tag cache readOnly =
  case validateName "Shared directory tag" tag of
    Left err -> pure $ RespError err
    Right () -> do
      runServerLogging state $ logInfoN $ "Adding shared directory to VM " <> T.pack (show vmId) <> ": " <> path

      let pool = ssDbPool state
      let vmKey = toSqlKey vmId :: VmId

      -- Check VM exists
      mVm <- runSqlPool (get vmKey) pool
      case mVm of
        Nothing -> pure RespVmNotFound
        Just vm -> do
          -- Check for duplicate tag
          existingTags <- runSqlPool (selectList [SharedDirVmId ==. vmKey, SharedDirTag ==. tag] []) pool
          case existingTags of
            (_ : _) -> pure $ RespError $ "Tag '" <> tag <> "' already exists for this VM"
            [] -> do
              -- Insert shared directory
              let sharedDir =
                    SharedDir
                      { sharedDirVmId = vmKey
                      , sharedDirPath = path
                      , sharedDirTag = tag
                      , sharedDirCache = cache
                      , sharedDirReadOnly = readOnly
                      , sharedDirPid = Nothing
                      }
              dirId <- runSqlPool (insert sharedDir) pool
              let dirIdInt = fromSqlKey dirId

              runServerLogging state $ logInfoN $ "Shared directory added with ID: " <> T.pack (show dirIdInt)

              -- If VM is running, start virtiofsd for this directory
              case vmStatus vm of
                VmRunning -> do
                  runServerLogging state $ logInfoN "VM is running, starting virtiofsd..."
                  _ <-
                    runServerLogging state $
                      startVirtiofsdProcesses pool (ssQemuConfig state) vmId
                  pure () -- Log result but continue
                _ -> pure ()

              pure $ RespSharedDirAdded dirIdInt

-- | Remove a shared directory from a VM
handleSharedDirRemove :: ServerState -> Int64 -> Int64 -> IO Response
handleSharedDirRemove state vmId sharedDirId = do
  runServerLogging state $ logInfoN $ "Removing shared directory " <> T.pack (show sharedDirId) <> " from VM " <> T.pack (show vmId)

  let pool = ssDbPool state
  let vmKey = toSqlKey vmId :: VmId
  let dirKey = toSqlKey sharedDirId :: SharedDirId

  -- Check VM exists
  mVm <- runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm -> do
      -- Check shared dir exists and belongs to this VM
      mDir <- runSqlPool (get dirKey) pool
      case mDir of
        Nothing -> pure RespSharedDirNotFound
        Just dir ->
          if sharedDirVmId dir /= vmKey
            then pure RespSharedDirNotFound
            else do
              -- If VM is running, we need to kill the virtiofsd process
              case vmStatus vm of
                VmRunning -> do
                  runServerLogging state $ logInfoN "VM is running, cannot remove shared directory"
                  pure RespVmMustBeStopped
                _ -> do
                  -- Delete from database
                  runSqlPool (delete dirKey) pool
                  runServerLogging state $ logInfoN "Shared directory removed"
                  pure RespSharedDirOk

-- | List shared directories for a VM
handleSharedDirList :: ServerState -> Int64 -> IO Response
handleSharedDirList state vmId = do
  runServerLogging state $ logDebugN $ "Listing shared directories for VM " <> T.pack (show vmId)

  let pool = ssDbPool state
  let vmKey = toSqlKey vmId :: VmId

  -- Check VM exists
  mVm <- runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just _ -> do
      dirs <- runSqlPool (selectList [SharedDirVmId ==. vmKey] []) pool
      let infos = map toSharedDirInfo dirs
      pure $ RespSharedDirList infos

-- | Convert database entity to protocol type
toSharedDirInfo :: Entity SharedDir -> SharedDirInfo
toSharedDirInfo (Entity key dir) =
  SharedDirInfo
    { sdiId = fromSqlKey key
    , sdiPath = sharedDirPath dir
    , sdiTag = sharedDirTag dir
    , sdiCache = sharedDirCache dir
    , sdiReadOnly = sharedDirReadOnly dir
    , sdiPid = sharedDirPid dir
    }
