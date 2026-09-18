{-# LANGUAGE OverloadedStrings #-}

-- | Cloud-init ISO generation for VMs: the user-facing regenerate
-- handler, the start-path "ensure the ISO exists" check, and the
-- disk-scan that tells us whether a -cloud-init CDROM is attached.
module Corvus.Handlers.Vm.CloudInit
  ( handleVmCloudInit
  , ensureCloudInitIso
  , hasCloudInitIso
  )
where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN)
import Corvus.Action (ActionContext, acState, runAction, runActionAsSubtask)
import Corvus.Handlers.CloudInit (RegenerateCloudInit (..))
import Corvus.Handlers.Vm.Db (getVmWithStatus)
import Corvus.Model
  ( DiskImage (diskImageName)
  , DriveMedia (MediaCdrom)
  , Vm (vmCloudInit, vmName)
  , VmId
  , driveDiskImageId
  )
import qualified Corvus.Model as M
import Corvus.Protocol (Response (RespError, RespVmEdited, RespVmNotFound))
import Corvus.Types (ServerState, ssDbPool)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (Entity (..), get, selectList, (==.))
import Database.Persist.Sql (SqlPersistT, runSqlPool, toSqlKey)

-- | Handle cloud-init ISO generation/regeneration for a VM
handleVmCloudInit :: ServerState -> Text -> Int64 -> IO Response
handleVmCloudInit state clientName vmId = do
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (vm, _) ->
      if not (vmCloudInit vm)
        then pure $ RespError "Cloud-init is not enabled on this VM"
        else do
          ciResp <- runAction state clientName (RegenerateCloudInit vmId (vmName vm))
          case ciResp of
            RespError err -> pure $ RespError $ "Cloud-init ISO generation failed: " <> err
            _ -> pure RespVmEdited

-- | Generate the NoCloud cloud-init ISO for this VM (if cloud-init
-- is enabled and the ISO isn't already attached). Failures are
-- non-fatal — the subtask records its own error.
ensureCloudInitIso :: ActionContext -> Int64 -> Vm -> LoggingT IO ()
ensureCloudInitIso ctx vmId vm = when (vmCloudInit vm) $ do
  hasIso <- liftIO $ runSqlPool (hasCloudInitIso vmId) (ssDbPool (acState ctx))
  unless hasIso $ do
    logInfoN $ "Generating cloud-init ISO for VM " <> T.pack (show vmId)
    _ <- liftIO $ runActionAsSubtask ctx (RegenerateCloudInit vmId (vmName vm))
    pure ()

-- | Check whether the VM has a cloud-init ISO disk attached
hasCloudInitIso :: Int64 -> SqlPersistT IO Bool
hasCloudInitIso vmId = do
  let key = toSqlKey vmId :: VmId
  drives <- selectList [M.DriveVmId ==. key, M.DriveMedia ==. Just MediaCdrom] []
  -- Check if any CDROM drive's disk name ends with "-cloud-init"
  results <- mapM checkDrive drives
  pure $ or results
  where
    checkDrive (Entity _ drive) = case driveDiskImageId drive of
      Nothing -> pure False
      Just diskKey -> do
        mDisk <- get diskKey
        pure $ case mDisk of
          Just disk -> "-cloud-init" `T.isSuffixOf` diskImageName disk
          Nothing -> False
