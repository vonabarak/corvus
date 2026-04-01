{-# LANGUAGE OverloadedStrings #-}

-- | Guest command execution handler.
-- Executes commands inside running VMs via the QEMU Guest Agent.
module Corvus.Handlers.GuestExec
  ( handleGuestExec
  )
where

import Corvus.Model (VmStatus (..))
import Corvus.Model hiding (VmStatus)
import Corvus.Protocol
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Qemu.GuestAgent (GuestExecResult (..), guestExec)
import Corvus.Types
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlPersistT, toSqlKey)

-- | Handle guest command execution via QGA.
-- Checks: VM exists, VM is running, guest agent is enabled.
handleGuestExec :: ServerState -> Int64 -> Text -> IO Response
handleGuestExec state vmId command = do
  mVm <- runSqlPool (getVmForExec vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just (status, ga) ->
      if status /= VmRunning
        then pure $ RespInvalidTransition status "VM must be running for guest-exec"
        else
          if not ga
            then pure RespGuestAgentNotEnabled
            else do
              result <- guestExec (ssQemuConfig state) vmId command
              case result of
                GuestExecSuccess exitcode stdout stderr ->
                  pure $ RespGuestExecResult exitcode stdout stderr
                GuestExecError err ->
                  pure $ RespGuestAgentError err
                GuestExecConnectionFailed err ->
                  pure $ RespGuestAgentError $ "Connection failed: " <> err

-- | Get VM status and guestAgent flag
getVmForExec :: Int64 -> SqlPersistT IO (Maybe (VmStatus, Bool))
getVmForExec vmId = do
  let key = toSqlKey vmId :: VmId
  mVm <- get key
  pure $ case mVm of
    Nothing -> Nothing
    Just vm -> Just (vmStatus vm, vmGuestAgent vm)
