{-# LANGUAGE OverloadedStrings #-}

-- | Guest command execution handler.
--
-- Executes commands inside running VMs via the QEMU Guest Agent.
-- Routes through @nodeAgent.vmGuestExec@: the agent owns the
-- QGA socket and the per-VM connection cache; the daemon just
-- validates state and dispatches the request.
module Corvus.Handlers.GuestExec
  ( -- * Action types
    GuestExec (..)

    -- * Handlers
  , handleGuestExec
  )
where

import Corvus.Action

import Control.Concurrent.STM (readTVarIO)
import Corvus.Model (VmStatus (..))
import Corvus.Model hiding (VmStatus)
import qualified Corvus.Node.VmSpec as VS
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Protocol
import Corvus.Types
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
              mAgent <- readTVarIO (ssNodeAgent state)
              case mAgent of
                Nothing -> pure $ RespGuestAgentError "nodeagent unavailable"
                Just nac -> do
                  let req =
                        VS.VmGuestExecReq
                          { VS.vgeVmId = vmId
                          , -- Daemon historically sent the whole command
                            -- as one shell-style line; preserve that by
                            -- using /bin/sh -c.
                            VS.vgePath = "/bin/sh"
                          , VS.vgeArgs = ["-c", command]
                          , VS.vgeCaptureOutput = True
                          , VS.vgeInputData = BS.empty
                          , VS.vgeTimeoutSec = 0
                          }
                  r <- NOA.vmGuestExec nac req
                  case r of
                    Left e ->
                      pure $ RespGuestAgentError ("vmGuestExec: " <> T.pack (show e))
                    Right info
                      | VS.vgiHasExit info ->
                          pure $
                            RespGuestExecResult
                              (fromIntegral (VS.vgiExitCode info))
                              (TE.decodeUtf8 (VS.vgiStdout info))
                              (TE.decodeUtf8 (VS.vgiStderr info))
                      | otherwise ->
                          pure $ RespGuestAgentError "guest-exec timeout / no exit"

-- | Get VM status and guestAgent flag
getVmForExec :: Int64 -> SqlPersistT IO (Maybe (VmStatus, Bool))
getVmForExec vmId = do
  let key = toSqlKey vmId :: VmId
  mVm <- get key
  pure $ case mVm of
    Nothing -> Nothing
    Just vm -> Just (vmStatus vm, vmGuestAgent vm)

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

data GuestExec = GuestExec
  { geVmId :: Int64
  , geCommand :: Text
  }

instance Action GuestExec where
  actionSubsystem _ = SubVm
  actionCommand _ = "guest-exec"
  actionEntityId = Just . fromIntegral . geVmId
  actionExecute ctx a = handleGuestExec (acState ctx) (geVmId a) (geCommand a)
