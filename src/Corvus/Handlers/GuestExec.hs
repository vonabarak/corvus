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

import Corvus.Model (VmStatus (..))
import Corvus.Model hiding (VmStatus)
import qualified Corvus.Node.VmSpec as VS
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeRouting (withVmNodeAgent)
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
              let req =
                    VS.VmGuestExecReq
                      { VS.vgeVmId = vmId
                      , -- Send the bare command; the agent's
                        -- 'guestExec' detects the guest OS and
                        -- wraps with @/bin/sh -c@ (Linux/BSD) or
                        -- @cmd.exe /c@ (Windows). Pre-wrapping
                        -- with @/bin/sh -c@ here would double-wrap
                        -- on Windows and fail.
                        VS.vgePath = command
                      , VS.vgeArgs = []
                      , VS.vgeCaptureOutput = True
                      , VS.vgeInputData = BS.empty
                      , VS.vgeTimeoutSec = 0
                      }
              outer <- withVmNodeAgent state vmId $ \nac -> NOA.vmGuestExec nac req
              case outer of
                Left err -> pure $ RespGuestAgentError err
                Right r -> case r of
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
                        -- Agent set hasExit=False, surfacing either a
                        -- QGA-side timeout ("guest-exec timed out
                        -- waiting for process to exit") or a
                        -- connection error. Either way the real
                        -- diagnostic lives in stderr — forward it.
                        let stderrText = TE.decodeUtf8 (VS.vgiStderr info)
                            msg
                              | T.null stderrText = "guest-exec did not return an exit code"
                              | otherwise = stderrText
                         in pure $ RespGuestAgentError msg

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
