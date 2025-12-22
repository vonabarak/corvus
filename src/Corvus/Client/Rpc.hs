{-# LANGUAGE OverloadedStrings #-}

-- | RPC call functions for the Corvus client.
module Corvus.Client.Rpc
  ( -- * Basic commands
    sendPing,
    getStatus,
    requestShutdown,

    -- * VM listing
    listVms,
    showVm,

    -- * VM actions
    VmActionResult (..),
    vmStart,
    vmStop,
    vmPause,
    vmReset,
  )
where

import Corvus.Client.Connection
import Corvus.Model (VmStatus)
import Corvus.Protocol
import Data.Int (Int64)
import Data.Text (Text)

-- | Result of a VM action
data VmActionResult
  = -- | Action succeeded, new status
    VmActionSuccess !VmStatus
  | -- | VM not found
    VmActionNotFound
  | -- | Invalid transition: current status and error
    VmActionInvalid !VmStatus !Text
  deriving (Eq, Show)

-- | Send a ping request
sendPing :: Connection -> IO (Either ConnectionError ())
sendPing conn = do
  result <- sendRequest conn ReqPing
  case result of
    Left err -> pure $ Left err
    Right RespPong -> pure $ Right ()
    Right (RespError msg) -> pure $ Left $ ServerError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Get daemon status
getStatus :: Connection -> IO (Either ConnectionError StatusInfo)
getStatus conn = do
  result <- sendRequest conn ReqStatus
  case result of
    Left err -> pure $ Left err
    Right (RespStatus info) -> pure $ Right info
    Right (RespError msg) -> pure $ Left $ ServerError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Request daemon shutdown
requestShutdown :: Connection -> IO (Either ConnectionError Bool)
requestShutdown conn = do
  result <- sendRequest conn ReqShutdown
  case result of
    Left err -> pure $ Left err
    Right (RespShutdownAck ack) -> pure $ Right ack
    Right (RespError msg) -> pure $ Left $ ServerError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | List all VMs
listVms :: Connection -> IO (Either ConnectionError [VmInfo])
listVms conn = do
  result <- sendRequest conn ReqListVms
  case result of
    Left err -> pure $ Left err
    Right (RespVmList vms) -> pure $ Right vms
    Right (RespError msg) -> pure $ Left $ ServerError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Show VM details
showVm :: Connection -> Int64 -> IO (Either ConnectionError (Maybe VmDetails))
showVm conn vmId = do
  result <- sendRequest conn (ReqShowVm vmId)
  case result of
    Left err -> pure $ Left err
    Right (RespVmDetails details) -> pure $ Right (Just details)
    Right RespVmNotFound -> pure $ Right Nothing
    Right (RespError msg) -> pure $ Left $ ServerError msg
    Right _ -> pure $ Left $ DecodeFailed "Unexpected response"

-- | Helper for VM action responses
handleVmActionResponse :: Either ConnectionError Response -> Either ConnectionError VmActionResult
handleVmActionResponse result = case result of
  Left err -> Left err
  Right (RespVmStateChanged newStatus) -> Right $ VmActionSuccess newStatus
  Right RespVmNotFound -> Right VmActionNotFound
  Right (RespInvalidTransition currentStatus errMsg) -> Right $ VmActionInvalid currentStatus errMsg
  Right (RespError msg) -> Left $ ServerError msg
  Right _ -> Left $ DecodeFailed "Unexpected response"

-- | Start a VM (stopped/paused -> running)
vmStart :: Connection -> Int64 -> IO (Either ConnectionError VmActionResult)
vmStart conn vmId = handleVmActionResponse <$> sendRequest conn (ReqVmStart vmId)

-- | Stop a VM (running -> stopped)
vmStop :: Connection -> Int64 -> IO (Either ConnectionError VmActionResult)
vmStop conn vmId = handleVmActionResponse <$> sendRequest conn (ReqVmStop vmId)

-- | Pause a VM (running -> paused)
vmPause :: Connection -> Int64 -> IO (Either ConnectionError VmActionResult)
vmPause conn vmId = handleVmActionResponse <$> sendRequest conn (ReqVmPause vmId)

-- | Reset a VM (any -> stopped)
vmReset :: Connection -> Int64 -> IO (Either ConnectionError VmActionResult)
vmReset conn vmId = handleVmActionResponse <$> sendRequest conn (ReqVmReset vmId)
