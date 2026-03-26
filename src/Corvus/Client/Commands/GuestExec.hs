{-# LANGUAGE OverloadedStrings #-}

-- | Guest execution command handlers for the Corvus client.
module Corvus.Client.Commands.GuestExec
  ( handleVmExec
  )
where

import Control.Monad (unless)
import Corvus.Client.Connection
import Corvus.Client.Output (isStructured, outputError, outputOkWith)
import Corvus.Client.Rpc (GuestExecResult (..), vmExec)
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Model (EnumText (..))
import Data.Aeson (toJSON)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (stderr)

-- | Handle guest-exec command
handleVmExec :: OutputFormat -> Connection -> Int64 -> Text -> IO Bool
handleVmExec fmt conn vmId command = do
  resp <- vmExec conn vmId command
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (GuestExecOk exitcode stdout stdErr) -> do
      if isStructured fmt
        then
          outputOkWith
            fmt
            [ ("exitcode", toJSON exitcode)
            , ("stdout", toJSON stdout)
            , ("stderr", toJSON stdErr)
            ]
        else do
          TIO.putStr stdout
          unless (T.null stdErr) $ TIO.hPutStr stderr stdErr
      pure (exitcode == 0)
    Right GuestExecVmNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("VM with ID " <> T.pack (show vmId) <> " not found")
        else putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right GuestExecNotEnabled -> do
      if isStructured fmt
        then outputError fmt "guest_agent_not_enabled" ("Guest agent is not enabled on VM " <> T.pack (show vmId))
        else putStrLn $ "Error: Guest agent is not enabled on VM " ++ show vmId ++ ". Enable with: crv vm edit " ++ show vmId ++ " --guest-agent true"
      pure False
    Right (GuestExecInvalidState status msg) -> do
      if isStructured fmt
        then outputError fmt "invalid_state" msg
        else do
          putStrLn $ "Error: " ++ T.unpack msg
          putStrLn $ "Current status: " ++ T.unpack (enumToText status)
      pure False
    Right (GuestExecAgentError msg) -> do
      if isStructured fmt
        then outputError fmt "guest_agent_error" msg
        else putStrLn $ "Guest agent error: " ++ T.unpack msg
      pure False
