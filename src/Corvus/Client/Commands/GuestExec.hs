{-# LANGUAGE OverloadedStrings #-}

-- | Guest execution command handlers for the Corvus client.
module Corvus.Client.Commands.GuestExec
  ( handleVmExec
  )
where

import Control.Monad (unless)
import Corvus.Client.Connection
import Corvus.Client.Output (emitError, emitOkWith, emitRpcError)
import Corvus.Client.Rpc (GuestExecResult (..), vmExec)
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Model (EnumText (..))
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (stderr)

-- | Handle guest-exec command
handleVmExec :: OutputFormat -> Connection -> Text -> Text -> IO Bool
handleVmExec fmt conn vmRef command = do
  resp <- vmExec conn vmRef command
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (GuestExecOk exitcode stdout stdErr) -> do
      emitOkWith
        fmt
        [ ("exitcode", toJSON exitcode)
        , ("stdout", toJSON stdout)
        , ("stderr", toJSON stdErr)
        ]
        $ do
          TIO.putStr stdout
          unless (T.null stdErr) $ TIO.hPutStr stderr stdErr
      pure (exitcode == 0)
    Right GuestExecVmNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right GuestExecNotEnabled -> do
      emitError fmt "guest_agent_not_enabled" ("Guest agent is not enabled on VM '" <> vmRef <> "'") $
        putStrLn $
          "Error: Guest agent is not enabled on VM '" ++ T.unpack vmRef ++ "'. Enable with: crv vm edit " ++ T.unpack vmRef ++ " --guest-agent true"
      pure False
    Right (GuestExecInvalidState status msg) -> do
      emitError fmt "invalid_state" msg $ do
        putStrLn $ "Error: " ++ T.unpack msg
        putStrLn $ "Current status: " ++ T.unpack (enumToText status)
      pure False
    Right (GuestExecAgentError msg) -> do
      emitError fmt "guest_agent_error" msg $ putStrLn $ "Guest agent error: " ++ T.unpack msg
      pure False
