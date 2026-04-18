{-# LANGUAGE OverloadedStrings #-}

-- | SSH key command handlers for the Corvus client.
module Corvus.Client.Commands.SshKey
  ( -- * Command handlers
    handleSshKeyCreate
  , handleSshKeyDelete
  , handleSshKeyList
  , handleSshKeyAttach
  , handleSshKeyDetach
  , handleSshKeyListForVm

    -- * Formatters
  , sshKeyColumns
  )
where

import Corvus.Client.Connection
import Corvus.Client.Output (Align (..), Column (..), TableOpts, emitError, emitOk, emitOkWith, emitResult, emitRpcError, printTable)
import Corvus.Client.Rpc
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Protocol (SshKeyInfo (..))
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T

-- | Handle ssh-key create command
handleSshKeyCreate :: OutputFormat -> Connection -> Text -> Text -> IO Bool
handleSshKeyCreate fmt conn name publicKey = do
  resp <- sshKeyCreate conn name publicKey
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (SshKeyCreated keyId) -> do
      emitOkWith fmt [("id", toJSON keyId)] $
        putStrLn $
          "SSH key created with ID: " ++ show keyId
      pure True
    Right (SshKeyError msg) -> do
      emitError fmt "error" msg $ putStrLn $ "Error creating SSH key: " ++ T.unpack msg
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key delete command
handleSshKeyDelete :: OutputFormat -> Connection -> Text -> IO Bool
handleSshKeyDelete fmt conn keyRef = do
  resp <- sshKeyDelete conn keyRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right SshKeyOk -> do
      emitOk fmt $ putStrLn "SSH key deleted."
      pure True
    Right SshKeyNotFound -> do
      emitError fmt "not_found" "SSH key not found" $
        putStrLn $
          "SSH key '" ++ T.unpack keyRef ++ "' not found."
      pure False
    Right (SshKeyInUse vmPairs) -> do
      let vmNames = T.intercalate ", " (map snd vmPairs)
      emitError fmt "in_use" ("SSH key is attached to VMs: " <> vmNames) $ do
        putStrLn $ "SSH key is attached to VMs: " ++ T.unpack vmNames
        putStrLn "Detach the key from all VMs first."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key list command
handleSshKeyList :: OutputFormat -> TableOpts -> Connection -> IO Bool
handleSshKeyList fmt tableOpts conn = do
  resp <- sshKeyList conn
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (SshKeyListResult keys) -> do
      emitResult fmt keys $
        if null keys
          then putStrLn "No SSH keys found."
          else printTable tableOpts sshKeyColumns keys
      pure True
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key attach command
handleSshKeyAttach :: OutputFormat -> Connection -> Text -> Text -> IO Bool
handleSshKeyAttach fmt conn vmRef keyRef = do
  resp <- sshKeyAttach conn vmRef keyRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right SshKeyOk -> do
      emitOk fmt $ putStrLn "SSH key attached to VM."
      pure True
    Right SshKeyNotFound -> do
      emitError fmt "not_found" "SSH key not found" $
        putStrLn $
          "SSH key '" ++ T.unpack keyRef ++ "' not found."
      pure False
    Right SshKeyVmNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right (SshKeyError msg) -> do
      emitError fmt "error" msg $ putStrLn $ "Error attaching SSH key: " ++ T.unpack msg
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key detach command
handleSshKeyDetach :: OutputFormat -> Connection -> Text -> Text -> IO Bool
handleSshKeyDetach fmt conn vmRef keyRef = do
  resp <- sshKeyDetach conn vmRef keyRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right SshKeyOk -> do
      emitOk fmt $ putStrLn "SSH key detached from VM."
      pure True
    Right SshKeyNotFound -> do
      emitError fmt "not_found" "SSH key not found" $
        putStrLn $
          "SSH key '" ++ T.unpack keyRef ++ "' not found or not attached to VM."
      pure False
    Right SshKeyVmNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right (SshKeyError msg) -> do
      emitError fmt "error" msg $ putStrLn $ "Error detaching SSH key: " ++ T.unpack msg
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key list-vm command
handleSshKeyListForVm :: OutputFormat -> TableOpts -> Connection -> Text -> IO Bool
handleSshKeyListForVm fmt tableOpts conn vmRef = do
  resp <- sshKeyListForVm conn vmRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (SshKeyListResult keys) -> do
      -- Short view: drop the ATTACHED_VMS column (always empty in this context).
      emitResult fmt keys $
        if null keys
          then putStrLn "No SSH keys attached to this VM."
          else printTable tableOpts (filter (\c -> colName c /= "ATTACHED_VMS") sshKeyColumns) keys
      pure True
    Right SshKeyVmNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

--------------------------------------------------------------------------------
-- Printers
--------------------------------------------------------------------------------

-- | Column definitions for the @ssh-key list@ table.
sshKeyColumns :: [Column SshKeyInfo]
sshKeyColumns =
  [ Column "ID" RightAlign Nothing (show . skiId)
  , Column "NAME" LeftAlign (Just 30) (T.unpack . skiName)
  , Column "PUBLIC_KEY" LeftAlign (Just 50) (T.unpack . skiPublicKey)
  , Column "ATTACHED_VMS" LeftAlign (Just 30) formatAttached
  ]
  where
    formatAttached k
      | null (skiAttachedVms k) = "-"
      | otherwise = T.unpack (T.intercalate ", " (map snd (skiAttachedVms k)))
