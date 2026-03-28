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
  , printSshKeyInfo
  )
where

import Corvus.Client.Connection
import Corvus.Client.Output (isStructured, outputError, outputOk, outputOkWith, outputResult, printTableHeader)
import Corvus.Client.Rpc
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Protocol (SshKeyInfo (..))
import Data.Aeson (toJSON)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

-- | Handle ssh-key create command
handleSshKeyCreate :: OutputFormat -> Connection -> Text -> Text -> IO Bool
handleSshKeyCreate fmt conn name publicKey = do
  resp <- sshKeyCreate conn name publicKey
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (SshKeyCreated keyId) -> do
      if isStructured fmt
        then outputOkWith fmt [("id", toJSON keyId)]
        else putStrLn $ "SSH key created with ID: " ++ show keyId
      pure True
    Right (SshKeyError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error creating SSH key: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key delete command
handleSshKeyDelete :: OutputFormat -> Connection -> Int64 -> IO Bool
handleSshKeyDelete fmt conn keyId = do
  resp <- sshKeyDelete conn keyId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right SshKeyOk -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn "SSH key deleted."
      pure True
    Right SshKeyNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "SSH key not found"
        else putStrLn $ "SSH key with ID " ++ show keyId ++ " not found."
      pure False
    Right (SshKeyInUse vmIds) -> do
      if isStructured fmt
        then outputError fmt "in_use" ("SSH key is attached to VMs: " <> T.pack (show vmIds))
        else do
          putStrLn $ "SSH key is attached to VMs: " ++ show vmIds
          putStrLn "Detach the key from all VMs first."
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key list command
handleSshKeyList :: OutputFormat -> Connection -> IO Bool
handleSshKeyList fmt conn = do
  resp <- sshKeyList conn
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (SshKeyListResult keys) -> do
      if isStructured fmt
        then outputResult fmt keys
        else do
          if null keys
            then putStrLn "No SSH keys found."
            else do
              printTableHeader [("ID", -6), ("NAME", -20), ("PUBLIC_KEY", -50), ("ATTACHED_VMS", -15)]
              mapM_ printSshKeyInfo keys
      pure True
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key attach command
handleSshKeyAttach :: OutputFormat -> Connection -> Int64 -> Int64 -> IO Bool
handleSshKeyAttach fmt conn vmId keyId = do
  resp <- sshKeyAttach conn vmId keyId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right SshKeyOk -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn "SSH key attached to VM."
      pure True
    Right SshKeyNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "SSH key not found"
        else putStrLn $ "SSH key with ID " ++ show keyId ++ " not found."
      pure False
    Right SshKeyVmNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("VM with ID " <> T.pack (show vmId) <> " not found")
        else putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (SshKeyError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error attaching SSH key: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key detach command
handleSshKeyDetach :: OutputFormat -> Connection -> Int64 -> Int64 -> IO Bool
handleSshKeyDetach fmt conn vmId keyId = do
  resp <- sshKeyDetach conn vmId keyId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right SshKeyOk -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn "SSH key detached from VM."
      pure True
    Right SshKeyNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "SSH key not found"
        else putStrLn $ "SSH key with ID " ++ show keyId ++ " not found or not attached to VM."
      pure False
    Right SshKeyVmNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("VM with ID " <> T.pack (show vmId) <> " not found")
        else putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (SshKeyError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error detaching SSH key: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key list-vm command
handleSshKeyListForVm :: OutputFormat -> Connection -> Int64 -> IO Bool
handleSshKeyListForVm fmt conn vmId = do
  resp <- sshKeyListForVm conn vmId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (SshKeyListResult keys) -> do
      if isStructured fmt
        then outputResult fmt keys
        else do
          if null keys
            then putStrLn "No SSH keys attached to this VM."
            else do
              printTableHeader [("ID", -6), ("NAME", -20), ("PUBLIC_KEY", -50)]
              mapM_ printSshKeyInfoShort keys
      pure True
    Right SshKeyVmNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("VM with ID " <> T.pack (show vmId) <> " not found")
        else putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

--------------------------------------------------------------------------------
-- Printers
--------------------------------------------------------------------------------

-- | Print SSH key info in table format
printSshKeyInfo :: SshKeyInfo -> IO ()
printSshKeyInfo k =
  putStrLn $
    printf
      "%-6d %-20s %-50s %-15s"
      (skiId k)
      (T.unpack $ skiName k)
      (truncateKey 50 $ skiPublicKey k)
      (if null (skiAttachedVms k) then "-" else show (skiAttachedVms k))

-- | Print SSH key info without attached VMs column
printSshKeyInfoShort :: SshKeyInfo -> IO ()
printSshKeyInfoShort k =
  putStrLn $
    printf
      "%-6d %-20s %-50s"
      (skiId k)
      (T.unpack $ skiName k)
      (truncateKey 50 $ skiPublicKey k)

-- | Truncate SSH public key for display
truncateKey :: Int -> Text -> String
truncateKey maxLen key =
  let keyStr = T.unpack key
   in if length keyStr > maxLen
        then take (maxLen - 3) keyStr ++ "..."
        else keyStr
