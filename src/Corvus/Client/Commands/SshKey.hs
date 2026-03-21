{-# LANGUAGE OverloadedStrings #-}

-- | SSH key command handlers for the Corvus client.
module Corvus.Client.Commands.SshKey
  ( -- * Command handlers
    handleSshKeyCreate,
    handleSshKeyDelete,
    handleSshKeyList,
    handleSshKeyAttach,
    handleSshKeyDetach,
    handleSshKeyListForVm,

    -- * Formatters
    printSshKeyInfo,
  )
where

import Corvus.Client.Connection
import Corvus.Client.Rpc
import Corvus.Protocol (SshKeyInfo (..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

-- | Handle ssh-key create command
handleSshKeyCreate :: Connection -> Text -> Text -> IO Bool
handleSshKeyCreate conn name publicKey = do
  resp <- sshKeyCreate conn name publicKey
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (SshKeyCreated keyId) -> do
      putStrLn $ "SSH key created with ID: " ++ show keyId
      pure True
    Right (SshKeyError msg) -> do
      putStrLn $ "Error creating SSH key: " ++ T.unpack msg
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key delete command
handleSshKeyDelete :: Connection -> Int64 -> IO Bool
handleSshKeyDelete conn keyId = do
  resp <- sshKeyDelete conn keyId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right SshKeyOk -> do
      putStrLn "SSH key deleted."
      pure True
    Right SshKeyNotFound -> do
      putStrLn $ "SSH key with ID " ++ show keyId ++ " not found."
      pure False
    Right (SshKeyInUse vmIds) -> do
      putStrLn $ "SSH key is attached to VMs: " ++ show vmIds
      putStrLn "Detach the key from all VMs first."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key list command
handleSshKeyList :: Connection -> IO Bool
handleSshKeyList conn = do
  resp <- sshKeyList conn
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (SshKeyListResult keys) -> do
      if null keys
        then putStrLn "No SSH keys found."
        else do
          putStrLn $
            printf
              "%-6s %-20s %-50s %-15s"
              ("ID" :: String)
              ("NAME" :: String)
              ("PUBLIC_KEY" :: String)
              ("ATTACHED_VMS" :: String)
          putStrLn $ replicate 95 '-'
          mapM_ printSshKeyInfo keys
      pure True
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key attach command
handleSshKeyAttach :: Connection -> Int64 -> Int64 -> IO Bool
handleSshKeyAttach conn vmId keyId = do
  resp <- sshKeyAttach conn vmId keyId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right SshKeyOk -> do
      putStrLn "SSH key attached to VM."
      pure True
    Right SshKeyNotFound -> do
      putStrLn $ "SSH key with ID " ++ show keyId ++ " not found."
      pure False
    Right SshKeyVmNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (SshKeyError msg) -> do
      putStrLn $ "Error attaching SSH key: " ++ T.unpack msg
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key detach command
handleSshKeyDetach :: Connection -> Int64 -> Int64 -> IO Bool
handleSshKeyDetach conn vmId keyId = do
  resp <- sshKeyDetach conn vmId keyId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right SshKeyOk -> do
      putStrLn "SSH key detached from VM."
      pure True
    Right SshKeyNotFound -> do
      putStrLn $ "SSH key with ID " ++ show keyId ++ " not found or not attached to VM."
      pure False
    Right SshKeyVmNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (SshKeyError msg) -> do
      putStrLn $ "Error detaching SSH key: " ++ T.unpack msg
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle ssh-key list-vm command
handleSshKeyListForVm :: Connection -> Int64 -> IO Bool
handleSshKeyListForVm conn vmId = do
  resp <- sshKeyListForVm conn vmId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (SshKeyListResult keys) -> do
      if null keys
        then putStrLn "No SSH keys attached to this VM."
        else do
          putStrLn $
            printf
              "%-6s %-20s %-50s"
              ("ID" :: String)
              ("NAME" :: String)
              ("PUBLIC_KEY" :: String)
          putStrLn $ replicate 80 '-'
          mapM_ printSshKeyInfoShort keys
      pure True
    Right SshKeyVmNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
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
