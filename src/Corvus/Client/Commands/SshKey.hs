{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

import Control.Exception (SomeException, try)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output (Align (..), Column (..), TableOpts, emitError, emitOk, emitOkWith, emitResult, printTable)
import Corvus.Client.Types (OutputFormat)
import Corvus.Protocol (SshKeyInfo (..))
import Corvus.Wire.Common (entityRefFromText)
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T

-- | Handle ssh-key create command
handleSshKeyCreate :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
handleSshKeyCreate fmt conn name publicKey = do
  r <- try @SomeException (CR.rpcSshKeyCreate conn name publicKey)
  case r of
    Right keyId -> do
      emitOkWith fmt [("id", toJSON keyId)] $
        putStrLn $
          "SSH key created with ID: " ++ show keyId
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error creating SSH key: " ++ show e)
      pure False

-- | Handle ssh-key delete command
handleSshKeyDelete :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleSshKeyDelete fmt conn keyRef = do
  r <- try (CR.rpcSshKeyDelete conn (entityRefFromText keyRef)) :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "SSH key deleted."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle ssh-key list command
handleSshKeyList :: OutputFormat -> TableOpts -> CapnpConnection -> IO Bool
handleSshKeyList fmt tableOpts conn = do
  r <- try @SomeException (CR.rpcSshKeyList conn)
  case r of
    Right keys -> do
      emitResult fmt keys $
        if null keys
          then putStrLn "No SSH keys found."
          else printTable tableOpts sshKeyColumns keys
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle ssh-key attach command
handleSshKeyAttach :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
handleSshKeyAttach fmt conn vmRef keyRef = do
  r <- try (CR.rpcSshKeyAttach conn (entityRefFromText vmRef) (entityRefFromText keyRef)) :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "SSH key attached to VM."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error attaching SSH key: " ++ show e)
      pure False

-- | Handle ssh-key detach command
handleSshKeyDetach :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
handleSshKeyDetach fmt conn vmRef keyRef = do
  r <- try (CR.rpcSshKeyDetach conn (entityRefFromText vmRef) (entityRefFromText keyRef)) :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "SSH key detached from VM."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error detaching SSH key: " ++ show e)
      pure False

-- | Handle ssh-key list-vm command
handleSshKeyListForVm :: OutputFormat -> TableOpts -> CapnpConnection -> Text -> IO Bool
handleSshKeyListForVm fmt tableOpts conn vmRef = do
  r <- try @SomeException (CR.rpcSshKeyListForVm conn (entityRefFromText vmRef))
  case r of
    Right keys -> do
      emitResult fmt keys $
        if null keys
          then putStrLn "No SSH keys attached to this VM."
          else printTable tableOpts (filter (\c -> colName c /= "ATTACHED_VMS") sshKeyColumns) keys
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

--------------------------------------------------------------------------------
-- Printers
--------------------------------------------------------------------------------

-- | Column definitions for the @ssh-key list@ table.
sshKeyColumns :: [Column SshKeyInfo]
sshKeyColumns =
  [ Column "ID" RightAlign (show . skiId)
  , Column "NAME" LeftAlign (T.unpack . skiName)
  , Column "PUBLIC_KEY" LeftAlign (T.unpack . skiPublicKey)
  , Column "ATTACHED_VMS" LeftAlign formatAttached
  ]
  where
    formatAttached k
      | null (skiAttachedVms k) = "-"
      | otherwise = T.unpack (T.intercalate ", " (map snd (skiAttachedVms k)))
