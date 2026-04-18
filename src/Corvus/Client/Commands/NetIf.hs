{-# LANGUAGE OverloadedStrings #-}

-- | Network interface command handlers for the Corvus client.
module Corvus.Client.Commands.NetIf
  ( -- * Command handlers
    handleNetIfAdd
  , handleNetIfRemove
  , handleNetIfList

    -- * Parsers
  , parseNetInterfaceType
  )
where

import Corvus.Client.Connection
import Corvus.Client.Output (Align (..), Column (..), TableOpts, emitError, emitOk, emitOkWith, emitResult, emitRpcError, printTable)
import Corvus.Client.Rpc
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Model (EnumText (..), NetInterfaceType)
import Corvus.Protocol (NetIfInfo (..))
import Data.Aeson (toJSON)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T

-- | Parse network interface type string to NetInterfaceType
parseNetInterfaceType :: Text -> Either Text NetInterfaceType
parseNetInterfaceType = enumFromText

-- | Handle network interface add command
handleNetIfAdd :: OutputFormat -> Connection -> Text -> NetInterfaceType -> Text -> Maybe Text -> Maybe Text -> IO Bool
handleNetIfAdd fmt conn vmRef ifaceType hostDevice macAddress mNetworkRef = do
  resp <- netIfAdd conn vmRef ifaceType hostDevice macAddress mNetworkRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (NetIfAdded netIfId) -> do
      emitOkWith fmt [("id", toJSON netIfId)] $
        putStrLn $
          "Network interface added with ID: " ++ show netIfId
      pure True
    Right NetIfVmNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right (NetIfError msg) -> do
      emitError fmt "error" msg $ putStrLn $ "Failed to add network interface: " ++ T.unpack msg
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle network interface remove command
handleNetIfRemove :: OutputFormat -> Connection -> Text -> Int64 -> IO Bool
handleNetIfRemove fmt conn vmRef netIfId = do
  resp <- netIfRemove conn vmRef netIfId
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right NetIfOk -> do
      emitOk fmt $ putStrLn "Network interface removed."
      pure True
    Right NetIfNotFound -> do
      emitError fmt "not_found" "Network interface not found" $
        putStrLn $
          "Network interface with ID " ++ show netIfId ++ " not found."
      pure False
    Right NetIfVmNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle network interface list command
handleNetIfList :: OutputFormat -> TableOpts -> Connection -> Text -> IO Bool
handleNetIfList fmt tableOpts conn vmRef = do
  resp <- netIfList conn vmRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (NetIfListResult netIfs) -> do
      emitResult fmt netIfs $
        if null netIfs
          then putStrLn "No network interfaces found for this VM."
          else printTable tableOpts netIfColumns netIfs
      pure True
    Right NetIfVmNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Column definitions for the @net-if list@ table.
netIfColumns :: [Column NetIfInfo]
netIfColumns =
  [ Column "ID" RightAlign Nothing (show . niId)
  , Column "TYPE" LeftAlign Nothing (T.unpack . enumToText . niType)
  , Column "DEVICE" LeftAlign (Just 30) (T.unpack . niHostDevice)
  , Column "MAC" LeftAlign Nothing (T.unpack . niMacAddress)
  , Column "GUEST_IPS" LeftAlign (Just 40) (maybe "" T.unpack . niGuestIpAddresses)
  ]
