{-# LANGUAGE OverloadedStrings #-}

-- | Client command handlers for virtual network operations.
module Corvus.Client.Commands.Network
  ( handleNetworkCreate
  , handleNetworkDelete
  , handleNetworkStart
  , handleNetworkStop
  , handleNetworkList
  , handleNetworkShow
  , handleNetworkEdit
  )
where

import Control.Monad (unless)
import Corvus.Client.Connection (Connection)
import Corvus.Client.Output
import Corvus.Client.Rpc
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Protocol (NetworkInfo (..))
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

-- | Handle network create command
handleNetworkCreate :: OutputFormat -> Connection -> Text -> Text -> Bool -> Bool -> Bool -> IO Bool
handleNetworkCreate fmt conn name subnet dhcp nat autostart = do
  resp <- networkCreate conn name subnet dhcp nat autostart
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (NetworkCreated nwId) -> do
      if isStructured fmt
        then outputOkWith fmt [("id", toJSON nwId)]
        else putStrLn $ "Network created with ID: " ++ show nwId
      pure True
    Right (NetworkError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Failed to create network: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle network delete command
handleNetworkDelete :: OutputFormat -> Connection -> Text -> IO Bool
handleNetworkDelete fmt conn nwRef = do
  resp <- networkDelete conn nwRef
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right NetworkDeleted -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn "Network deleted."
      pure True
    Right NetworkNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "Network not found"
        else putStrLn $ "Network '" ++ T.unpack nwRef ++ "' not found."
      pure False
    Right NetworkInUse -> do
      if isStructured fmt
        then outputError fmt "in_use" "Network is in use or running"
        else putStrLn "Cannot delete network: it is in use by network interfaces or currently running."
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle network start command
handleNetworkStart :: OutputFormat -> Connection -> Text -> IO Bool
handleNetworkStart fmt conn nwRef = do
  resp <- networkStart conn nwRef
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right NetworkStarted -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn "Network started."
      pure True
    Right NetworkNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "Network not found"
        else putStrLn $ "Network '" ++ T.unpack nwRef ++ "' not found."
      pure False
    Right NetworkAlreadyRunning -> do
      if isStructured fmt
        then outputError fmt "already_running" "Network is already running"
        else putStrLn "Network is already running."
      pure False
    Right (NetworkError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Failed to start network: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle network stop command
handleNetworkStop :: OutputFormat -> Connection -> Text -> Bool -> IO Bool
handleNetworkStop fmt conn nwRef force = do
  resp <- networkStop conn nwRef force
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right NetworkStopped -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn "Network stopped."
      pure True
    Right NetworkNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "Network not found"
        else putStrLn $ "Network '" ++ T.unpack nwRef ++ "' not found."
      pure False
    Right NetworkNotRunning -> do
      if isStructured fmt
        then outputError fmt "not_running" "Network is not running"
        else putStrLn "Network is not running."
      pure False
    Right NetworkInUse -> do
      if isStructured fmt
        then outputError fmt "in_use" "Network has running VMs attached. Use --force to stop anyway."
        else putStrLn "Cannot stop network: running VMs are connected. Use --force to stop anyway."
      pure False
    Right (NetworkError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Failed to stop network: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle network list command
handleNetworkList :: OutputFormat -> Connection -> IO Bool
handleNetworkList fmt conn = do
  resp <- networkList conn
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (NetworkListResult networks) -> do
      if isStructured fmt
        then outputValue fmt (toJSON networks)
        else do
          if null networks
            then putStrLn "No networks found."
            else do
              printTableHeader [("ID", -5), ("NAME", -20), ("SUBNET", -18), ("DHCP", -6), ("NAT", -5), ("STATUS", -10), ("AS", -3)]
              mapM_ printNetworkInfo networks
      pure True
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle network show command
handleNetworkShow :: OutputFormat -> Connection -> Text -> IO Bool
handleNetworkShow fmt conn nwRef = do
  resp <- networkShow conn nwRef
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (NetworkDetails info) -> do
      if isStructured fmt
        then outputValue fmt (toJSON info)
        else do
          printField "ID" (show (nwiId info))
          printField "Name" (T.unpack (nwiName info))
          let sub = nwiSubnet info
          unless (T.null sub) $
            printField "Subnet" (T.unpack sub)
          printField "DHCP" (if nwiDhcp info then "enabled" else "disabled")
          printField "NAT" (if nwiNat info then "enabled" else "disabled")
          printField "Status" (if nwiRunning info then "running" else "stopped")
          printField "Autostart" (if nwiAutostart info then "enabled" else "disabled")
          case nwiDnsmasqPid info of
            Just pid -> printField "DHCP PID" (show pid)
            Nothing -> pure ()
      pure True
    Right NetworkNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "Network not found"
        else putStrLn $ "Network '" ++ T.unpack nwRef ++ "' not found."
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Print a network info row
printNetworkInfo :: NetworkInfo -> IO ()
printNetworkInfo info =
  printf
    "%-5d %-20s %-18s %-6s %-5s %-10s %-3s\n"
    (nwiId info)
    (T.unpack $ nwiName info)
    (let s = nwiSubnet info in if T.null s then "-" :: String else T.unpack s)
    (if nwiDhcp info then "yes" :: String else "no")
    (if nwiNat info then "yes" :: String else "no")
    (if nwiRunning info then "running" :: String else "stopped")
    (if nwiAutostart info then "+" :: String else "-")

-- | Handle network edit command
handleNetworkEdit :: OutputFormat -> Connection -> Text -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO Bool
handleNetworkEdit fmt conn nwRef mSubnet mDhcp mNat mAutostart = do
  resp <- networkEdit conn nwRef mSubnet mDhcp mNat mAutostart
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right NetworkEdited -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn $ "Network '" ++ T.unpack nwRef ++ "' updated."
      pure True
    Right NetworkNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Network '" <> nwRef <> "' not found")
        else putStrLn $ "Network '" ++ T.unpack nwRef ++ "' not found."
      pure False
    Right (NetworkError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Failed to edit network: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False
