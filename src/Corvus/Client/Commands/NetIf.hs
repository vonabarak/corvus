{-# LANGUAGE OverloadedStrings #-}

-- | Network interface command handlers for the Corvus client.
module Corvus.Client.Commands.NetIf
  ( -- * Command handlers
    handleNetIfAdd,
    handleNetIfRemove,
    handleNetIfList,

    -- * Parsers
    parseNetInterfaceType,
  )
where

import Corvus.Client.Connection
import Corvus.Client.Rpc
import Corvus.Model (EnumText (..), NetInterfaceType)
import Corvus.Protocol (NetIfInfo (..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

-- | Parse network interface type string to NetInterfaceType
parseNetInterfaceType :: Text -> Either Text NetInterfaceType
parseNetInterfaceType = enumFromText

-- | Handle network interface add command
handleNetIfAdd :: Connection -> Int64 -> NetInterfaceType -> Text -> Text -> IO Bool
handleNetIfAdd conn vmId ifaceType hostDevice macAddress = do
  resp <- netIfAdd conn vmId ifaceType hostDevice macAddress
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (NetIfAdded netIfId) -> do
      putStrLn $ "Network interface added with ID: " ++ show netIfId
      pure True
    Right NetIfVmNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (NetIfError msg) -> do
      putStrLn $ "Failed to add network interface: " ++ T.unpack msg
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle network interface remove command
handleNetIfRemove :: Connection -> Int64 -> Int64 -> IO Bool
handleNetIfRemove conn vmId netIfId = do
  resp <- netIfRemove conn vmId netIfId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right NetIfOk -> do
      putStrLn "Network interface removed."
      pure True
    Right NetIfNotFound -> do
      putStrLn $ "Network interface with ID " ++ show netIfId ++ " not found."
      pure False
    Right NetIfVmNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle network interface list command
handleNetIfList :: Connection -> Int64 -> IO Bool
handleNetIfList conn vmId = do
  resp <- netIfList conn vmId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (NetIfListResult netIfs) -> do
      if null netIfs
        then putStrLn "No network interfaces found for this VM."
        else do
          putStrLn $
            printf
              "%-5s %-15s %-20s %-20s"
              ("ID" :: String)
              ("TYPE" :: String)
              ("DEVICE" :: String)
              ("MAC" :: String)
          putStrLn $ replicate 65 '-'
          mapM_ printNetIfInfo netIfs
      pure True
    Right NetIfVmNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Print network interface info
printNetIfInfo :: NetIfInfo -> IO ()
printNetIfInfo info =
  printf
    "%-5d %-15s %-20s %-20s\n"
    (niId info)
    (T.unpack $ enumToText $ niType info)
    (T.unpack $ niHostDevice info)
    (T.unpack $ niMacAddress info)
