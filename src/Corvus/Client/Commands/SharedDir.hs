{-# LANGUAGE OverloadedStrings #-}

-- | Shared directory command handlers for the Corvus client.
module Corvus.Client.Commands.SharedDir
  ( -- * Command handlers
    handleSharedDirAdd,
    handleSharedDirRemove,
    handleSharedDirList,

    -- * Parsers
    parseSharedDirCache,
  )
where

import Corvus.Client.Connection
import Corvus.Client.Rpc
import Corvus.Model (EnumText (..), SharedDirCache)
import Corvus.Protocol (SharedDirInfo (..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

-- | Parse shared directory cache string to SharedDirCache
parseSharedDirCache :: Text -> Either Text SharedDirCache
parseSharedDirCache = enumFromText

-- | Handle shared directory add command
handleSharedDirAdd :: Connection -> Int64 -> Text -> Text -> SharedDirCache -> Bool -> IO Bool
handleSharedDirAdd conn vmId path tag cache readOnly = do
  resp <- sharedDirAdd conn vmId path tag cache readOnly
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (SharedDirAdded dirId) -> do
      putStrLn $ "Shared directory added with ID: " ++ show dirId
      pure True
    Right SharedDirVmNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (SharedDirError msg) -> do
      putStrLn $ "Failed to add shared directory: " ++ T.unpack msg
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle shared directory remove command
handleSharedDirRemove :: Connection -> Int64 -> Int64 -> IO Bool
handleSharedDirRemove conn vmId sharedDirId = do
  resp <- sharedDirRemove conn vmId sharedDirId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right SharedDirOk -> do
      putStrLn "Shared directory removed."
      pure True
    Right SharedDirNotFound -> do
      putStrLn $ "Shared directory with ID " ++ show sharedDirId ++ " not found."
      pure False
    Right SharedDirVmNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right SharedDirVmMustBeStopped -> do
      putStrLn "Cannot remove shared directory while VM is running. Stop the VM first."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle shared directory list command
handleSharedDirList :: Connection -> Int64 -> IO Bool
handleSharedDirList conn vmId = do
  resp <- sharedDirList conn vmId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (SharedDirListResult dirs) -> do
      if null dirs
        then putStrLn "No shared directories found for this VM."
        else do
          putStrLn $
            printf
              "%-5s %-40s %-15s %-10s %-10s %-10s"
              ("ID" :: String)
              ("PATH" :: String)
              ("TAG" :: String)
              ("CACHE" :: String)
              ("READ_ONLY" :: String)
              ("PID" :: String)
          putStrLn $ replicate 100 '-'
          mapM_ printSharedDirInfo dirs
      pure True
    Right SharedDirVmNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Print shared directory info
printSharedDirInfo :: SharedDirInfo -> IO ()
printSharedDirInfo info =
  printf
    "%-5d %-40s %-15s %-10s %-10s %-10s\n"
    (sdiId info)
    (T.unpack $ sdiPath info)
    (T.unpack $ sdiTag info)
    (T.unpack $ enumToText $ sdiCache info)
    (show $ sdiReadOnly info)
    (maybe "-" show $ sdiPid info)
