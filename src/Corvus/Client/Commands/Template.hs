{-# LANGUAGE OverloadedStrings #-}

-- | Template command handlers for the Corvus client.
module Corvus.Client.Commands.Template
  ( -- * Command handlers
    handleTemplateCreate,
    handleTemplateDelete,
    handleTemplateList,
    handleTemplateShow,
    handleTemplateInstantiate,

    -- * Formatters
    printTemplateVmInfo,
    printTemplateDetails,
  )
where

import Control.Monad (forM_)
import Corvus.Client.Connection
import Corvus.Client.Output (isStructured, outputError, outputOk, outputOkWith, outputResult)
import Corvus.Client.Rpc
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Model (EnumText (..))
import Corvus.Protocol (TemplateDetails (..), TemplateDriveInfo (..), TemplateNetIfInfo (..), TemplateSshKeyInfo (..), TemplateVmInfo (..))
import Data.Aeson (toJSON)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Time (defaultTimeLocale, formatTime)
import System.Directory (doesFileExist)
import Text.Printf (printf)

-- | Handle template create command
handleTemplateCreate :: OutputFormat -> Connection -> FilePath -> IO Bool
handleTemplateCreate fmt conn path = do
  exists <- doesFileExist path
  if not exists
    then do
      if isStructured fmt
        then outputError fmt "file_not_found" (T.pack $ "YAML file not found: " ++ path)
        else putStrLn $ "Error: YAML file not found: " ++ path
      pure False
    else do
      content <- T.IO.readFile path
      resp <- templateCreate conn content
      case resp of
        Left err -> do
          if isStructured fmt
            then outputError fmt "rpc_error" (T.pack $ show err)
            else putStrLn $ "Error: " ++ show err
          pure False
        Right (TemplateCreated tid) -> do
          if isStructured fmt
            then outputOkWith fmt [("id", toJSON tid)]
            else putStrLn $ "Template created with ID: " ++ show tid
          pure True
        Right (TemplateError msg) -> do
          if isStructured fmt
            then outputError fmt "error" msg
            else putStrLn $ "Error: " ++ T.unpack msg
          pure False
        Right other -> do
          if isStructured fmt
            then outputError fmt "unexpected" (T.pack $ show other)
            else putStrLn $ "Unexpected response: " ++ show other
          pure False

-- | Handle template delete command
handleTemplateDelete :: OutputFormat -> Connection -> Int64 -> IO Bool
handleTemplateDelete fmt conn tid = do
  resp <- templateDelete conn tid
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right TemplateDeleted -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn "Template deleted."
      pure True
    Right TemplateNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "Template not found"
        else putStrLn "Template not found."
      pure False
    Right (TemplateError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle template list command
handleTemplateList :: OutputFormat -> Connection -> IO Bool
handleTemplateList fmt conn = do
  resp <- templateList conn
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (TemplateListResult templates) -> do
      if isStructured fmt
        then outputResult fmt templates
        else do
          if null templates
            then putStrLn "No templates found."
            else do
              putStrLn $
                printf
                  "%-6s %-30s %-6s %-8s"
                  ("ID" :: String)
                  ("NAME" :: String)
                  ("CPUS" :: String)
                  ("RAM_MB" :: String)
              putStrLn $ replicate 55 '-'
              mapM_ printTemplateVmInfo templates
      pure True
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle template show command
handleTemplateShow :: OutputFormat -> Connection -> Int64 -> IO Bool
handleTemplateShow fmt conn tid = do
  resp <- templateShow conn tid
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (TemplateDetailsResult details) -> do
      if isStructured fmt
        then outputResult fmt details
        else printTemplateDetails details
      pure True
    Right TemplateNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "Template not found"
        else putStrLn "Template not found."
      pure False
    Right (TemplateError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle template instantiate command
handleTemplateInstantiate :: OutputFormat -> Connection -> Int64 -> Text -> IO Bool
handleTemplateInstantiate fmt conn tid name = do
  resp <- templateInstantiate conn tid name
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (TemplateInstantiated vmId) -> do
      if isStructured fmt
        then outputOkWith fmt [("id", toJSON vmId)]
        else putStrLn $ "VM instantiated with ID: " ++ show vmId
      pure True
    Right TemplateNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "Template not found"
        else putStrLn "Template not found."
      pure False
    Right (TemplateError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

--------------------------------------------------------------------------------
-- Printers
--------------------------------------------------------------------------------

-- | Print template VM info in list view
printTemplateVmInfo :: TemplateVmInfo -> IO ()
printTemplateVmInfo t =
  putStrLn $
    printf
      "%-6d %-30s %-6d %-8d"
      (tviId t)
      (T.unpack $ tviName t)
      (tviCpuCount t)
      (tviRamMb t)

-- | Print full template details
printTemplateDetails :: TemplateDetails -> IO ()
printTemplateDetails t = do
  putStrLn $ "Template ID:  " ++ show (tvdId t)
  putStrLn $ "Name:         " ++ T.unpack (tvdName t)
  putStrLn $ "CPUs:         " ++ show (tvdCpuCount t)
  putStrLn $ "RAM:          " ++ show (tvdRamMb t) ++ " MB"
  putStrLn $ "Created At:   " ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (tvdCreatedAt t)
  case tvdDescription t of
    Just desc -> putStrLn $ "Description:  " ++ T.unpack desc
    Nothing -> pure ()

  putStrLn "\nDrives:"
  if null (tvdDrives t)
    then putStrLn "  No drives defined."
    else do
      putStrLn $
        printf
          "  %-15s %-12s %-10s %-10s %-10s %-8s"
          ("IMAGE" :: String)
          ("INTERFACE" :: String)
          ("STRATEGY" :: String)
          ("READ_ONLY" :: String)
          ("CACHE" :: String)
          ("NEW_SIZE" :: String)
      putStrLn $ "  " ++ replicate 75 '-'
      forM_ (tvdDrives t) $ \d ->
        putStrLn $
          printf
            "  %-15s %-12s %-10s %-10s %-10s %-8s"
            (T.unpack $ tvdiDiskImageName d)
            (T.unpack $ enumToText $ tvdiInterface d)
            (T.unpack $ enumToText $ tvdiCloneStrategy d)
            (show $ tvdiReadOnly d)
            (T.unpack $ enumToText $ tvdiCacheType d)
            (maybe "-" show (tvdiNewSizeMb d))

  putStrLn "\nNetwork Interfaces:"
  if null (tvdNetIfs t)
    then putStrLn "  No network interfaces defined."
    else do
      putStrLn $
        printf
          "  %-10s %-20s"
          ("TYPE" :: String)
          ("HOST_DEVICE" :: String)
      putStrLn $ "  " ++ replicate 35 '-'
      forM_ (tvdNetIfs t) $ \ni ->
        putStrLn $
          printf
            "  %-10s %-20s"
            (T.unpack $ enumToText $ tvniType ni)
            (maybe "-" T.unpack $ tvniHostDevice ni)

  putStrLn "\nSSH Keys:"
  if null (tvdSshKeys t)
    then putStrLn "  No SSH keys defined."
    else do
      forM_ (tvdSshKeys t) $ \k ->
        putStrLn $ "  - " ++ T.unpack (tvskiName k) ++ " (ID: " ++ show (tvskiId k) ++ ")"
