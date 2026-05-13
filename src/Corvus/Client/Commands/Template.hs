{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Template command handlers for the Corvus client.
module Corvus.Client.Commands.Template
  ( -- * Command handlers
    handleTemplateCreate
  , handleTemplateEdit
  , handleTemplateDelete
  , handleTemplateList
  , handleTemplateShow
  , handleTemplateInstantiate

    -- * Formatters
  , templateVmColumns
  , printTemplateDetails
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Commands.Template.Yaml (skeletonTemplateYaml, templateDetailsToYaml)
import Corvus.Client.Editor (editInEditor)
import Corvus.Client.Output (Align (..), Column (..), TableOpts, emitError, emitOk, emitOkWith, emitResult, printField, printTable, tableFormat)
import Corvus.Client.Types (OutputFormat)
import Corvus.Model (EnumText (..))
import Corvus.Protocol (TemplateDetails (..), TemplateDriveInfo (..), TemplateNetIfInfo (..), TemplateSshKeyInfo (..), TemplateVmInfo (..))
import Corvus.Wire.Common (entityRefFromText)
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Time (defaultTimeLocale, formatTime)
import System.Directory (doesFileExist)
import Text.Printf (printf)

-- | Handle template create command
handleTemplateCreate :: OutputFormat -> CapnpConnection -> Maybe FilePath -> IO Bool
handleTemplateCreate fmt conn mPath = case mPath of
  Just path -> do
    exists <- doesFileExist path
    if not exists
      then do
        emitError fmt "file_not_found" (T.pack $ "YAML file not found: " ++ path) $
          putStrLn $
            "Error: YAML file not found: " ++ path
        pure False
      else do
        content <- T.IO.readFile path
        sendCreate fmt conn content
  Nothing -> do
    edited <- editInEditor skeletonTemplateYaml
    case edited of
      Left err -> do
        emitError fmt "editor" err $ T.IO.putStrLn $ "Error: " <> err
        pure False
      Right content -> sendCreate fmt conn content

sendCreate :: OutputFormat -> CapnpConnection -> Text -> IO Bool
sendCreate fmt conn content = do
  r <- try @SomeException (CR.rpcTemplateCreate conn content)
  case r of
    Right tid -> do
      emitOkWith fmt [("id", toJSON tid)] $
        putStrLn $
          "Template created with ID: " ++ show tid
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle template edit command: fetch → edit → update.
handleTemplateEdit :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleTemplateEdit fmt conn tRef = do
  r <- try @SomeException (CR.rpcTemplateShow conn (entityRefFromText tRef))
  case r of
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False
    Right details -> do
      let initial = templateDetailsToYaml details
      edited <- editInEditor initial
      case edited of
        Left err -> do
          emitError fmt "editor" err $ T.IO.putStrLn $ "Error: " <> err
          pure False
        Right content
          | content == initial -> do
              emitOk fmt $ putStrLn "No changes; template left untouched."
              pure True
          | otherwise -> do
              ur <- try (CR.rpcTemplateUpdate conn (entityRefFromText tRef) content) :: IO (Either SomeException ())
              case ur of
                Right () -> do
                  emitOk fmt $ putStrLn $ "Template '" ++ T.unpack tRef ++ "' updated."
                  pure True
                Left e -> do
                  emitError fmt "rpc_error" (T.pack (show e)) $
                    putStrLn ("Error: " ++ show e)
                  pure False

-- | Handle template delete command
handleTemplateDelete :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleTemplateDelete fmt conn tid = do
  r <- try (CR.rpcTemplateDelete conn (entityRefFromText tid)) :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "Template deleted."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle template list command
handleTemplateList :: OutputFormat -> TableOpts -> CapnpConnection -> IO Bool
handleTemplateList fmt tableOpts conn = do
  r <- try @SomeException (CR.rpcTemplateList conn)
  case r of
    Right templates -> do
      emitResult fmt templates $
        if null templates
          then putStrLn "No templates found."
          else printTable tableOpts templateVmColumns templates
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle template show command
handleTemplateShow :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleTemplateShow fmt conn tid = do
  r <- try @SomeException (CR.rpcTemplateShow conn (entityRefFromText tid))
  case r of
    Right details -> do
      emitResult fmt details $ printTemplateDetails details
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle template instantiate command
handleTemplateInstantiate :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
handleTemplateInstantiate fmt conn tid name = do
  r <- try @SomeException (CR.rpcTemplateInstantiate conn (entityRefFromText tid) name)
  case r of
    Right vmId -> do
      emitOkWith fmt [("id", toJSON vmId)] $
        putStrLn $
          "VM instantiated with ID: " ++ show vmId
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

--------------------------------------------------------------------------------
-- Printers
--------------------------------------------------------------------------------

-- | Column definitions for the @template list@ table.
templateVmColumns :: [Column TemplateVmInfo]
templateVmColumns =
  [ Column "ID" RightAlign (show . tviId)
  , Column "NAME" LeftAlign (T.unpack . tviName)
  , Column "CPUS" RightAlign (show . tviCpuCount)
  , Column "RAM_MB" RightAlign (show . tviRamMb)
  ]

-- | Print full template details
printTemplateDetails :: TemplateDetails -> IO ()
printTemplateDetails t = do
  printField "Template ID" (show (tvdId t))
  printField "Name" (T.unpack (tvdName t))
  printField "CPUs" (show (tvdCpuCount t))
  printField "RAM" (show (tvdRamMb t) ++ " MB")
  printField "Created At" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (tvdCreatedAt t))
  case tvdDescription t of
    Just desc -> printField "Description" (T.unpack desc)
    Nothing -> pure ()
  printField "Console" (if tvdHeadless t then "serial (headless)" else "SPICE (graphics)")
  printField "Guest Agent" (if tvdGuestAgent t then "enabled" else "disabled")
  printField "Cloud-init" (if tvdCloudInit t then "enabled" else "disabled")
  printField "Autostart" (if tvdAutostart t then "enabled" else "disabled")
  case tvdCloudInitConfig t of
    Just _ -> printField "Cloud-init Config" "custom"
    Nothing -> pure ()

  putStrLn "\nDrives:"
  if null (tvdDrives t)
    then putStrLn "  No drives defined."
    else do
      let dCols = [("IMAGE", -15), ("INTERFACE", -12), ("STRATEGY", -10), ("READ_ONLY", -10), ("CACHE", -10), ("NEW_SIZE", -8)]
          (dFmt, dSep) = tableFormat dCols
      printf ("  " ++ dFmt) ("IMAGE" :: String) ("INTERFACE" :: String) ("STRATEGY" :: String) ("READ_ONLY" :: String) ("CACHE" :: String) ("NEW_SIZE" :: String)
      putStrLn $ "  " ++ dSep
      forM_ (tvdDrives t) $ \d ->
        printf
          ("  " ++ dFmt)
          (maybe "-" T.unpack (tvdiDiskImageName d))
          (T.unpack $ enumToText $ tvdiInterface d)
          (T.unpack $ enumToText $ tvdiCloneStrategy d)
          (show $ tvdiReadOnly d)
          (T.unpack $ enumToText $ tvdiCacheType d)
          (maybe "-" show (tvdiSizeMb d))

  putStrLn "\nNetwork Interfaces:"
  if null (tvdNetIfs t)
    then putStrLn "  No network interfaces defined."
    else do
      let nCols = [("TYPE", -10), ("HOST_DEVICE", -20)]
          (niFmt, niSep) = tableFormat nCols
      printf ("  " ++ niFmt) ("TYPE" :: String) ("HOST_DEVICE" :: String)
      putStrLn $ "  " ++ niSep
      forM_ (tvdNetIfs t) $ \ni ->
        printf
          ("  " ++ niFmt)
          (T.unpack $ enumToText $ tvniType ni)
          (maybe "-" T.unpack $ tvniHostDevice ni)

  putStrLn "\nSSH Keys:"
  if null (tvdSshKeys t)
    then putStrLn "  No SSH keys defined."
    else forM_ (tvdSshKeys t) $ \k ->
      putStrLn $ "  - " ++ T.unpack (tvskiName k) ++ " (ID: " ++ show (tvskiId k) ++ ")"
