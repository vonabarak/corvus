{-# LANGUAGE OverloadedStrings #-}

-- | Cloud-init config command handlers for the Corvus client.
module Corvus.Client.Commands.CloudInit
  ( handleCloudInitGenerate
  , handleCloudInitSet
  , handleCloudInitEdit
  , handleCloudInitShow
  , handleCloudInitDelete
  )
where

import Corvus.Client.Connection (Connection)
import Corvus.Client.Editor (editInEditor)
import Corvus.Client.Output (isStructured, outputError, outputOk, outputResult)
import Corvus.Client.Rpc (CloudInitResult (..), VmEditResult (..), cloudInitDelete, cloudInitGet, cloudInitSet, vmCloudInit)
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Handlers.Template (CloudInitConfigYaml (..))
import Corvus.Protocol (CloudInitInfo (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist)

-- | Handle cloud-init generate command (regenerate ISO)
handleCloudInitGenerate :: OutputFormat -> Connection -> Text -> IO Bool
handleCloudInitGenerate fmt conn vmRef = do
  resp <- vmCloudInit conn vmRef
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right VmEdited -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn "Cloud-init ISO generated."
      pure True
    Right (VmEditError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right VmEditNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "VM not found"
        else putStrLn "VM not found"
      pure False
    Right VmEditMustBeStopped -> do
      if isStructured fmt
        then outputError fmt "vm_running" "VM must be stopped"
        else putStrLn "VM must be stopped"
      pure False

-- | Handle cloud-init set command.
-- When a file path is given, parse it as a CloudInitConfigYaml.
-- When no file is given, open $EDITOR on a skeleton.
handleCloudInitSet :: OutputFormat -> Connection -> Text -> Maybe FilePath -> IO Bool
handleCloudInitSet fmt conn vmRef mPath = case mPath of
  Just path -> do
    exists <- doesFileExist path
    if not exists
      then do
        if isStructured fmt
          then outputError fmt "file_not_found" (T.pack $ "File not found: " ++ path)
          else putStrLn $ "Error: File not found: " ++ path
        pure False
      else do
        content <- TIO.readFile path
        sendCloudInitConfig fmt conn vmRef content
  Nothing -> do
    edited <- editInEditor skeletonCloudInitYaml
    case edited of
      Left err -> do
        if isStructured fmt
          then outputError fmt "editor" err
          else TIO.putStrLn $ "Error: " <> err
        pure False
      Right content -> sendCloudInitConfig fmt conn vmRef content

-- | Handle cloud-init edit command: fetch → edit in $EDITOR → update.
handleCloudInitEdit :: OutputFormat -> Connection -> Text -> IO Bool
handleCloudInitEdit fmt conn vmRef = do
  showResp <- cloudInitGet conn vmRef
  case showResp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right CloudInitNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "VM not found"
        else putStrLn "VM not found"
      pure False
    Right (CloudInitConfig mConfig) -> do
      let initial = maybe skeletonCloudInitYaml cloudInitInfoToYaml mConfig
      edited <- editInEditor initial
      case edited of
        Left err -> do
          if isStructured fmt
            then outputError fmt "editor" err
            else TIO.putStrLn $ "Error: " <> err
          pure False
        Right content
          | content == initial -> do
              if isStructured fmt
                then outputOk fmt
                else putStrLn "No changes; cloud-init config left untouched."
              pure True
          | otherwise -> sendCloudInitConfig fmt conn vmRef content
    Right (CloudInitError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right _ -> do
      putStrLn "Unexpected response"
      pure False

-- | Handle cloud-init show command
handleCloudInitShow :: OutputFormat -> Connection -> Text -> IO Bool
handleCloudInitShow fmt conn vmRef = do
  resp <- cloudInitGet conn vmRef
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (CloudInitConfig mConfig) -> do
      if isStructured fmt
        then outputResult fmt mConfig
        else case mConfig of
          Nothing -> putStrLn "Using default cloud-init configuration."
          Just ci -> do
            putStrLn $ "Inject SSH Keys: " ++ show (ciiInjectSshKeys ci)
            putStrLn $ "User Data:       " ++ maybe "(default)" (const "custom") (ciiUserData ci)
            case ciiUserData ci of
              Just ud -> do
                putStrLn "--- user-data ---"
                TIO.putStrLn ud
                putStrLn "-----------------"
              Nothing -> pure ()
            putStrLn $ "Network Config:  " ++ maybe "(none)" (const "custom") (ciiNetworkConfig ci)
            case ciiNetworkConfig ci of
              Just nc -> do
                putStrLn "--- network-config ---"
                TIO.putStrLn nc
                putStrLn "----------------------"
              Nothing -> pure ()
      pure True
    Right CloudInitNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "VM not found"
        else putStrLn "VM not found"
      pure False
    Right (CloudInitError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right _ -> do
      putStrLn "Unexpected response"
      pure False

-- | Handle cloud-init delete command
handleCloudInitDelete :: OutputFormat -> Connection -> Text -> IO Bool
handleCloudInitDelete fmt conn vmRef = do
  resp <- cloudInitDelete conn vmRef
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right CloudInitOk -> do
      if isStructured fmt
        then outputResult fmt ("ok" :: Text)
        else putStrLn "Cloud-init config deleted. Using defaults."
      pure True
    Right CloudInitNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" "VM not found"
        else putStrLn "VM not found"
      pure False
    Right (CloudInitError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right _ -> do
      putStrLn "Unexpected response"
      pure False

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Parse YAML content as CloudInitConfigYaml and send to server.
sendCloudInitConfig :: OutputFormat -> Connection -> Text -> Text -> IO Bool
sendCloudInitConfig fmt conn vmRef content =
  case Yaml.decodeEither' (TE.encodeUtf8 content) of
    Left err -> do
      let msg = T.pack $ show err
      if isStructured fmt
        then outputError fmt "parse_error" msg
        else putStrLn $ "Error parsing YAML: " ++ T.unpack msg
      pure False
    Right cic -> do
      resp <- cloudInitSet conn vmRef (cicyUserData cic) (cicyNetworkConfig cic) (cicyInjectSshKeys cic)
      case resp of
        Left err -> do
          if isStructured fmt
            then outputError fmt "rpc_error" (T.pack $ show err)
            else putStrLn $ "Error: " ++ show err
          pure False
        Right CloudInitOk -> do
          if isStructured fmt
            then outputResult fmt ("ok" :: Text)
            else putStrLn "Cloud-init config updated."
          pure True
        Right CloudInitNotFound -> do
          if isStructured fmt
            then outputError fmt "not_found" "VM not found"
            else putStrLn "VM not found"
          pure False
        Right (CloudInitError msg) -> do
          if isStructured fmt
            then outputError fmt "error" msg
            else putStrLn $ "Error: " ++ T.unpack msg
          pure False
        Right _ -> do
          putStrLn "Unexpected response"
          pure False

-- | Convert a CloudInitInfo (from the server) back to YAML text for editing.
cloudInitInfoToYaml :: CloudInitInfo -> Text
cloudInitInfoToYaml ci =
  TE.decodeUtf8 $
    Yaml.encode $
      Yaml.object $
        [ "injectSshKeys" Yaml..= ciiInjectSshKeys ci
        ]
          ++ maybe [] (\ud -> ["userData" Yaml..= ud]) (ciiUserData ci)
          ++ maybe [] (\nc -> ["networkConfig" Yaml..= nc]) (ciiNetworkConfig ci)

-- | Skeleton YAML for interactive cloud-init config creation.
skeletonCloudInitYaml :: Text
skeletonCloudInitYaml =
  "# Cloud-init configuration.\n\
  \# Edit this file, save and exit to apply.\n\
  \#\n\
  \# userData accepts either structured YAML (cloud-config) or a raw string\n\
  \# (e.g. a PowerShell script starting with #ps1_sysnative for Windows).\n\
  \injectSshKeys: true\n\
  \# userData:\n\
  \#   packages:\n\
  \#     - qemu-guest-agent\n\
  \#   runcmd:\n\
  \#     - systemctl enable qemu-guest-agent\n\
  \# networkConfig:\n\
  \#   version: 2\n\
  \#   ethernets:\n\
  \#     eth0:\n\
  \#       dhcp4: true\n"
