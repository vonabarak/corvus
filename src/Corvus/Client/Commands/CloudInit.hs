{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Cloud-init config command handlers for the Corvus client.
module Corvus.Client.Commands.CloudInit
  ( handleCloudInitGenerate
  , handleCloudInitSet
  , handleCloudInitEdit
  , handleCloudInitShow
  , handleCloudInitDelete
  )
where

import Control.Exception (SomeException, try)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Editor (editInEditor)
import Corvus.Client.Output (emitError, emitOk, emitResult)
import Corvus.Client.Types (OutputFormat)
import Corvus.Protocol (CloudInitInfo (..))
import Corvus.Schema.CloudInit (CloudInitConfigYaml (..))
import Corvus.Wire.Common (entityRefFromText)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist)

-- | Handle cloud-init generate command (regenerate ISO).
--
-- The Vm cap's 'cloudInit' method regenerates the ISO and returns
-- the current config; success → "ok".
handleCloudInitGenerate :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleCloudInitGenerate fmt conn vmRef = do
  r <- try @SomeException (CR.rpcVmCloudInit conn (entityRefFromText vmRef))
  case r of
    Right _ -> do
      emitOk fmt $ putStrLn "Cloud-init ISO generated."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle cloud-init set command.
handleCloudInitSet :: OutputFormat -> CapnpConnection -> Text -> Maybe FilePath -> IO Bool
handleCloudInitSet fmt conn vmRef mPath = case mPath of
  Just path -> do
    exists <- doesFileExist path
    if not exists
      then do
        emitError fmt "file_not_found" (T.pack $ "File not found: " ++ path) $
          putStrLn $
            "Error: File not found: " ++ path
        pure False
      else do
        content <- TIO.readFile path
        sendCloudInitConfig fmt conn vmRef content
  Nothing -> do
    edited <- editInEditor skeletonCloudInitYaml
    case edited of
      Left err -> do
        emitError fmt "editor" err $ TIO.putStrLn $ "Error: " <> err
        pure False
      Right content -> sendCloudInitConfig fmt conn vmRef content

-- | Handle cloud-init edit command: fetch → edit → update.
handleCloudInitEdit :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleCloudInitEdit fmt conn vmRef = do
  r <- try @SomeException (CR.rpcCloudInitGet conn (entityRefFromText vmRef))
  case r of
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False
    Right mConfig -> do
      let initial = maybe skeletonCloudInitYaml cloudInitInfoToYaml mConfig
      edited <- editInEditor initial
      case edited of
        Left err -> do
          emitError fmt "editor" err $ TIO.putStrLn $ "Error: " <> err
          pure False
        Right content
          | content == initial -> do
              emitOk fmt $ putStrLn "No changes; cloud-init config left untouched."
              pure True
          | otherwise -> sendCloudInitConfig fmt conn vmRef content

-- | Handle cloud-init show command
handleCloudInitShow :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleCloudInitShow fmt conn vmRef = do
  r <- try @SomeException (CR.rpcCloudInitGet conn (entityRefFromText vmRef))
  case r of
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False
    Right mConfig -> do
      emitResult fmt mConfig $ case mConfig of
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

-- | Handle cloud-init delete command
handleCloudInitDelete :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleCloudInitDelete fmt conn vmRef = do
  r <- try (CR.rpcCloudInitDelete conn (entityRefFromText vmRef)) :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitResult fmt ("ok" :: Text) $ putStrLn "Cloud-init config deleted. Using defaults."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Parse YAML content as CloudInitConfigYaml and send to server.
sendCloudInitConfig :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
sendCloudInitConfig fmt conn vmRef content =
  case Yaml.decodeEither' (TE.encodeUtf8 content) of
    Left err -> do
      let msg = T.pack $ show err
      emitError fmt "parse_error" msg $ putStrLn $ "Error parsing YAML: " ++ T.unpack msg
      pure False
    Right (cic :: CloudInitConfigYaml) -> do
      r <-
        try
          ( CR.rpcCloudInitSet
              conn
              (entityRefFromText vmRef)
              (cicyUserData cic)
              (cicyNetworkConfig cic)
              (cicyInjectSshKeys cic)
          )
          :: IO (Either SomeException ())
      case r of
        Right () -> do
          emitResult fmt ("ok" :: Text) $ putStrLn "Cloud-init config updated."
          pure True
        Left e -> do
          emitError fmt "rpc_error" (T.pack (show e)) $
            putStrLn ("Error: " ++ show e)
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
