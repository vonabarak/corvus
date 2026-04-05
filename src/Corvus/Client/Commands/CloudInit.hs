{-# LANGUAGE OverloadedStrings #-}

-- | Cloud-init config command handlers for the Corvus client.
module Corvus.Client.Commands.CloudInit
  ( handleCloudInitGenerate
  , handleCloudInitSet
  , handleCloudInitShow
  , handleCloudInitDelete
  )
where

import Corvus.Client.Connection (Connection)
import Corvus.Client.Output (isStructured, outputError, outputOk, outputResult)
import Corvus.Client.Rpc (CloudInitResult (..), VmEditResult (..), cloudInitDelete, cloudInitGet, cloudInitSet, vmCloudInit)
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Protocol (CloudInitInfo (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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

-- | Handle cloud-init set command
handleCloudInitSet :: OutputFormat -> Connection -> Text -> Maybe FilePath -> Maybe FilePath -> Bool -> IO Bool
handleCloudInitSet fmt conn vmRef mUserDataFile mNetworkConfigFile noInjectSshKeys = do
  -- Read file contents
  mUserData <- case mUserDataFile of
    Just path -> do
      content <- TIO.readFile path
      -- Strip #cloud-config header if present (Corvus manages it)
      let stripped = stripCloudConfigHeader content
      pure $ Just stripped
    Nothing -> pure Nothing
  mNetworkConfig <- case mNetworkConfigFile of
    Just path -> Just <$> TIO.readFile path
    Nothing -> pure Nothing

  let injectKeys = not noInjectSshKeys
  resp <- cloudInitSet conn vmRef mUserData mNetworkConfig injectKeys
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

-- | Strip the #cloud-config header from user-data text.
-- Corvus manages this header internally.
stripCloudConfigHeader :: Text -> Text
stripCloudConfigHeader t =
  case T.lines t of
    (first : rest)
      | T.strip first == "#cloud-config" -> T.unlines rest
    _ -> t
