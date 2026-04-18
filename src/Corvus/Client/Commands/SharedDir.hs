{-# LANGUAGE OverloadedStrings #-}

-- | Shared directory command handlers for the Corvus client.
module Corvus.Client.Commands.SharedDir
  ( -- * Command handlers
    handleSharedDirAdd
  , handleSharedDirRemove
  , handleSharedDirList

    -- * Parsers
  , parseSharedDirCache
  )
where

import Corvus.Client.Connection
import Corvus.Client.Output (Align (..), Column (..), TableOpts, emitError, emitOk, emitOkWith, emitResult, emitRpcError, printTable)
import Corvus.Client.Rpc
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Model (EnumText (..), SharedDirCache)
import Corvus.Protocol (SharedDirInfo (..))
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T

-- | Parse shared directory cache string to SharedDirCache
parseSharedDirCache :: Text -> Either Text SharedDirCache
parseSharedDirCache = enumFromText

-- | Handle shared directory add command
handleSharedDirAdd :: OutputFormat -> Connection -> Text -> Text -> Text -> SharedDirCache -> Bool -> IO Bool
handleSharedDirAdd fmt conn vmRef path tag cache readOnly = do
  resp <- sharedDirAdd conn vmRef path tag cache readOnly
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (SharedDirAdded dirId) -> do
      emitOkWith fmt [("id", toJSON dirId)] $
        putStrLn $
          "Shared directory added with ID: " ++ show dirId
      pure True
    Right SharedDirVmNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right (SharedDirError msg) -> do
      emitError fmt "error" msg $ putStrLn $ "Failed to add shared directory: " ++ T.unpack msg
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle shared directory remove command
handleSharedDirRemove :: OutputFormat -> Connection -> Text -> Text -> IO Bool
handleSharedDirRemove fmt conn vmRef sharedDirRef = do
  resp <- sharedDirRemove conn vmRef sharedDirRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right SharedDirOk -> do
      emitOk fmt $ putStrLn "Shared directory removed."
      pure True
    Right SharedDirNotFound -> do
      emitError fmt "not_found" "Shared directory not found" $
        putStrLn $
          "Shared directory '" ++ T.unpack sharedDirRef ++ "' not found."
      pure False
    Right SharedDirVmNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right SharedDirVmMustBeStopped -> do
      emitError fmt "vm_must_be_stopped" "VM must be stopped" $
        putStrLn "Cannot remove shared directory while VM is running. Stop the VM first."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle shared directory list command
handleSharedDirList :: OutputFormat -> TableOpts -> Connection -> Text -> IO Bool
handleSharedDirList fmt tableOpts conn vmRef = do
  resp <- sharedDirList conn vmRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (SharedDirListResult dirs) -> do
      emitResult fmt dirs $
        if null dirs
          then putStrLn "No shared directories found for this VM."
          else printTable tableOpts sharedDirColumns dirs
      pure True
    Right SharedDirVmNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Column definitions for the @shared-dir list@ table.
sharedDirColumns :: [Column SharedDirInfo]
sharedDirColumns =
  [ Column "ID" RightAlign Nothing (show . sdiId)
  , Column "PATH" LeftAlign (Just 50) (T.unpack . sdiPath)
  , Column "TAG" LeftAlign (Just 20) (T.unpack . sdiTag)
  , Column "CACHE" LeftAlign Nothing (T.unpack . enumToText . sdiCache)
  , Column "READ_ONLY" LeftAlign Nothing (show . sdiReadOnly)
  , Column "PID" RightAlign Nothing (maybe "-" show . sdiPid)
  ]
