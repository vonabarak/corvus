{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

import Control.Exception (SomeException, try)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output (Align (..), Column (..), TableOpts, emitError, emitOk, emitOkWith, emitResult, printTable)
import Corvus.Client.Types (OutputFormat)
import Corvus.Model (EnumText (..), SharedDirCache)
import Corvus.Protocol (SharedDirInfo (..))
import Corvus.Wire.Common (entityRefFromText)
import Data.Aeson (toJSON)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T

-- | Parse shared directory cache string to SharedDirCache
parseSharedDirCache :: Text -> Either Text SharedDirCache
parseSharedDirCache = enumFromText

-- | Handle shared directory add command
handleSharedDirAdd :: OutputFormat -> CapnpConnection -> Text -> Text -> Text -> SharedDirCache -> Bool -> IO Bool
handleSharedDirAdd fmt conn vmRef path tag cache readOnly = do
  r <- try @SomeException (CR.rpcSharedDirAdd conn (entityRefFromText vmRef) path tag cache readOnly)
  case r of
    Right dirId -> do
      emitOkWith fmt [("id", toJSON dirId)] $
        putStrLn $
          "Shared directory added with ID: " ++ show dirId
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Failed to add shared directory: " ++ show e)
      pure False

-- | Handle shared directory remove command
handleSharedDirRemove :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
handleSharedDirRemove fmt conn vmRef sharedDirIdText = do
  case readMaybeInt64 (T.unpack sharedDirIdText) of
    Nothing -> do
      emitError fmt "bad_id" ("Invalid shared-directory ID: " <> sharedDirIdText) $
        putStrLn ("Invalid shared-directory ID: " ++ T.unpack sharedDirIdText)
      pure False
    Just sid -> do
      r <- try (CR.rpcSharedDirRemove conn (entityRefFromText vmRef) sid) :: IO (Either SomeException ())
      case r of
        Right () -> do
          emitOk fmt $ putStrLn "Shared directory removed."
          pure True
        Left e -> do
          emitError fmt "rpc_error" (T.pack (show e)) $
            putStrLn ("Error: " ++ show e)
          pure False

-- | Handle shared directory list command
handleSharedDirList :: OutputFormat -> TableOpts -> CapnpConnection -> Text -> IO Bool
handleSharedDirList fmt tableOpts conn vmRef = do
  r <- try @SomeException (CR.rpcSharedDirList conn (entityRefFromText vmRef))
  case r of
    Right dirs -> do
      emitResult fmt dirs $
        if null dirs
          then putStrLn "No shared directories found for this VM."
          else printTable tableOpts sharedDirColumns dirs
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

readMaybeInt64 :: String -> Maybe Int64
readMaybeInt64 s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

-- | Column definitions for the @shared-dir list@ table.
sharedDirColumns :: [Column SharedDirInfo]
sharedDirColumns =
  [ Column "ID" RightAlign (show . sdiId)
  , Column "PATH" LeftAlign (T.unpack . sdiPath)
  , Column "TAG" LeftAlign (T.unpack . sdiTag)
  , Column "CACHE" LeftAlign (T.unpack . enumToText . sdiCache)
  , Column "READ_ONLY" LeftAlign (show . sdiReadOnly)
  , Column "PID" RightAlign (maybe "-" show . sdiPid)
  ]
