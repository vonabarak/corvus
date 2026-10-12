{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Client command handlers for @crv node@ subcommands.
module Corvus.Client.Commands.Node
  ( handleNodeAdd
  , handleNodeList
  , handleNodeShow
  , handleNodeEdit
  , handleNodeDrain
  , handleNodeDelete
  )
where

import Control.Exception (SomeException, try)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output
import Corvus.Client.Output.Columns (nodeColumns)
import Corvus.Client.Types (OutputFormat)
import Corvus.Model (EnumText (..), NodeAdminState)
import Corvus.Protocol (NodeDetails (..))
import Corvus.Size (formatSize)
import Corvus.Wire.Common (entityRefFromText)
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)

--------------------------------------------------------------------------------
-- handleNodeAdd
--------------------------------------------------------------------------------

handleNodeAdd
  :: OutputFormat
  -> CapnpConnection
  -> Text
  -> Text
  -> Int
  -> Int
  -> Maybe Text
  -> Maybe Text
  -> Text
  -- ^ admin state, as parsed text
  -> Bool
  -- ^ netd-disabled
  -> IO Bool
handleNodeAdd fmt conn name host nodeAgentPort netAgentPort mBasePath mDesc adminStateText netdDisabled =
  case enumFromText adminStateText :: Either Text NodeAdminState of
    Left err -> do
      emitError fmt "bad_arg" err $ putStrLn ("Invalid admin-state: " ++ T.unpack err)
      pure False
    Right adminState -> do
      -- An empty basePath on the wire is the signal to the daemon
      -- to ask the target nodeagent for its preferred $HOME/VMs.
      let basePath = fromMaybe T.empty mBasePath
      r <-
        try @SomeException
          (CR.rpcNodeAdd conn name host nodeAgentPort netAgentPort basePath mDesc adminState netdDisabled)
      case r of
        Right nid -> do
          emitOkWith fmt [("id", toJSON nid)] $
            putStrLn $
              "Node created with ID: " ++ show nid
          pure True
        Left e -> do
          emitRpcError fmt e $
            putStrLn ("Failed to add node: " ++ show e)
          pure False

--------------------------------------------------------------------------------
-- handleNodeList
--------------------------------------------------------------------------------

handleNodeList :: OutputFormat -> TableOpts -> CapnpConnection -> IO Bool
handleNodeList fmt tableOpts conn = do
  r <- try @SomeException (CR.rpcNodeList conn)
  case r of
    Right nodes -> do
      emitListResult fmt tableOpts nodeColumns "No nodes registered." nodes
    Left e -> do
      emitRpcError fmt e $
        putStrLn ("Error: " ++ show e)
      pure False

--------------------------------------------------------------------------------
-- handleNodeShow
--------------------------------------------------------------------------------

handleNodeShow :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleNodeShow fmt conn nRef = do
  r <- try @SomeException (CR.rpcNodeShow conn (entityRefFromText nRef))
  case r of
    Right d -> do
      emitResult fmt d $ printDetails d
      pure True
    Left e -> do
      emitRpcError fmt e $
        putStrLn ("Error: " ++ show e)
      pure False
  where
    printDetails d = do
      printField "ID" (show (nodId d))
      printField "Name" (T.unpack (nodName d))
      printField "Host" (T.unpack (nodHost d))
      printField "Nodeagent port" (show (nodNodeAgentPort d))
      printField "Netd port" (show (nodNetAgentPort d))
      printField "Base path" (T.unpack (nodBasePath d))
      printField "Admin state" (T.unpack (enumToText (nodAdminState d)))
      printField "Description" (maybe "(none)" T.unpack (nodDescription d))
      printField "Created" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (nodCreatedAt d))
      printField "Kernel" (maybe "(unknown)" T.unpack (nodKernelRelease d))
      printField "Agent version" (maybe "(unknown)" T.unpack (nodAgentVersion d))
      printField "CPUs (total)" (maybe "--" show (nodCpuCount d))
      printField "RAM total" (maybe "--" formatSize (nodRamTotal d))
      printField "RAM free" (maybe "--" formatSize (nodRamFree d))
      printField "Storage total" (maybe "--" formatSize (nodStorageBytesTotal d))
      printField "Storage free" (maybe "--" formatSize (nodStorageBytesFree d))
      printField "Load avg (1/5/15)" $
        loadStr (nodLoadAvg1 d) (nodLoadAvg5 d) (nodLoadAvg15 d)
      printField "Nodeagent last push" $
        maybe "(never)" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S") (nodLastNodeAgentPushAt d)
      printField "Netd last push" $
        maybe "(never)" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S") (nodLastNetAgentPushAt d)
      printField "Netd disabled" (formatBool (nodNetdDisabled d))
      printField "Netd connected" $
        formatBool (nodNetdConnected d)

    loadStr a b c = render a ++ " / " ++ render b ++ " / " ++ render c
    render = maybe "--" (formatRound . (\x -> x :: Double))
    formatRound x =
      let scaled :: Int
          scaled = round (x * 100)
       in show (fromIntegral scaled / (100 :: Double))

--------------------------------------------------------------------------------
-- handleNodeEdit
--------------------------------------------------------------------------------

handleNodeEdit
  :: OutputFormat
  -> CapnpConnection
  -> Text
  -- ^ node ref
  -> Maybe Text
  -- ^ new name
  -> Maybe Text
  -- ^ new host
  -> Maybe Int
  -- ^ new nodeAgent port
  -> Maybe Int
  -- ^ new netd port
  -> Maybe Text
  -- ^ new basePath
  -> Maybe (Maybe Text)
  -- ^ description: Nothing = leave; Just Nothing = clear; Just (Just t) = set.
  -> Maybe Text
  -- ^ admin state text (online | draining | maintenance)
  -> Maybe Bool
  -- ^ netd-disabled: Nothing = leave unchanged.
  -> IO Bool
handleNodeEdit fmt conn nRef mName mHost mNodeAgentPort mNetAgentPort mBasePath mDesc mAdminText mNetdDisabled = do
  case traverse parseAdminState mAdminText of
    Left err -> do
      emitError fmt "bad_arg" err $ putStrLn ("Invalid admin-state: " ++ T.unpack err)
      pure False
    Right mAdminSt -> do
      r <-
        try @SomeException $
          CR.rpcNodeEdit
            conn
            (entityRefFromText nRef)
            mName
            mHost
            mNodeAgentPort
            mNetAgentPort
            mBasePath
            mDesc
            mAdminSt
            mNetdDisabled
      case r of
        Right () -> do
          emitOk fmt $ putStrLn "Node updated."
          pure True
        Left e -> do
          emitRpcError fmt e $
            putStrLn ("Failed to edit node: " ++ show e)
          pure False
  where
    parseAdminState = enumFromText :: Text -> Either Text NodeAdminState

--------------------------------------------------------------------------------
-- handleNodeDrain
--------------------------------------------------------------------------------

handleNodeDrain :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleNodeDrain fmt conn nRef = do
  r <- try @SomeException (CR.rpcNodeDrain conn (entityRefFromText nRef))
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "Node set to draining; scheduler will skip it."
      pure True
    Left e -> do
      emitRpcError fmt e $
        putStrLn ("Failed to drain node: " ++ show e)
      pure False

--------------------------------------------------------------------------------
-- handleNodeDelete
--------------------------------------------------------------------------------

handleNodeDelete :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleNodeDelete fmt conn nRef = do
  r <- try @SomeException (CR.rpcNodeDelete conn (entityRefFromText nRef))
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "Node deleted."
      pure True
    Left e -> do
      emitRpcError fmt e $
        putStrLn ("Failed to delete node: " ++ show e)
      pure False

--------------------------------------------------------------------------------
-- Table for `crv node list`
--------------------------------------------------------------------------------
