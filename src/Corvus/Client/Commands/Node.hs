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
import Corvus.Client.Types (OutputFormat)
import Corvus.Model (EnumText (..), NodeAdminState)
import qualified Corvus.Model as M
import Corvus.Protocol (NodeDetails (..), NodeInfo (..))
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
  -> Text
  -> Maybe Text
  -> Text
  -- ^ admin state, as parsed text
  -> IO Bool
handleNodeAdd fmt conn name host nodeAgentPort netAgentPort basePath mDesc adminStateText =
  case enumFromText adminStateText :: Either Text NodeAdminState of
    Left err -> do
      emitError fmt "bad_arg" err $ putStrLn ("Invalid admin-state: " ++ T.unpack err)
      pure False
    Right adminState -> do
      r <-
        try @SomeException
          (CR.rpcNodeAdd conn name host nodeAgentPort netAgentPort basePath mDesc adminState)
      case r of
        Right nid -> do
          emitOkWith fmt [("id", toJSON nid)] $
            putStrLn $
              "Node created with ID: " ++ show nid
          pure True
        Left e -> do
          emitError fmt "rpc_error" (T.pack (show e)) $
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
      emitResult fmt nodes $
        if null nodes
          then putStrLn "No nodes registered."
          else printTable tableOpts nodeColumns nodes
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
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
      emitError fmt "rpc_error" (T.pack (show e)) $
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
      printField "RAM total (MiB)" (maybe "--" show (nodRamMbTotal d))
      printField "RAM free (MiB)" (maybe "--" show (nodRamMbFree d))
      printField "Storage total (B)" (maybe "--" show (nodStorageBytesTotal d))
      printField "Storage free (B)" (maybe "--" show (nodStorageBytesFree d))
      printField "Load avg (1/5/15)" $
        loadStr (nodLoadAvg1 d) (nodLoadAvg5 d) (nodLoadAvg15 d)
      printField "Nodeagent last push" $
        maybe "(never)" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S") (nodLastNodeAgentPushAt d)
      printField "Netd last push" $
        maybe "(never)" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S") (nodLastNetAgentPushAt d)

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
  -> IO Bool
handleNodeEdit fmt conn nRef mName mHost mNodeAgentPort mNetAgentPort mBasePath mDesc mAdminText = do
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
      case r of
        Right () -> do
          emitOk fmt $ putStrLn "Node updated."
          pure True
        Left e -> do
          emitError fmt "rpc_error" (T.pack (show e)) $
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
      emitError fmt "rpc_error" (T.pack (show e)) $
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
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Failed to delete node: " ++ show e)
      pure False

--------------------------------------------------------------------------------
-- Table for `crv node list`
--------------------------------------------------------------------------------

nodeColumns :: [Column NodeInfo]
nodeColumns =
  [ Column "ID" RightAlign (show . noiId)
  , Column "NAME" LeftAlign (T.unpack . noiName)
  , Column "HOST" LeftAlign (T.unpack . noiHost)
  , Column "STATE" LeftAlign (T.unpack . enumToText . noiAdminState)
  , Column "CPUS" RightAlign (maybe "--" show . noiCpuCount)
  , Column "RAM_FREE" RightAlign (maybe "--" show . noiRamMbFree)
  , Column "DISK_FREE_GB" RightAlign $ \n ->
      maybe
        "--"
        (\b -> show ((fromIntegral b :: Integer) `div` (1024 * 1024 * 1024)))
        (noiStorageBytesFree n)
  , Column "LOAD1" RightAlign (maybe "--" (formatLoad . (\x -> x :: Double)) . noiLoadAvg1)
  ]
  where
    formatLoad x =
      let s = (round (x * 100) :: Int)
       in show (fromIntegral s / (100 :: Double))

-- | Suppress unused-import warning for 'M.NodeOnline'-style
-- module-qualified references the parser-side text → enum
-- conversion already covers — kept for documentation parity
-- with the @crv node@ man pages.
_unusedAdminStates :: NodeAdminState
_unusedAdminStates = fromMaybe M.NodeOnline (Just M.NodeOnline)
