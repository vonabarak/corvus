{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Client command handlers for virtual network operations.
module Corvus.Client.Commands.Network
  ( handleNetworkCreate
  , handleNetworkDelete
  , handleNetworkStart
  , handleNetworkStop
  , handleNetworkList
  , handleNetworkShow
  , handleNetworkEdit
  , handleNetworkAttachNode
  , handleNetworkDetachNode
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output
import Corvus.Client.Types (OutputFormat)
import Corvus.Protocol (NetworkInfo (..))
import Corvus.Wire.Common (entityRefFromText)
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T

-- | Handle network create command
handleNetworkCreate
  :: OutputFormat
  -> CapnpConnection
  -> Text
  -- ^ name
  -> Text
  -- ^ node ref
  -> Text
  -- ^ subnet
  -> Bool
  -> Bool
  -> Bool
  -> [Text]
  -- ^ DNS servers
  -> Text
  -- ^ domain ("" = default to name)
  -> Bool
  -- ^ hostDns
  -> IO Bool
handleNetworkCreate fmt conn name nodeRef subnet dhcp nat autostart dnsServers domain hostDns = do
  r <-
    try @SomeException
      (CR.rpcNetworkCreate conn name nodeRef subnet dhcp nat autostart dnsServers domain hostDns)
  case r of
    Right nwId -> do
      emitOkWith fmt [("id", toJSON nwId)] $
        putStrLn $
          "Network created with ID: " ++ show nwId
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Failed to create network: " ++ show e)
      pure False

-- | Handle network delete command
handleNetworkDelete :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleNetworkDelete fmt conn nwRef = do
  r <- try (CR.rpcNetworkDelete conn (entityRefFromText nwRef)) :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "Network deleted."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle network start command
handleNetworkStart :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleNetworkStart fmt conn nwRef = do
  r <- try (CR.rpcNetworkStart conn (entityRefFromText nwRef)) :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "Network started."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Failed to start network: " ++ show e)
      pure False

-- | Handle network stop command
handleNetworkStop :: OutputFormat -> CapnpConnection -> Text -> Bool -> IO Bool
handleNetworkStop fmt conn nwRef force = do
  r <- try (CR.rpcNetworkStop conn (entityRefFromText nwRef) force) :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "Network stopped."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Failed to stop network: " ++ show e)
      pure False

-- | Handle network list command
handleNetworkList :: OutputFormat -> TableOpts -> CapnpConnection -> IO Bool
handleNetworkList fmt tableOpts conn = do
  r <- try @SomeException (CR.rpcNetworkList conn)
  case r of
    Right networks -> do
      emitResult fmt networks $
        if null networks
          then putStrLn "No networks found."
          else printTable tableOpts networkColumns networks
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle network show command
handleNetworkShow :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleNetworkShow fmt conn nwRef = do
  r <- try @SomeException (CR.rpcNetworkShow conn (entityRefFromText nwRef))
  case r of
    Right info -> do
      emitResult fmt info $ do
        printField "ID" (show (nwiId info))
        printField "Name" (T.unpack (nwiName info))
        let sub = nwiSubnet info
        unless (T.null sub) $
          printField "Subnet" (T.unpack sub)
        printField "DHCP" (if nwiDhcp info then "enabled" else "disabled")
        printField "NAT" (if nwiNat info then "enabled" else "disabled")
        printField "Status" (if nwiRunning info then "running" else "stopped")
        printField "Autostart" (if nwiAutostart info then "enabled" else "disabled")
        case nwiDnsmasqPid info of
          Just pid -> printField "DHCP PID" (show pid)
          Nothing -> pure ()
        case nwiVni info of
          Just v -> printField "VXLAN VNI" (show v)
          Nothing -> pure ()
        case nwiPeerNodeIds info of
          [] -> pure ()
          ps ->
            printField
              "Peer nodes"
              (unwords (map show ps))
        case nwiDnsServers info of
          [] -> pure ()
          ds ->
            printField
              "DNS servers"
              (unwords (map T.unpack ds))
        let dom = nwiDomain info
        unless (T.null dom) $
          printField "Domain" (T.unpack dom)
        printField "Host DNS" (if nwiHostDns info then "enabled" else "disabled")
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Column definitions for the @network list@ table.
networkColumns :: [Column NetworkInfo]
networkColumns =
  [ Column "ID" RightAlign (show . nwiId)
  , Column "NAME" LeftAlign (T.unpack . nwiName)
  , Column "SUBNET" LeftAlign (\i -> let s = nwiSubnet i in if T.null s then "-" else T.unpack s)
  , Column "DHCP" LeftAlign (\i -> if nwiDhcp i then "yes" else "no")
  , Column "NAT" LeftAlign (\i -> if nwiNat i then "yes" else "no")
  , Column "STATUS" LeftAlign (\i -> if nwiRunning i then "running" else "stopped")
  , Column "PEERS" RightAlign (show . length . nwiPeerNodeIds)
  , Column "AS" LeftAlign (\i -> if nwiAutostart i then "+" else "-")
  ]

-- | Handle network edit command
handleNetworkEdit
  :: OutputFormat
  -> CapnpConnection
  -> Text
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe [Text]
  -> Maybe Text
  -> Maybe Bool
  -> IO Bool
handleNetworkEdit fmt conn nwRef mSubnet mDhcp mNat mAutostart mDnsServers mDomain mHostDns = do
  r <-
    try
      ( CR.rpcNetworkEdit
          conn
          (entityRefFromText nwRef)
          mSubnet
          mDhcp
          mNat
          mAutostart
          mDnsServers
          mDomain
          mHostDns
      )
      :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn $ "Network '" ++ T.unpack nwRef ++ "' updated."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Failed to edit network: " ++ show e)
      pure False

-- | Handle network attach-node command
handleNetworkAttachNode
  :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
handleNetworkAttachNode fmt conn nwRef nodeRef = do
  r <-
    try
      ( CR.rpcNetworkAttachNode
          conn
          (entityRefFromText nwRef)
          (entityRefFromText nodeRef)
      )
      :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $
        putStrLn $
          "Node '" ++ T.unpack nodeRef ++ "' attached to network '" ++ T.unpack nwRef ++ "'."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Failed to attach node: " ++ show e)
      pure False

-- | Handle network detach-node command
handleNetworkDetachNode
  :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
handleNetworkDetachNode fmt conn nwRef nodeRef = do
  r <-
    try
      ( CR.rpcNetworkDetachNode
          conn
          (entityRefFromText nwRef)
          (entityRefFromText nodeRef)
      )
      :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $
        putStrLn $
          "Node '" ++ T.unpack nodeRef ++ "' detached from network '" ++ T.unpack nwRef ++ "'."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Failed to detach node: " ++ show e)
      pure False
