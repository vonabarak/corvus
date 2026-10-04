{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Network interface command handlers for the Corvus client.
module Corvus.Client.Commands.NetIf
  ( -- * Command handlers
    handleNetIfAdd
  , handleNetIfEdit
  , handleNetIfRemove
  , handleNetIfList

    -- * Parsers
  , parseNetInterfaceType
  )
where

import Control.Exception (SomeException, try)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output (Align (..), Column (..), TableOpts, emitError, emitOk, emitOkWith, emitResult, emitRpcError, printTable)
import Corvus.Client.Types (OutputFormat)
import Corvus.Model (EnumText (..), NetInterfaceType, NetworkDeviceModel)
import Corvus.Protocol (NetIfInfo (..))
import Corvus.Wire.Common (entityRefFromText)
import Data.Aeson (toJSON)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T

-- | Parse network interface type string to NetInterfaceType
parseNetInterfaceType :: Text -> Either Text NetInterfaceType
parseNetInterfaceType = enumFromText

-- | Handle network interface add command
handleNetIfAdd :: OutputFormat -> CapnpConnection -> Text -> NetInterfaceType -> Text -> Maybe Text -> Maybe Text -> Text -> IO Bool
handleNetIfAdd fmt conn vmRef ifaceType hostDevice macAddress mNetworkRef modelText = do
  let mNetEnt = fmap entityRefFromText mNetworkRef
  case enumFromText modelText of
    Left err -> emitError fmt "invalid_network_model" err (putStrLn (T.unpack err)) >> pure False
    Right model -> do
      r <- try @SomeException (CR.rpcNetIfAdd conn (entityRefFromText vmRef) ifaceType model hostDevice macAddress mNetEnt)
      case r of
        Right nid -> do
          emitOkWith fmt [("id", toJSON nid)] $
            putStrLn $
              "Network interface added with ID: " ++ show nid
          pure True
        Left e -> do
          emitRpcError fmt e $
            putStrLn ("Failed to add network interface: " ++ show e)
          pure False

handleNetIfEdit :: OutputFormat -> CapnpConnection -> Text -> Int64 -> Text -> IO Bool
handleNetIfEdit fmt conn vmRef netIfId modelText =
  case enumFromText modelText :: Either Text NetworkDeviceModel of
    Left err -> emitError fmt "invalid_network_model" err (putStrLn (T.unpack err)) >> pure False
    Right model -> do
      r <- try @SomeException (CR.rpcNetIfEdit conn (entityRefFromText vmRef) netIfId model)
      case r of
        Right () -> emitOk fmt (putStrLn "Network interface updated.") >> pure True
        Left err -> emitRpcError fmt err (print err) >> pure False

-- | Handle network interface remove command
handleNetIfRemove :: OutputFormat -> CapnpConnection -> Text -> Int64 -> IO Bool
handleNetIfRemove fmt conn vmRef netIfId = do
  r <- try (CR.rpcNetIfRemove conn (entityRefFromText vmRef) netIfId) :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "Network interface removed."
      pure True
    Left e -> do
      emitRpcError fmt e $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle network interface list command
handleNetIfList :: OutputFormat -> TableOpts -> CapnpConnection -> Text -> IO Bool
handleNetIfList fmt tableOpts conn vmRef = do
  r <- try @SomeException (CR.rpcNetIfList conn (entityRefFromText vmRef))
  case r of
    Right netIfs -> do
      emitResult fmt netIfs $
        if null netIfs
          then putStrLn "No network interfaces found for this VM."
          else printTable tableOpts netIfColumns netIfs
      pure True
    Left e -> do
      emitRpcError fmt e $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Column definitions for the @net-if list@ table.
netIfColumns :: [Column NetIfInfo]
netIfColumns =
  [ Column "ID" RightAlign (show . niId)
  , Column "MODEL" LeftAlign (T.unpack . enumToText . niModel)
  , Column "TYPE" LeftAlign (T.unpack . enumToText . niType)
  , Column "DEVICE" LeftAlign (T.unpack . niHostDevice)
  , Column "MAC" LeftAlign (T.unpack . niMacAddress)
  , Column "GUEST_IPS" LeftAlign (maybe "" T.unpack . niGuestIpAddresses)
  ]
