-- | Network interface management handlers.
module Corvus.Handlers.NetIf
  ( -- * Action types
    NetIfAdd (..)
  , NetIfRemove (..)

    -- * Handlers
  , handleNetIfAdd
  , handleNetIfRemove
  , handleNetIfList
  )
where

import Corvus.Action

import Control.Monad.IO.Class (liftIO)
import Corvus.Model (NetInterfaceType (..), Network (..), NetworkInterface (..), TaskSubsystem (..), Vm, VmId, VmStatus (..))
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Types (ServerState (..))
import Corvus.Utils.Network (generateMacAddress)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, toSqlKey)

--------------------------------------------------------------------------------
-- Network Interface Handlers
--------------------------------------------------------------------------------

-- | Add a network interface to a VM.
-- If no MAC address is provided, a random one is generated with the QEMU OUI prefix.
handleNetIfAdd
  :: ServerState
  -> Int64
  -> NetInterfaceType
  -> Text
  -> Maybe Text
  -> Maybe Int64
  -> IO Response
handleNetIfAdd state vmId ifaceType hostDevice mMacAddress mNetworkId = do
  mac <- case mMacAddress of
    Just m | not (T.null m) -> pure m
    _ -> generateMacAddress
  let networkKey = toSqlKey <$> mNetworkId :: Maybe M.NetworkId
      -- When a network is specified, force managed type
      actualType = case mNetworkId of
        Just _ -> NetManaged
        Nothing -> ifaceType
  result <- runSqlPool (addNetIf vmId actualType hostDevice mac networkKey) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (Left err) -> pure $ RespError err
    Just (Right netIfId) -> pure $ RespNetIfAdded netIfId

-- | Remove a network interface from a VM
handleNetIfRemove :: ServerState -> Int64 -> Int64 -> IO Response
handleNetIfRemove state vmId netIfId = do
  result <- runSqlPool (removeNetIf vmId netIfId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just False -> pure RespNetIfNotFound
    Just True -> pure RespNetIfOk

-- | List network interfaces for a VM
handleNetIfList :: ServerState -> Int64 -> IO Response
handleNetIfList state vmId = do
  result <- runSqlPool (listNetIfs vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just netIfs -> pure $ RespNetIfList netIfs

--------------------------------------------------------------------------------
-- Database Operations
--------------------------------------------------------------------------------

-- | Add a network interface to a VM
addNetIf
  :: Int64
  -> NetInterfaceType
  -> Text
  -> Text
  -> Maybe M.NetworkId
  -> SqlPersistT IO (Maybe (Either Text Int64))
addNetIf vmId ifaceType hostDevice macAddress mNetworkKey = do
  let vmKey = toSqlKey vmId :: VmId
  mVm <- get vmKey
  case mVm of
    Nothing -> pure Nothing
    Just _ -> do
      -- Validate network exists if specified
      case mNetworkKey of
        Just nwKey -> do
          mNetwork <- get nwKey
          case mNetwork of
            Nothing -> pure $ Just $ Left "Network not found"
            Just _ -> doInsert vmKey mNetworkKey
        Nothing -> doInsert vmKey Nothing
  where
    doInsert vmKey nwKey = do
      let netIf =
            NetworkInterface
              { networkInterfaceVmId = vmKey
              , networkInterfaceInterfaceType = ifaceType
              , networkInterfaceHostDevice = hostDevice
              , networkInterfaceMacAddress = macAddress
              , networkInterfaceNetworkId = nwKey
              , networkInterfaceGuestIpAddresses = Nothing
              }
      netIfKey <- insert netIf
      pure $ Just $ Right $ fromSqlKey netIfKey

-- | Remove a network interface from a VM
removeNetIf :: Int64 -> Int64 -> SqlPersistT IO (Maybe Bool)
removeNetIf vmId netIfId = do
  let vmKey = toSqlKey vmId :: VmId
      netIfKey = toSqlKey netIfId :: M.NetworkInterfaceId
  mVm <- get vmKey
  case mVm of
    Nothing -> pure Nothing
    Just _ -> do
      mNetIf <- get netIfKey
      case mNetIf of
        Nothing -> pure $ Just False
        Just netIf ->
          if networkInterfaceVmId netIf /= vmKey
            then pure $ Just False
            else do
              delete netIfKey
              pure $ Just True

-- | List network interfaces for a VM
listNetIfs :: Int64 -> SqlPersistT IO (Maybe [NetIfInfo])
listNetIfs vmId = do
  let vmKey = toSqlKey vmId :: VmId
  mVm <- get vmKey
  case mVm of
    Nothing -> pure Nothing
    Just _ -> do
      netIfs <- selectList [M.NetworkInterfaceVmId ==. vmKey] []
      infos <- mapM toNetIfInfo netIfs
      pure $ Just infos
  where
    toNetIfInfo (Entity key netIf) = do
      mNetworkName <- case networkInterfaceNetworkId netIf of
        Nothing -> pure Nothing
        Just nwKey -> do
          mNetwork <- get nwKey
          pure $ networkName <$> mNetwork
      pure
        NetIfInfo
          { niId = fromSqlKey key
          , niType = networkInterfaceInterfaceType netIf
          , niHostDevice = networkInterfaceHostDevice netIf
          , niMacAddress = networkInterfaceMacAddress netIf
          , niNetworkId = fromSqlKey <$> networkInterfaceNetworkId netIf
          , niNetworkName = mNetworkName
          , niGuestIpAddresses = networkInterfaceGuestIpAddresses netIf
          }

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

data NetIfAdd = NetIfAdd
  { niaVmId :: Int64
  , niaType :: NetInterfaceType
  , niaHostDevice :: Text
  , niaMacAddress :: Maybe Text
  , niaNetworkId :: Maybe Int64
  }

instance Action NetIfAdd where
  actionSubsystem _ = SubVm
  actionCommand _ = "add-netif"
  actionEntityId = Just . fromIntegral . niaVmId
  actionExecute ctx a = handleNetIfAdd (acState ctx) (niaVmId a) (niaType a) (niaHostDevice a) (niaMacAddress a) (niaNetworkId a)

data NetIfRemove = NetIfRemove
  { nirVmId :: Int64
  , nirNetIfId :: Int64
  }

instance Action NetIfRemove where
  actionSubsystem _ = SubVm
  actionCommand _ = "remove-netif"
  actionEntityId = Just . fromIntegral . nirVmId
  actionExecute ctx a = handleNetIfRemove (acState ctx) (nirVmId a) (nirNetIfId a)
