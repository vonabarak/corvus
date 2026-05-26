-- | Network interface management handlers.
module Corvus.Handlers.NetIf
  ( -- * Action types
    NetIfAdd (..)
  , NetIfRemove (..)

    -- * Handlers
  , handleNetIfAdd
  , handleNetIfRemove
  , handleNetIfList

    -- * Validators
  , checkVmNodeAllowsNicType
  , checkNodeAllowsNicType
  )
where

import Corvus.Action

import Control.Monad.IO.Class (liftIO)
import qualified Corvus.Handlers.Network as NetworkH
import qualified Corvus.Handlers.Network.Ipam as Ipam
import Corvus.Model (NetInterfaceType (..), Network (..), NetworkInterface (..), TaskSubsystem (..), Vm (..), VmId, VmStatus (..))
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Types (ServerState (..))
import Corvus.Utils.Network (generateMacAddress)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
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
  let networkKey = toSqlKey <$> mNetworkId :: Maybe M.NetworkId
      -- When a network is specified, force managed type
      actualType = case mNetworkId of
        Just _ -> NetManaged
        Nothing -> ifaceType
  gateResult <-
    runSqlPool (checkVmNodeAllowsNicType (toSqlKey vmId :: VmId) actualType) (ssDbPool state)
  case gateResult of
    Just err -> pure $ RespError err
    Nothing -> case actualType of
      NetBridge
        | T.null hostDevice ->
            pure $
              RespError
                "bridge interface requires --host-device (the host bridge name)"
      _ -> do
        mac <- case mMacAddress of
          Just m | not (T.null m) -> pure m
          _ -> generateMacAddress
        result <- runSqlPool (addNetIf vmId actualType hostDevice mac networkKey) (ssDbPool state)
        case result of
          Nothing -> pure RespVmNotFound
          Just (Left err) -> pure $ RespError err
          Just (Right (netIfId, mNetworkKey, refreshNetworkSpec)) -> do
            -- The DB insert succeeded; if it created a NIC on a managed
            -- network whose IP allocation changed, push the updated
            -- spec out so dnsmasq picks up the new --dhcp-host
            -- reservation. Best-effort: a failure here doesn't fail the
            -- add (the supervisor reapply will catch up later).
            case (refreshNetworkSpec, mNetworkKey) of
              (True, Just key) -> do
                _ <- NetworkH.pushNetworkToAllMembers state key
                pure ()
              _ -> pure ()
            pure $ RespNetIfAdded netIfId

-- | Remove a network interface from a VM
handleNetIfRemove :: ServerState -> Int64 -> Int64 -> IO Response
handleNetIfRemove state vmId netIfId = do
  result <- runSqlPool (removeNetIf vmId netIfId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just False -> pure RespNetIfNotFound
    Just True -> pure RespOk

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

-- | Add a network interface to a VM. Returns @(netIfId,
-- networkKey, refreshNeeded)@ where @refreshNeeded@ is True when
-- the row landed on a managed network and the daemon should re-push
-- the network spec to all members (so the new DHCP reservation
-- propagates).
addNetIf
  :: Int64
  -> NetInterfaceType
  -> Text
  -> Text
  -> Maybe M.NetworkId
  -> SqlPersistT IO (Maybe (Either Text (Int64, Maybe M.NetworkId, Bool)))
addNetIf vmId ifaceType hostDevice macAddress mNetworkKey = do
  let vmKey = toSqlKey vmId :: VmId
  mVm <- get vmKey
  case mVm of
    Nothing -> pure Nothing
    Just vm -> do
      case mNetworkKey of
        Just nwKey -> do
          mNetwork <- get nwKey
          case mNetwork of
            Nothing -> pure $ Just $ Left "Network not found"
            Just nw -> do
              -- A managed NIC's VM may be on the owner OR on any
              -- peer node — the overlay (when present) carries
              -- traffic between them.
              isMember <- vmNodeIsNetworkMember nw nwKey (vmNodeId vm)
              if not isMember
                then
                  pure $
                    Just $
                      Left $
                        "VM's node is neither the owner nor a peer of network '"
                          <> networkName nw
                          <> "' (add it with `crv network attach-node`)"
                else allocateAndInsert vmKey nwKey nw
        Nothing -> doInsert vmKey Nothing Nothing
  where
    vmNodeIsNetworkMember nw nwKey vmNode
      | networkNodeId nw == vmNode = pure True
      | otherwise = do
          c <-
            count
              [ M.NetworkPeerNetworkId ==. nwKey
              , M.NetworkPeerNodeId ==. vmNode
              ]
          pure (c > 0)

    allocateAndInsert vmKey nwKey nw
      | T.null (networkSubnet nw) =
          -- No subnet → no IPAM. Keeps L2-only networks usable.
          doInsert vmKey (Just nwKey) Nothing
      | otherwise = do
          used <- allocatedIpsForNetwork nwKey
          case Ipam.allocateIp (networkSubnet nw) used of
            Left err -> pure $ Just $ Left err
            Right ip -> doInsert vmKey (Just nwKey) (Just ip)

    allocatedIpsForNetwork nwKey = do
      nics <- selectList [M.NetworkInterfaceNetworkId ==. Just nwKey] []
      pure $ mapMaybe (M.networkInterfaceIpAddress . entityVal) nics

    doInsert vmKey nwKey mIp = do
      let netIf =
            NetworkInterface
              { networkInterfaceVmId = vmKey
              , networkInterfaceInterfaceType = ifaceType
              , networkInterfaceHostDevice = hostDevice
              , networkInterfaceMacAddress = macAddress
              , networkInterfaceNetworkId = nwKey
              , networkInterfaceGuestIpAddresses = Nothing
              , networkInterfaceIpAddress = mIp
              }
      netIfKey <- insert netIf
      let needsRefresh = case (nwKey, mIp) of
            (Just _, Just _) -> True
            _ -> False
      pure $ Just $ Right (fromSqlKey netIfKey, nwKey, needsRefresh)

-- | Pre-condition for attaching a NIC of a given type to a VM:
-- if the VM's node has @netdDisabled = True@, only 'NetUser' and
-- 'NetVde' are allowed. Returns 'Nothing' when the placement is
-- accepted, or @Just err@ with a caller-displayable message.
checkVmNodeAllowsNicType :: VmId -> NetInterfaceType -> SqlPersistT IO (Maybe Text)
checkVmNodeAllowsNicType vmKey t = do
  mVm <- get vmKey
  case mVm of
    Nothing -> pure Nothing -- VM-not-found is surfaced later by the insert path.
    Just vm -> checkNodeAllowsNicType (vmNodeId vm) t

-- | As 'checkVmNodeAllowsNicType' but takes the node directly,
-- for paths that already know the placement target.
checkNodeAllowsNicType :: M.NodeId -> NetInterfaceType -> SqlPersistT IO (Maybe Text)
checkNodeAllowsNicType nid t = do
  mNode <- get nid
  case mNode of
    Just n
      | M.nodeNetdDisabled n && t `notElem` [NetUser, NetVde] ->
          pure $
            Just $
              "Network interface type '"
                <> M.enumToText t
                <> "' requires netd, but node '"
                <> M.nodeName n
                <> "' has netdDisabled=true. Only 'user' and 'vde' types are allowed."
    _ -> pure Nothing

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
          , niIpAddress = networkInterfaceIpAddress netIf
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
