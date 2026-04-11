{-# LANGUAGE OverloadedStrings #-}

-- | Virtual network management handlers.
module Corvus.Handlers.Network
  ( -- * Action types
    NetworkCreate (..)
  , NetworkDelete (..)
  , NetworkStart (..)
  , NetworkStop (..)
  , NetworkEdit (..)

    -- * Infrastructure Action types (subtasks of start/stop)
  , CreateBridge (..)
  , SetupNetworkNat (..)
  , StartDnsmasq (..)
  , TeardownNetworkNat (..)
  , StopDnsmasqAction (..)
  , DestroyBridge (..)

    -- * Handlers
  , handleNetworkCreate
  , handleNetworkDelete
  , handleNetworkStart
  , handleNetworkStop
  , handleNetworkList
  , handleNetworkShow
  , handleNetworkEdit
  )
where

import Corvus.Action

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Handlers.Resolve (validateName)
import Corvus.Model (Network (..), NetworkInterface (..), TaskId, TaskSubsystem (..), Vm (..), VmStatus (..))
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu.Netns.Manager (addNatRule, createBridge, destroyBridge, removeNatRule, startDnsmasq, stopDnsmasq)
import Corvus.Types (ServerState (..), runServerLogging)
import Corvus.Utils.Network (validateSubnet)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, toSqlKey)

--------------------------------------------------------------------------------
-- Network Handlers
--------------------------------------------------------------------------------

-- | Create a new virtual network
handleNetworkCreate :: ServerState -> Text -> Text -> Bool -> Bool -> Bool -> IO Response
handleNetworkCreate state name subnet dhcp nat autostart =
  case validateName "Network" name of
    Left err -> pure $ RespNetworkError err
    Right () -> do
      -- Validate subnet if provided
      let validatedSubnet
            | T.null subnet = Right ""
            | otherwise = validateSubnet subnet
      case validatedSubnet of
        Left err -> pure $ RespNetworkError $ "Invalid subnet: " <> err
        Right normalizedSubnet -> do
          -- DHCP requires a subnet; NAT requires a subnet
          if dhcp && T.null normalizedSubnet
            then pure $ RespNetworkError "DHCP requires a subnet"
            else
              if nat && T.null normalizedSubnet
                then pure $ RespNetworkError "NAT requires a subnet"
                else do
                  now <- getCurrentTime
                  let network =
                        Network
                          { networkName = name
                          , networkSubnet = normalizedSubnet
                          , networkDhcp = dhcp
                          , networkNat = nat
                          , networkRunning = False
                          , networkDnsmasqPid = Nothing
                          , networkCreatedAt = now
                          , networkAutostart = autostart
                          }
                  result <- runSqlPool (insertUnique network) (ssDbPool state)
                  case result of
                    Nothing -> pure $ RespNetworkError $ "Network with name '" <> name <> "' already exists"
                    Just key -> pure $ RespNetworkCreated $ fromSqlKey key

-- | Delete a virtual network
handleNetworkDelete :: ServerState -> Int64 -> IO Response
handleNetworkDelete state networkId = do
  let key = toSqlKey networkId :: M.NetworkId
  result <- runSqlPool (getAndCheckDelete key) (ssDbPool state)
  case result of
    Nothing -> pure RespNetworkNotFound
    Just False -> pure RespNetworkInUse
    Just True -> pure RespNetworkDeleted
  where
    getAndCheckDelete key = do
      mNetwork <- get key
      case mNetwork of
        Nothing -> pure Nothing
        Just network -> do
          -- Check if any network interfaces reference this network
          refs <- count [M.NetworkInterfaceNetworkId ==. Just key]
          if refs > 0
            then pure $ Just False
            else do
              -- Check network is not running
              if networkRunning network
                then pure $ Just False
                else do
                  delete key
                  pure $ Just True

-- | Start a virtual network (create bridge in namespace)
handleNetworkStart :: ServerState -> Int64 -> TaskId -> IO Response
handleNetworkStart state networkId parentTaskId = runServerLogging state $ do
  let key = toSqlKey networkId :: M.NetworkId
      pool = ssDbPool state
      nwName = T.pack (show networkId)
  mNetwork <- liftIO $ runSqlPool (get key) pool
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network
      | networkRunning network -> pure RespNetworkAlreadyRunning
      | otherwise -> do
          mNsPid <- liftIO $ readTVarIO (ssNamespacePid state)
          case mNsPid of
            Nothing -> do
              logWarnN "Network namespace is not running"
              pure $ RespNetworkError "Network namespace is not available"
            Just nsPid -> do
              logInfoN $ "Starting network " <> nwName

              -- Subtask 1: Create bridge
              bridgeResp <- liftIO $ runActionAsSubtask state (CreateBridge nsPid networkId (networkSubnet network)) parentTaskId
              case bridgeResp of
                RespError err -> do
                  logWarnN $ "Failed to create bridge for network " <> nwName <> ": " <> err
                  pure $ RespNetworkError err
                _ -> do
                  -- Subtask 2: Add NAT rules if enabled
                  when (networkNat network && not (T.null (networkSubnet network))) $ do
                    mPastaPid <- liftIO $ readTVarIO (ssPastaPid state)
                    case mPastaPid of
                      Nothing ->
                        logWarnN $ "NAT enabled but pasta is not running for network " <> nwName
                      Just _ -> do
                        natResp <- liftIO $ runActionAsSubtask state (SetupNetworkNat nsPid networkId (networkSubnet network)) parentTaskId
                        case natResp of
                          RespError err -> logWarnN $ "Failed to add NAT rules for network " <> nwName <> ": " <> err
                          _ -> logInfoN $ "NAT rules added for network " <> nwName

                  -- Subtask 3: Start dnsmasq if DHCP is enabled (also marks network as running)
                  if networkDhcp network && not (T.null (networkSubnet network))
                    then do
                      dnsmasqResp <- liftIO $ runActionAsSubtask state (StartDnsmasq nsPid networkId (networkSubnet network) (networkNat network)) parentTaskId
                      case dnsmasqResp of
                        RespError err -> do
                          logWarnN $ "Failed to start dnsmasq for network " <> nwName <> ": " <> err
                          pure $ RespNetworkError err
                        _ -> do
                          logInfoN $ "Network " <> nwName <> " started with DHCP"
                          pure RespNetworkStarted
                    else do
                      liftIO $ runSqlPool (update key [M.NetworkRunning =. True]) pool
                      logInfoN $ "Network " <> nwName <> " started (no DHCP)"
                      pure RespNetworkStarted

-- | Stop a virtual network
handleNetworkStop :: ServerState -> Int64 -> Bool -> TaskId -> IO Response
handleNetworkStop state networkId force parentTaskId = runServerLogging state $ do
  let key = toSqlKey networkId :: M.NetworkId
      pool = ssDbPool state
  mNetwork <- liftIO $ runSqlPool (get key) pool
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network
      | not (networkRunning network) -> pure RespNetworkNotRunning
      | otherwise -> do
          if not force
            then do
              hasRunningVms <- liftIO $ runSqlPool (checkRunningVms key) pool
              if hasRunningVms
                then pure RespNetworkInUse
                else doStop key network
            else doStop key network
  where
    doStop key network = do
      let nwName = networkName network
      logInfoN $ "Stopping network " <> T.pack (show networkId)
      mNsPid <- liftIO $ readTVarIO (ssNamespacePid state)

      -- Subtask 1: Remove NAT rules if enabled
      when (networkNat network) $ do
        case mNsPid of
          Just nsPid -> do
            natResp <- liftIO $ runActionAsSubtask state (TeardownNetworkNat nsPid networkId) parentTaskId
            case natResp of
              RespError err -> logWarnN $ "Failed to remove NAT rules: " <> err
              _ -> logInfoN $ "NAT rules removed for network " <> T.pack (show networkId)
          Nothing -> pure ()

      -- Subtask 2: Stop dnsmasq if running
      case networkDnsmasqPid network of
        Just dnsmasqPid -> do
          _ <- liftIO $ runActionAsSubtask state (StopDnsmasqAction dnsmasqPid) parentTaskId
          logInfoN $ "Stopped dnsmasq PID " <> T.pack (show dnsmasqPid)
        Nothing -> pure ()

      -- Subtask 3: Destroy bridge in namespace
      case mNsPid of
        Just nsPid -> do
          bridgeResp <- liftIO $ runActionAsSubtask state (DestroyBridge nsPid networkId) parentTaskId
          case bridgeResp of
            RespError err ->
              logWarnN $ "Failed to destroy bridge for network " <> T.pack (show networkId) <> ": " <> err
            _ -> pure ()
        Nothing -> pure ()

      -- Clear state
      liftIO $
        runSqlPool
          ( update key [M.NetworkRunning =. False, M.NetworkDnsmasqPid =. Nothing]
          )
          (ssDbPool state)
      logInfoN $ "Network " <> T.pack (show networkId) <> " stopped"
      pure RespNetworkStopped

    checkRunningVms key = do
      netIfs <- selectList [M.NetworkInterfaceNetworkId ==. Just key] []
      let vmKeys = map (networkInterfaceVmId . entityVal) netIfs
      vms <- mapM get vmKeys
      let runningVms = filter isRunning (zip vmKeys vms)
      pure $ not (null runningVms)

    isRunning (_, Just vm) = vmStatus vm == VmRunning || vmStatus vm == M.VmPaused
    isRunning (_, Nothing) = False

-- | List all virtual networks
handleNetworkList :: ServerState -> IO Response
handleNetworkList state = do
  networks <- runSqlPool (selectList [] [Asc M.NetworkName]) (ssDbPool state)
  pure $ RespNetworkList $ map toNetworkInfo networks

-- | Show virtual network details
handleNetworkShow :: ServerState -> Int64 -> IO Response
handleNetworkShow state networkId = do
  let key = toSqlKey networkId :: M.NetworkId
  mNetwork <- runSqlPool (get key) (ssDbPool state)
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network -> pure $ RespNetworkDetails $ toNetworkInfoWith networkId network

-- | Edit virtual network properties.
-- Subnet, DHCP, and NAT require the network to be stopped.
-- Autostart can be toggled regardless of state.
handleNetworkEdit :: ServerState -> Int64 -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO Response
handleNetworkEdit state networkId mSubnet mDhcp mNat mAutostart = do
  let key = toSqlKey networkId :: M.NetworkId
      pool = ssDbPool state
  mNetwork <- runSqlPool (get key) pool
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network -> do
      let hasNonAutostartEdits = isJust mSubnet || isJust mDhcp || isJust mNat
      if hasNonAutostartEdits && networkRunning network
        then pure $ RespNetworkError "Network must be stopped to change subnet/dhcp/nat"
        else do
          -- Validate and normalize subnet if provided
          mNormalizedSubnet <- case mSubnet of
            Just s
              | T.null s -> pure $ Right (Just "")
              | otherwise -> case validateSubnet s of
                  Left err -> pure $ Left $ "Invalid subnet: " <> err
                  Right normalized -> pure $ Right (Just normalized)
            Nothing -> pure $ Right Nothing
          case mNormalizedSubnet of
            Left err -> pure $ RespNetworkError err
            Right mSub -> do
              let effectiveSubnet = fromMaybe (networkSubnet network) mSub
                  effectiveDhcp = fromMaybe (networkDhcp network) mDhcp
                  effectiveNat = fromMaybe (networkNat network) mNat
              if effectiveDhcp && T.null effectiveSubnet
                then pure $ RespNetworkError "DHCP requires a subnet"
                else
                  if effectiveNat && T.null effectiveSubnet
                    then pure $ RespNetworkError "NAT requires a subnet"
                    else do
                      let updates =
                            maybe [] (\s -> [M.NetworkSubnet =. s]) mSub
                              ++ maybe [] (\d -> [M.NetworkDhcp =. d]) mDhcp
                              ++ maybe [] (\n -> [M.NetworkNat =. n]) mNat
                              ++ maybe [] (\a -> [M.NetworkAutostart =. a]) mAutostart
                      case updates of
                        [] -> pure RespNetworkEdited
                        us -> do
                          runSqlPool (update key us) pool
                          pure RespNetworkEdited

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

toNetworkInfo :: Entity Network -> NetworkInfo
toNetworkInfo (Entity key network) = toNetworkInfoWith (fromSqlKey key) network

toNetworkInfoWith :: Int64 -> Network -> NetworkInfo
toNetworkInfoWith nwId network =
  NetworkInfo
    { nwiId = nwId
    , nwiName = networkName network
    , nwiSubnet = networkSubnet network
    , nwiDhcp = networkDhcp network
    , nwiNat = networkNat network
    , nwiRunning = networkRunning network
    , nwiDnsmasqPid = networkDnsmasqPid network
    , nwiCreatedAt = networkCreatedAt network
    , nwiAutostart = networkAutostart network
    }

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

data NetworkCreate = NetworkCreate
  { ncrName :: Text
  , ncrSubnet :: Text
  , ncrDhcp :: Bool
  , ncrNat :: Bool
  , ncrAutostart :: Bool
  }

instance Action NetworkCreate where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "create"
  actionEntityName = Just . ncrName
  actionExecute ctx a = handleNetworkCreate (acState ctx) (ncrName a) (ncrSubnet a) (ncrDhcp a) (ncrNat a) (ncrAutostart a)

newtype NetworkDelete = NetworkDelete {ndelNetworkId :: Int64}

instance Action NetworkDelete where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "delete"
  actionEntityId = Just . fromIntegral . ndelNetworkId
  actionExecute ctx a = handleNetworkDelete (acState ctx) (ndelNetworkId a)

data NetworkEdit = NetworkEdit
  { nedNetworkId :: Int64
  , nedSubnet :: Maybe Text
  , nedDhcp :: Maybe Bool
  , nedNat :: Maybe Bool
  , nedAutostart :: Maybe Bool
  }

instance Action NetworkEdit where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "edit"
  actionEntityId = Just . fromIntegral . nedNetworkId
  actionExecute ctx a = handleNetworkEdit (acState ctx) (nedNetworkId a) (nedSubnet a) (nedDhcp a) (nedNat a) (nedAutostart a)

-- Complex handlers with subtask creation

newtype NetworkStart = NetworkStart {nstartNetworkId :: Int64}

instance Action NetworkStart where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "start"
  actionEntityId = Just . fromIntegral . nstartNetworkId
  actionExecute ctx a = handleNetworkStart (acState ctx) (nstartNetworkId a) (acTaskId ctx)

data NetworkStop = NetworkStop
  { nstopNetworkId :: Int64
  , nstopForce :: Bool
  }

instance Action NetworkStop where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "stop"
  actionEntityId = Just . fromIntegral . nstopNetworkId
  actionExecute ctx a = handleNetworkStop (acState ctx) (nstopNetworkId a) (nstopForce a) (acTaskId ctx)

-- Infrastructure subtask Actions for network start/stop

data CreateBridge = CreateBridge
  { cbNsPid :: Int
  , cbNetworkId :: Int64
  , cbSubnet :: Text
  }

instance Action CreateBridge where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "create-bridge"
  actionEntityId = Just . fromIntegral . cbNetworkId
  actionExecute ctx a = do
    result <- createBridge (cbNsPid a) (ssQemuConfig $ acState ctx) (cbNetworkId a) (cbSubnet a)
    pure $ either RespError (const RespOk) result

data SetupNetworkNat = SetupNetworkNat
  { snnNsPid :: Int
  , snnNetworkId :: Int64
  , snnSubnet :: Text
  }

instance Action SetupNetworkNat where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "setup-nat"
  actionEntityId = Just . fromIntegral . snnNetworkId
  actionExecute ctx a = do
    result <- addNatRule (snnNsPid a) (ssQemuConfig $ acState ctx) (snnNetworkId a) (snnSubnet a)
    pure $ either RespError (const RespOk) result

data StartDnsmasq = StartDnsmasq
  { sdqNsPid :: Int
  , sdqNetworkId :: Int64
  , sdqSubnet :: Text
  , sdqNat :: Bool
  }

instance Action StartDnsmasq where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "start-dnsmasq"
  actionEntityId = Just . fromIntegral . sdqNetworkId
  actionExecute ctx a = do
    let state = acState ctx
        key = toSqlKey (sdqNetworkId a) :: M.NetworkId
    result <- startDnsmasq (sdqNsPid a) (ssQemuConfig state) (sdqNetworkId a) (sdqSubnet a) (sdqNat a)
    case result of
      Left err -> pure $ RespError err
      Right dnsmasqPid -> do
        runSqlPool (update key [M.NetworkRunning =. True, M.NetworkDnsmasqPid =. Just dnsmasqPid]) (ssDbPool state)
        pure RespOk

data TeardownNetworkNat = TeardownNetworkNat
  { tnnNsPid :: Int
  , tnnNetworkId :: Int64
  }

instance Action TeardownNetworkNat where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "teardown-nat"
  actionEntityId = Just . fromIntegral . tnnNetworkId
  actionExecute ctx a = do
    result <- removeNatRule (tnnNsPid a) (ssQemuConfig $ acState ctx) (tnnNetworkId a)
    pure $ either RespError (const RespOk) result

newtype StopDnsmasqAction = StopDnsmasqAction {sdaPid :: Int}

instance Action StopDnsmasqAction where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "stop-dnsmasq"
  actionExecute _ctx a = do
    stopDnsmasq (sdaPid a)
    pure RespOk

data DestroyBridge = DestroyBridge
  { dbNsPid :: Int
  , dbNetworkId :: Int64
  }

instance Action DestroyBridge where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "destroy-bridge"
  actionEntityId = Just . fromIntegral . dbNetworkId
  actionExecute _ctx a = do
    result <- destroyBridge (dbNsPid a) (dbNetworkId a)
    pure $ either RespError (const RespOk) result
