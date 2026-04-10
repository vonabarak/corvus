{-# LANGUAGE OverloadedStrings #-}

-- | Virtual network management handlers.
module Corvus.Handlers.Network
  ( handleNetworkCreate
  , handleNetworkDelete
  , handleNetworkStart
  , handleNetworkStop
  , handleNetworkList
  , handleNetworkShow
  , handleNetworkEdit
  )
where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Handlers.Resolve (validateName)
import Corvus.Handlers.Subtask (SubtaskSpec (..), withOptionalSubtask)
import Corvus.Model (Network (..), NetworkInterface (..), TaskId, TaskSubsystem (..), Vm (..), VmStatus (..))
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu.Netns.Manager (addNatRule, createBridge, destroyBridge, removeNatRule, startDnsmasq, stopDnsmasq)
import Corvus.Types (ServerState (..), runServerLogging)
import Corvus.Utils.Subnet (validateSubnet)
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
              bridgeResult <- liftIO $ do
                let spec = SubtaskSpec SubNetwork "create-bridge" (Just $ networkName network)
                withOptionalSubtask
                  pool
                  (Just parentTaskId)
                  spec
                  (createBridge nsPid (ssQemuConfig state) networkId (networkSubnet network))
                  (const Nothing)
              case bridgeResult of
                Left err -> do
                  logWarnN $ "Failed to create bridge for network " <> nwName <> ": " <> err
                  pure $ RespNetworkError err
                Right () -> do
                  -- Subtask 2: Add NAT rules if enabled
                  when (networkNat network && not (T.null (networkSubnet network))) $ do
                    mPastaPid <- liftIO $ readTVarIO (ssPastaPid state)
                    case mPastaPid of
                      Nothing ->
                        logWarnN $ "NAT enabled but pasta is not running for network " <> nwName
                      Just _ -> do
                        natResult <- liftIO $ do
                          let spec = SubtaskSpec SubNetwork "setup-nat" (Just $ networkName network)
                          withOptionalSubtask
                            pool
                            (Just parentTaskId)
                            spec
                            (addNatRule nsPid (ssQemuConfig state) networkId (networkSubnet network))
                            (const Nothing)
                        case natResult of
                          Left err -> logWarnN $ "Failed to add NAT rules for network " <> nwName <> ": " <> err
                          Right () -> logInfoN $ "NAT rules added for network " <> nwName

                  -- Subtask 3: Start dnsmasq if DHCP is enabled
                  if networkDhcp network && not (T.null (networkSubnet network))
                    then do
                      dnsmasqResult <- liftIO $ do
                        let spec = SubtaskSpec SubNetwork "start-dnsmasq" (Just $ networkName network)
                        withOptionalSubtask
                          pool
                          (Just parentTaskId)
                          spec
                          (startDnsmasq nsPid (ssQemuConfig state) networkId (networkSubnet network) (networkNat network))
                          (Just . fromIntegral)
                      case dnsmasqResult of
                        Left err -> do
                          logWarnN $ "Failed to start dnsmasq for network " <> nwName <> ": " <> err
                          pure $ RespNetworkError err
                        Right dnsmasqPid -> do
                          liftIO $
                            runSqlPool
                              ( update key [M.NetworkRunning =. True, M.NetworkDnsmasqPid =. Just dnsmasqPid]
                              )
                              pool
                          logInfoN $ "Network " <> nwName <> " started with DHCP (dnsmasq PID " <> T.pack (show dnsmasqPid) <> ")"
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
      let pool = ssDbPool state
          nwName = networkName network
      logInfoN $ "Stopping network " <> T.pack (show networkId)
      mNsPid <- liftIO $ readTVarIO (ssNamespacePid state)

      -- Subtask 1: Remove NAT rules if enabled
      when (networkNat network) $ do
        case mNsPid of
          Just nsPid -> do
            natResult <- liftIO $ do
              let spec = SubtaskSpec SubNetwork "teardown-nat" (Just nwName)
              withOptionalSubtask
                pool
                (Just parentTaskId)
                spec
                (removeNatRule nsPid (ssQemuConfig state) networkId)
                (const Nothing)
            case natResult of
              Left err -> logWarnN $ "Failed to remove NAT rules: " <> err
              Right () -> logInfoN $ "NAT rules removed for network " <> T.pack (show networkId)
          Nothing -> pure ()

      -- Subtask 2: Stop dnsmasq if running
      case networkDnsmasqPid network of
        Just dnsmasqPid -> do
          liftIO $ do
            let spec = SubtaskSpec SubNetwork "stop-dnsmasq" (Just nwName)
            withOptionalSubtask
              pool
              (Just parentTaskId)
              spec
              (stopDnsmasq dnsmasqPid >> pure (Right ()))
              (const Nothing)
          logInfoN $ "Stopped dnsmasq PID " <> T.pack (show dnsmasqPid)
        Nothing -> pure ()

      -- Subtask 3: Destroy bridge in namespace
      case mNsPid of
        Just nsPid -> do
          bridgeResult <- liftIO $ do
            let spec = SubtaskSpec SubNetwork "destroy-bridge" (Just nwName)
            withOptionalSubtask
              pool
              (Just parentTaskId)
              spec
              (destroyBridge nsPid networkId)
              (const Nothing)
          case bridgeResult of
            Left err ->
              logWarnN $ "Failed to destroy bridge for network " <> T.pack (show networkId) <> ": " <> err
            Right () -> pure ()
        Nothing -> pure ()

      -- Clear state
      liftIO $
        runSqlPool
          ( update key [M.NetworkRunning =. False, M.NetworkDnsmasqPid =. Nothing]
          )
          pool
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
