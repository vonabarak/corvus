{-# LANGUAGE OverloadedStrings #-}

-- | Virtual network management handlers.
module Corvus.Handlers.Network
  ( handleNetworkCreate
  , handleNetworkDelete
  , handleNetworkStart
  , handleNetworkStop
  , handleNetworkList
  , handleNetworkShow
  )
where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Handlers.Resolve (validateName)
import Corvus.Model (Network (..), NetworkInterface (..), Vm (..), VmStatus (..))
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu.Netns.Manager (createBridge, destroyBridge, startDnsmasq, stopDnsmasq)
import Corvus.Types (ServerState (..), runServerLogging)
import Corvus.Utils.Subnet (validateSubnet)
import Data.Int (Int64)
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
handleNetworkCreate :: ServerState -> Text -> Text -> Bool -> IO Response
handleNetworkCreate state name subnet dhcp =
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
          -- DHCP requires a subnet
          if dhcp && T.null normalizedSubnet
            then pure $ RespNetworkError "DHCP requires a subnet"
            else do
              now <- getCurrentTime
              let network =
                    Network
                      { networkName = name
                      , networkSubnet = normalizedSubnet
                      , networkDhcp = dhcp
                      , networkRunning = False
                      , networkDnsmasqPid = Nothing
                      , networkCreatedAt = now
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
handleNetworkStart :: ServerState -> Int64 -> IO Response
handleNetworkStart state networkId = runServerLogging state $ do
  let key = toSqlKey networkId :: M.NetworkId
  mNetwork <- liftIO $ runSqlPool (get key) (ssDbPool state)
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network
      | networkRunning network -> pure RespNetworkAlreadyRunning
      | otherwise -> do
          -- Get namespace PID
          mNsPid <- liftIO $ readTVarIO (ssNamespacePid state)
          case mNsPid of
            Nothing -> do
              logWarnN "Network namespace is not running"
              pure $ RespNetworkError "Network namespace is not available"
            Just nsPid -> do
              logInfoN $ "Starting network " <> T.pack (show networkId)
              -- Create bridge in namespace
              bridgeResult <- liftIO $ createBridge nsPid (ssQemuConfig state) networkId (networkSubnet network)
              case bridgeResult of
                Left err -> do
                  logWarnN $ "Failed to create bridge for network " <> T.pack (show networkId) <> ": " <> err
                  pure $ RespNetworkError err
                Right () -> do
                  -- Start dnsmasq if DHCP is enabled
                  if networkDhcp network && not (T.null (networkSubnet network))
                    then do
                      dnsmasqResult <- liftIO $ startDnsmasq nsPid (ssQemuConfig state) networkId (networkSubnet network)
                      case dnsmasqResult of
                        Left err -> do
                          logWarnN $ "Failed to start dnsmasq for network " <> T.pack (show networkId) <> ": " <> err
                          pure $ RespNetworkError err
                        Right dnsmasqPid -> do
                          liftIO $
                            runSqlPool
                              ( update
                                  key
                                  [ M.NetworkRunning =. True
                                  , M.NetworkDnsmasqPid =. Just dnsmasqPid
                                  ]
                              )
                              (ssDbPool state)
                          logInfoN $ "Network " <> T.pack (show networkId) <> " started with DHCP (dnsmasq PID " <> T.pack (show dnsmasqPid) <> ")"
                          pure RespNetworkStarted
                    else do
                      liftIO $
                        runSqlPool
                          (update key [M.NetworkRunning =. True])
                          (ssDbPool state)
                      logInfoN $ "Network " <> T.pack (show networkId) <> " started (no DHCP)"
                      pure RespNetworkStarted

-- | Stop a virtual network
handleNetworkStop :: ServerState -> Int64 -> Bool -> IO Response
handleNetworkStop state networkId force = runServerLogging state $ do
  let key = toSqlKey networkId :: M.NetworkId
  mNetwork <- liftIO $ runSqlPool (get key) (ssDbPool state)
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network
      | not (networkRunning network) -> pure RespNetworkNotRunning
      | otherwise -> do
          -- Check for running VMs unless force
          if not force
            then do
              hasRunningVms <- liftIO $ runSqlPool (checkRunningVms key) (ssDbPool state)
              if hasRunningVms
                then pure RespNetworkInUse
                else doStop key network
            else doStop key network
  where
    doStop key network = do
      logInfoN $ "Stopping network " <> T.pack (show networkId)
      mNsPid <- liftIO $ readTVarIO (ssNamespacePid state)

      -- Stop dnsmasq if running
      case networkDnsmasqPid network of
        Just dnsmasqPid -> do
          liftIO $ stopDnsmasq dnsmasqPid
          logInfoN $ "Stopped dnsmasq PID " <> T.pack (show dnsmasqPid)
        Nothing -> pure ()

      -- Destroy bridge in namespace
      case mNsPid of
        Just nsPid -> do
          bridgeResult <- liftIO $ destroyBridge nsPid networkId
          case bridgeResult of
            Left err ->
              logWarnN $ "Failed to destroy bridge for network " <> T.pack (show networkId) <> ": " <> err
            Right () -> pure ()
        Nothing -> pure ()

      -- Clear state
      liftIO $
        runSqlPool
          ( update
              key
              [ M.NetworkRunning =. False
              , M.NetworkDnsmasqPid =. Nothing
              ]
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
    , nwiRunning = networkRunning network
    , nwiDnsmasqPid = networkDnsmasqPid network
    , nwiCreatedAt = networkCreatedAt network
    }
