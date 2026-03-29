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

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Model (Network (..), NetworkInterface (..), Vm (..), VmStatus (..))
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu.Vde (startVdeSwitch, stopVdeSwitch)
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
handleNetworkCreate :: ServerState -> Text -> Text -> IO Response
handleNetworkCreate state name subnet = do
  -- Validate subnet if provided
  let validatedSubnet
        | T.null subnet = Right ""
        | otherwise = validateSubnet subnet
  case validatedSubnet of
    Left err -> pure $ RespNetworkError $ "Invalid subnet: " <> err
    Right normalizedSubnet -> do
      now <- getCurrentTime
      let network =
            Network
              { networkName = name
              , networkSubnet = normalizedSubnet
              , networkVdeSwitchPid = Nothing
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
              case networkVdeSwitchPid network of
                Just _ -> pure $ Just False
                Nothing -> do
                  delete key
                  pure $ Just True

-- | Start a virtual network (launch vde_switch)
handleNetworkStart :: ServerState -> Int64 -> IO Response
handleNetworkStart state networkId = runServerLogging state $ do
  let key = toSqlKey networkId :: M.NetworkId
  mNetwork <- liftIO $ runSqlPool (get key) (ssDbPool state)
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network -> case networkVdeSwitchPid network of
      Just _ -> pure RespNetworkAlreadyRunning
      Nothing -> do
        logInfoN $ "Starting network " <> T.pack (show networkId)
        result <- liftIO $ startVdeSwitch (ssQemuConfig state) (ssDbPool state) networkId (networkSubnet network)
        case result of
          Left err -> do
            logWarnN $ "Failed to start network " <> T.pack (show networkId) <> ": " <> err
            pure $ RespNetworkError err
          Right () -> do
            logInfoN $ "Network " <> T.pack (show networkId) <> " started"
            pure RespNetworkStarted

-- | Stop a virtual network
handleNetworkStop :: ServerState -> Int64 -> Bool -> IO Response
handleNetworkStop state networkId force = runServerLogging state $ do
  let key = toSqlKey networkId :: M.NetworkId
  mNetwork <- liftIO $ runSqlPool (get key) (ssDbPool state)
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network -> case networkVdeSwitchPid network of
      Nothing -> pure RespNetworkNotRunning
      Just _ -> do
        -- Check for running VMs unless force
        unless force $ do
          hasRunningVms <- liftIO $ runSqlPool (checkRunningVms key) (ssDbPool state)
          when hasRunningVms $ do
            logWarnN $ "Network " <> T.pack (show networkId) <> " has running VMs attached"
        if not force
          then do
            hasRunningVms <- liftIO $ runSqlPool (checkRunningVms key) (ssDbPool state)
            if hasRunningVms
              then pure RespNetworkInUse
              else doStop networkId
          else doStop networkId
  where
    doStop nwId = do
      logInfoN $ "Stopping network " <> T.pack (show nwId)
      result <- liftIO $ stopVdeSwitch (ssQemuConfig state) (ssDbPool state) nwId
      case result of
        Left err -> do
          logWarnN $ "Failed to stop network " <> T.pack (show nwId) <> ": " <> err
          pure $ RespNetworkError err
        Right () -> do
          logInfoN $ "Network " <> T.pack (show nwId) <> " stopped"
          pure RespNetworkStopped

    checkRunningVms key = do
      -- Find network interfaces referencing this network
      netIfs <- selectList [M.NetworkInterfaceNetworkId ==. Just key] []
      -- Check if any of the VMs are running
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
    , nwiRunning = case networkVdeSwitchPid network of
        Just _ -> True
        Nothing -> False
    , nwiVdeSwitchPid = networkVdeSwitchPid network
    , nwiDnsmasqPid = networkDnsmasqPid network
    , nwiCreatedAt = networkCreatedAt network
    }
