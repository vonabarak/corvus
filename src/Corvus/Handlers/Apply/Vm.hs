{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.Apply.Vm (ApplyVmCreate (..)) where

import Control.Monad (forM_, when)
import Corvus.Action
import Corvus.Handlers.Apply.Resolve (resolveByName, resolveByNameFilter)
import Corvus.Handlers.Apply.Validation (effectiveCloudInit)
import Corvus.Handlers.CloudInit (RegenerateCloudInit (..))
import qualified Corvus.Handlers.NetIf as NetIfH
import Corvus.Handlers.Vm (VmCreate (..))
import Corvus.Model
import Corvus.Protocol
import Corvus.Schema.Apply
import Corvus.Schema.CloudInit (CloudInitConfigYaml (..))
import Corvus.Types
import Corvus.Utils.Network (generateMacAddress)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (fromSqlKey, insert_, runSqlPool, toSqlKey)

data ApplyVmCreate = ApplyVmCreate {avcKeyMap :: Map.Map Text Int64, avcDiskMap :: Map.Map Text Int64, avcNwMap :: Map.Map Text Int64, avcVm :: ApplyVm}
instance Action ApplyVmCreate where
  actionSubsystem _ = SubVm
  actionCommand _ = "create"
  actionEntityName = Just . avName . avcVm
  actionExecute ctx a = do
    result <- createOneVm ctx (avcKeyMap a) (avcDiskMap a) (avcNwMap a) (avcVm a)
    pure $ either RespError RespVmCreated result

createOneVm ctx keyMap diskMap nwMap v = do
  vmResult <- executeCreate ctx (VmCreate (avName v) (avNode v) (avCpuCount v) (avRamMb v) (avDescription v) (avHeadless v) (avGuestAgent v) (avTpm v) (effectiveCloudInit v) (avAutostart v) (avRebootQuirk v) (avCpuModel v)) (toSqlKey 0)
  case vmResult of
    Left err -> pure $ Left $ "VM '" <> avName v <> "': " <> err
    Right vmId -> createOneVmAttachments ctx keyMap diskMap nwMap v (toSqlKey vmId)
createOneVmAttachments ctx keyMap diskMap nwMap v vmId = do
  let state = acState ctx
  driveResult <- attachDrives state diskMap vmId (avDrives v) (avName v)
  case driveResult of
    Left err -> pure $ Left err
    Right () -> do
      niResult <- createNetIfs state nwMap vmId (avNetworkInterfaces v) (avName v) (avNode v)
      case niResult of
        Left err -> pure $ Left err
        Right () -> do
          forM_ (avSharedDirs v) $ \sd -> runSqlPool (insert_ SharedDir {sharedDirVmId = vmId, sharedDirPath = asdPath sd, sharedDirTag = asdTag sd, sharedDirCache = asdCache sd, sharedDirReadOnly = asdReadOnly sd}) (ssDbPool state)
          keyResult <- attachSshKeys state keyMap vmId (avSshKeys v) (avName v)
          case keyResult of
            Left err -> pure $ Left err
            Right () -> do
              forM_ (avCloudInitConfig v) $ \cic -> runSqlPool (insert_ $ CloudInit vmId (cicyUserData cic) (cicyNetworkConfig cic) (cicyInjectSshKeys cic)) (ssDbPool state)
              when (effectiveCloudInit v) $ do
                _ <- runAction state (acClientName ctx) (RegenerateCloudInit (fromSqlKey vmId) (avName v))
                pure ()
              pure $ Right $ fromSqlKey vmId
attachDrives state diskMap vmId drives vmName = go drives
  where
    go [] = pure $ Right ()
    go (d : ds) = do
      mDiskId <- resolveByName state UniqueDiskImageName diskMap (adrDisk d)
      case mDiskId of
        Nothing -> pure $ Left $ "VM '" <> vmName <> "': disk '" <> adrDisk d <> "' not found"
        Just diskId -> runSqlPool (insert_ Drive {driveVmId = vmId, driveDiskImageId = Just $ toSqlKey diskId, driveInterface = adrInterface d, driveMedia = adrMedia d, driveReadOnly = adrReadOnly d, driveCacheType = adrCacheType d, driveDiscard = adrDiscard d}) (ssDbPool state) >> go ds
createNetIfs state nwMap vmId netIfs vmName vmNodeRef = go netIfs
  where
    go [] = pure $ Right ()
    go (ni : nis) = case aniType ni of
      NetBridge | maybe True T.null (aniHostDevice ni) -> pure $ Left $ "VM '" <> vmName <> "': bridge interface requires hostDevice (the host bridge name)"
      _ -> do
        mGate <- runSqlPool (NetIfH.checkVmNodeAllowsNicType vmId $ aniType ni) (ssDbPool state)
        case mGate of
          Just err -> pure $ Left $ "VM '" <> vmName <> "': " <> err
          Nothing -> do
            case aniNetwork ni of
              Nothing -> doInsert ni nis Nothing
              Just nwName -> do
                mNetworkId <- resolveByNameFilter state (\nm -> [NetworkName ==. nm]) (\nid -> [NetworkNodeId ==. nid]) nwMap nwName vmNodeRef
                case mNetworkId of
                  Nothing -> pure $ Left $ "VM '" <> vmName <> "': network '" <> nwName <> "' not found"
                  Just nid -> doInsert ni nis (Just nid)
    doInsert ni nis networkId = do
      mac <- maybe generateMacAddress pure (aniMac ni)
      runSqlPool (insert_ NetworkInterface {networkInterfaceVmId = vmId, networkInterfaceInterfaceType = aniType ni, networkInterfaceHostDevice = fromMaybe "" (aniHostDevice ni), networkInterfaceMacAddress = mac, networkInterfaceNetworkId = fmap toSqlKey networkId, networkInterfaceGuestIpAddresses = Nothing, networkInterfaceIpAddress = Nothing}) (ssDbPool state)
      go nis
attachSshKeys state keyMap vmId keyNames vmName = go keyNames
  where
    go [] = pure $ Right ()
    go (kn : kns) = do
      mKeyId <- resolveByName state UniqueSshKeyName keyMap kn
      case mKeyId of
        Nothing -> pure $ Left $ "VM '" <> vmName <> "': SSH key '" <> kn <> "' not found"
        Just keyId -> runSqlPool (insert_ $ VmSshKey vmId (toSqlKey keyId)) (ssDbPool state) >> go kns
