{-# LANGUAGE OverloadedStrings #-}

-- | Compare declarative state, excluding daemon-owned runtime fields.
module Corvus.Handlers.Apply.Identity (matchesKey, matchesNetwork, matchesVm, matchesTemplate) where

import Control.Monad (join)
import Corvus.DiskSelector
import Corvus.Handlers.Apply.Resolve
import Corvus.Handlers.Apply.Validation (effectiveCloudInit)
import Corvus.Images (resolveImage)
import Corvus.Model
import Corvus.Schema.Apply
import Corvus.Schema.CloudInit
import Corvus.Schema.Template
import Corvus.Types
import Data.Int (Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql

matchesKey :: ServerState -> ApplySshKey -> Int64 -> IO Bool
matchesKey s k i = runSqlPool (maybe False ((== askPublicKey k) . sshKeyPublicKey) <$> get (toSqlKey i)) (ssDbPool s)

matchesNetwork :: ServerState -> ApplyNetwork -> Int64 -> IO Bool
matchesNetwork s n i =
  runSqlPool
    ( do
        old <- get (toSqlKey i)
        pure $ maybe False (\o -> o == o {networkSubnet = anSubnet n, networkDhcp = anDhcp n, networkNat = anNat n, networkAutostart = anAutostart n, networkDnsServers = T.intercalate "," (anDnsServers n), networkDomain = anDomain n, networkHostDns = anHostDns n}) old
    )
    (ssDbPool s)

matchesVm :: ServerState -> Map.Map Text Int64 -> Map.Map Text Int64 -> Map.Map Text Int64 -> ApplyVm -> Int64 -> IO (Either Text Bool)
matchesVm s km dm nm v i = do
  disks <- mapM (resolveDiskName s dm . adrDisk) (avDrives v)
  keys <- mapM (resolveByName s UniqueSshKeyName km) (avSshKeys v)
  networks <- mapM (traverse (\name -> resolveByNameFilter s (\x -> [NetworkName ==. x]) (\x -> [NetworkNodeId ==. x]) nm name (avNode v)) . aniNetwork) (avNetworkInterfaces v)
  if elem Nothing disks || elem Nothing keys || elem (Just Nothing) networks
    then pure $ Left "VM dependency not found"
    else
      runSqlPool
        ( do
            let vid = toSqlKey i
            old <- get vid
            drives <- map entityVal <$> selectList [DriveVmId ==. vid] [Asc DriveId]
            -- Only the daemon-generated cloud-init CD is omitted from user drives.
            userDrives <- filterMDrive vid drives
            nics <- map entityVal <$> selectList [NetworkInterfaceVmId ==. vid] [Asc NetworkInterfaceId]
            dirs <- map entityVal <$> selectList [SharedDirVmId ==. vid] [Asc SharedDirTag]
            audio <- map entityVal <$> selectList [AudioDeviceVmId ==. vid] [Asc AudioDeviceId]
            actualKeys <- map (fromSqlKey . vmSshKeySshKeyId . entityVal) <$> selectList [VmSshKeyVmId ==. vid] []
            ci <- fmap entityVal <$> getBy (UniqueCloudInitVm vid)
            let expectedDrives = zipWith (\d disk -> Drive vid (toSqlKey <$> disk) (adrInterface d) (adrMedia d) (adrReadOnly d) (adrCacheType d) (adrDiscard d)) (avDrives v) disks
                nicMatches o (n, net) = o == o {networkInterfaceInterfaceType = aniType n, networkInterfaceModel = aniModel n, networkInterfaceHostDevice = fromMaybe "" (aniHostDevice n), networkInterfaceMacAddress = fromMaybe (networkInterfaceMacAddress o) (aniMac n), networkInterfaceNetworkId = toSqlKey <$> join net}
                expectedDirs = L.sortOn sharedDirTag [SharedDir vid (asdPath d) (asdTag d) (asdCache d) (asdReadOnly d) | d <- avSharedDirs v]
                expectedAudio = [AudioDevice vid (tadyBackend a) (tadyModel a) (tadyOptions a) | a <- avAudioDevices v]
                expectedCi = (\c -> CloudInit vid (cicyUserData c) (cicyNetworkConfig c) (cicyInjectSshKeys c)) <$> avCloudInitConfig v
                scalar o = o == o {vmCpuCount = avCpuCount v, vmRam = avRam v, vmDescription = avDescription v, vmHeadless = avHeadless v, vmGuestAgent = avGuestAgent v, vmTpm = avTpm v, vmCloudInit = effectiveCloudInit v, vmAutostart = avAutostart v, vmRebootQuirk = avRebootQuirk v, vmCpuModel = avCpuModel v, vmGraphicsAdapter = avGraphicsAdapter v, vmVsock = avVsock v, vmBalloon = avBalloon v, vmRng = avRng v, vmStartupTimeoutSec = avStartupTimeoutSec v}
            pure $ Right $ maybe False scalar old && userDrives == expectedDrives && length nics == length networks && and (zipWith nicMatches nics (zip (avNetworkInterfaces v) networks)) && dirs == expectedDirs && audio == expectedAudio && L.sort actualKeys == L.sort (catMaybes keys) && ci == expectedCi && (not (effectiveCloudInit v) || length drives == length userDrives + 1)
        )
        (ssDbPool s)
  where
    filterMDrive vid ds =
      concat
        <$> mapM
          ( \d -> do
              image <- maybe (pure Nothing) get (driveDiskImageId d)
              let generated = maybe False (\x -> diskImageName x == "vm-" <> T.pack (show (fromSqlKey vid)) <> "-" <> avName v <> "-cloud-init" && diskImageEphemeral x && diskImageFormat x == FormatRaw && isNothing (diskImageBackingImageId x) && driveMedia d == Just MediaCdrom && driveInterface d == InterfaceIde && driveReadOnly d && driveCacheType d == CacheNone && not (driveDiscard d)) image
              pure [d | not generated]
          )
          ds

matchesTemplate :: ServerState -> TemplateYaml -> Int64 -> IO (Either Text Bool)
matchesTemplate s t i =
  runSqlPool
    ( do
        let tid = toSqlKey i
        old <- get tid
        sources <- mapM (traverse resolveImage . tdyDiskImage) (tyDrives t)
        keys <- mapM (getBy . UniqueSshKeyName . tkyName) (tySshKeys t)
        drives <- map entityVal <$> selectList [TemplateDriveTemplateId ==. tid] [Asc TemplateDriveId]
        nics <- map entityVal <$> selectList [TemplateNetworkInterfaceTemplateId ==. tid] [Asc TemplateNetworkInterfaceId]
        dirs <- map entityVal <$> selectList [TemplateSharedDirTemplateId ==. tid] [Asc TemplateSharedDirTag]
        audio <- map entityVal <$> selectList [TemplateAudioDeviceTemplateId ==. tid] [Asc TemplateAudioDeviceId]
        actualKeys <- map (fromSqlKey . templateSshKeySshKeyId . entityVal) <$> selectList [TemplateSshKeyTemplateId ==. tid] []
        ci <- fmap entityVal <$> getBy (UniqueTemplateCloudInitVm tid)
        let expectedDrive d = TemplateDrive tid (case tdyDiskImage d of Just (ImageId x) -> Just (toSqlKey x); _ -> Nothing) (case tdyDiskImage d of Just (ImageTag n _) -> Just n; _ -> tdyDiskName d) (case tdyDiskImage d of Just (ImageTag _ tag) -> Just tag; _ -> Nothing) (tdyInterface d) (tdyMedia d) (fromMaybe False (tdyReadOnly d)) (fromMaybe CacheNone (tdyCacheType d)) (fromMaybe False (tdyDiscard d)) (tdyStrategy d) (tdySize d) (tdyFormat d) (tdyEphemeral d)
            expectedNics = [TemplateNetworkInterface tid (case tnyNetwork n of Just name | not (T.null name) -> NetManaged; _ -> tnyType n) (tnyModel n) (tnyHostDevice n) (tnyNetwork n) | n <- tyNetworkInterfaces t]
            expectedDirs = L.sortOn templateSharedDirTag [TemplateSharedDir tid (tsdyPath d) (tsdyTag d) (tsdyCache d) (tsdyReadOnly d) | d <- tySharedDirs t]
            expectedAudio = [TemplateAudioDevice tid (tadyBackend a) (tadyModel a) (tadyOptions a) | a <- tyAudioDevices t]
            expectedCi = (\c -> TemplateCloudInit tid (cicyUserData c) (cicyNetworkConfig c) (cicyInjectSshKeys c)) <$> tyCloudInitConfig t
            scalar o = o == TemplateVm (tyName t) (tyCpuCount t) (tyRam t) (tyDescription t) (tyHeadless t) (tyCloudInit t) (tyGuestAgent t) (tyTpm t) (tyAutostart t) (tyRebootQuirk t) (templateVmCreatedAt o) (tyGraphicsAdapter t) (tyVsock t) (tyBalloon t) (tyRng t) (tyStartupTimeoutSec t) (tyHidden t)
        pure $ if elem (Just Nothing) sources || elem Nothing keys then Left "Template dependency not found" else Right $ maybe False scalar old && drives == map expectedDrive (tyDrives t) && nics == expectedNics && dirs == expectedDirs && audio == expectedAudio && ci == expectedCi && L.sort actualKeys == L.sort [fromSqlKey (entityKey k) | Just k <- keys]
    )
    (ssDbPool s)
