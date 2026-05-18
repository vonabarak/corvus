{-# LANGUAGE OverloadedStrings #-}

-- | DB-row → wire-spec translators for `corvus-nodeagent`.
--
-- The daemon owns the DB; the agent doesn't. So before the
-- daemon can ask the agent to do anything VM-shaped, it has to
-- walk the DB and pack everything into a 'VS.VmSpec'.
--
-- This module is that walker. It also performs the netd-side
-- TAP allocation for managed network interfaces — the daemon
-- (not the agent) talks to netd, so the spec carries
-- already-resolved TAP names by the time it crosses to the
-- agent.
module Corvus.NodeAgentClient.Spec
  ( assembleVmSpec
  )
where

import Corvus.Model
  ( Drive (..)
  , DriveInterface (..)
  , EnumText (..)
  , NetInterfaceType (..)
  , NetworkInterface (..)
  , SharedDir (..)
  , Vm (..)
  , VmId
  )
import qualified Corvus.Model as M
import qualified Corvus.NetAgentClient as NA
import qualified Corvus.NetAgentClient.Spec as NS
import qualified Corvus.Node.VmSpec as VS
import Corvus.Qemu.Config (QemuConfig, getEffectiveBasePath)
import Data.Int (Int32, Int64)
import Data.Pool (Pool)
import qualified Data.Text as T
import Data.Word (Word32)
import Database.Persist (Entity (..), get, selectList, (==.))
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)
import qualified Database.Persist.Sql
import System.FilePath ((</>))
import System.Posix.User (getEffectiveGroupID, getEffectiveUserID)

-- | Walk the DB for a VM and pack everything the agent needs to
-- spawn QEMU into a 'VmSpec'.
--
-- For each managed NIC, this also calls @netd.applyTap@ to get
-- the persistent TAP name and writes it into 'vnsHostDevice', so
-- the agent never has to talk to netd directly. The agent's
-- 'vmStart' simply takes the resolved spec and feeds it to
-- 'buildQemuCommandFromSpec'.
--
-- @waitMs@ is the per-spec ping-after-start timeout the agent
-- will use when @vm.guestAgent@ is true (0 to skip; daemon
-- decides the default — typically the @qcHealthcheckInterval@ ×
-- some factor).
assembleVmSpec
  :: Pool SqlBackend
  -> QemuConfig
  -> Maybe NA.NetAgentClient
  -> Int64
  -> Word32
  -> IO (Maybe VS.VmSpec)
assembleVmSpec pool config mNetAgent vmId waitMs = do
  let vmKey = toSqlKey vmId :: VmId
  mVm <- runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure Nothing
    Just vm -> do
      basePath <- getEffectiveBasePath config
      (drives, netIfs, sharedDirs) <-
        runSqlPool
          ( do
              ds <- selectList [M.DriveVmId ==. vmKey] []
              nIfs <- selectList [M.NetworkInterfaceVmId ==. vmKey] []
              sds <- selectList [M.SharedDirVmId ==. vmKey] []
              dimg <- mapM fetchDriveWithImage ds
              pure (dimg, nIfs, sds)
          )
          pool
      -- For each managed NIC, ask netd to allocate a persistent
      -- TAP and substitute the resolved ifname.
      resolvedNetIfs <- mapM (resolveNetIf mNetAgent) netIfs
      let driveSpecs =
            [ encodeDriveSpec basePath drive di
            | (drive, Just di) <- drives
            ]
          netIfSpecs = map encodeNetIfSpec resolvedNetIfs
          sharedDirSpecs = map (encodeSharedDirSpec . entityVal) sharedDirs
          spec =
            VS.VmSpec
              { VS.vsVmId = vmId
              , VS.vsName = vmName vm
              , VS.vsCpuCount = fromIntegral (vmCpuCount vm) :: Int32
              , VS.vsRamMb = fromIntegral (vmRamMb vm) :: Int32
              , VS.vsHeadless = vmHeadless vm
              , VS.vsGuestAgent = vmGuestAgent vm
              , VS.vsVsockCid = fmap fromIntegral (vmVsockCid vm)
              , VS.vsSpicePort = fmap fromIntegral (vmSpicePort vm)
              , VS.vsDrives = driveSpecs
              , VS.vsNetIfs = netIfSpecs
              , VS.vsSharedDirs = sharedDirSpecs
              , VS.vsWaitForGuestAgentMs =
                  if vmGuestAgent vm then waitMs else 0
              }
      pure (Just spec)

-- | Load the 'DiskImage' row each 'Drive' references.
fetchDriveWithImage
  :: Entity Drive
  -> Database.Persist.Sql.SqlPersistT IO (Drive, Maybe M.DiskImage)
fetchDriveWithImage (Entity _ drive) = do
  mImg <- get (driveDiskImageId drive)
  pure (drive, mImg)

-- | Encode a (Drive, DiskImage) pair as 'VS.VmDriveSpec'. The
-- resulting @vdsDiskFilePath@ is an absolute host path (relative
-- DB paths are resolved against @basePath@).
encodeDriveSpec :: FilePath -> Drive -> M.DiskImage -> VS.VmDriveSpec
encodeDriveSpec basePath drive img =
  let raw = T.unpack (M.diskImageFilePath img)
      absPath =
        if take 1 raw == "/"
          then raw
          else basePath </> raw
   in VS.VmDriveSpec
        { VS.vdsDiskFilePath = T.pack absPath
        , VS.vdsFormat = enumToText (M.diskImageFormat img)
        , VS.vdsIfKind = ifKindFor (driveInterface drive)
        , VS.vdsMedia = maybe "" enumToText (driveMedia drive)
        , VS.vdsReadOnly = driveReadOnly drive
        , VS.vdsCache = enumToText (driveCacheType drive)
        , VS.vdsDiscard = driveDiscard drive
        }

ifKindFor :: DriveInterface -> T.Text
ifKindFor InterfacePflash = "pflash"
ifKindFor InterfaceFloppy = "floppy"
ifKindFor iface = enumToText iface

-- | Encode a 'NetworkInterface' row as 'VS.VmNetIfSpec'.
-- 'networkInterfaceHostDevice' is already resolved for managed
-- NICs (see 'resolveNetIf').
encodeNetIfSpec :: NetworkInterface -> VS.VmNetIfSpec
encodeNetIfSpec n =
  VS.VmNetIfSpec
    { VS.vnsIfType = ifTypeText (networkInterfaceInterfaceType n)
    , VS.vnsHostDevice = networkInterfaceHostDevice n
    , VS.vnsMacAddress = networkInterfaceMacAddress n
    }

ifTypeText :: NetInterfaceType -> T.Text
ifTypeText NetUser = "user"
ifTypeText NetTap = "tap"
ifTypeText NetBridge = "bridge"
ifTypeText NetMacvtap = "macvtap"
ifTypeText NetVde = "vde"
ifTypeText NetManaged = "managed"

encodeSharedDirSpec :: SharedDir -> VS.VmSharedDirSpec
encodeSharedDirSpec d =
  VS.VmSharedDirSpec
    { VS.vssHostPath = sharedDirPath d
    , VS.vssTag = sharedDirTag d
    , VS.vssCache = enumToText (sharedDirCache d)
    , VS.vssReadOnly = sharedDirReadOnly d
    }

-- | Resolve a 'NetworkInterface' for transit on the wire.
-- Managed NICs get a persistent TAP allocated via netd (the agent
-- never sees a 'NetManaged' NIC with empty 'hostDevice'); other
-- types pass through unchanged.
resolveNetIf
  :: Maybe NA.NetAgentClient -> Entity NetworkInterface -> IO NetworkInterface
resolveNetIf mAgent (Entity ifaceKey netIf) =
  case (networkInterfaceNetworkId netIf, mAgent) of
    (Just nwKey, Just nac) -> do
      uid <- getEffectiveUserID
      gid <- getEffectiveGroupID
      let bridge = NS.corvusBridgeName (fromSqlKey nwKey)
          tapName = NS.corvusTapName (fromSqlKey ifaceKey)
          spec =
            NA.TapSpec
              { NA.tsName = tapName
              , NA.tsBridge = bridge
              , NA.tsUid = fromIntegral uid
              , NA.tsGid = fromIntegral gid
              }
      result <- NA.applyTap nac spec
      case result of
        Right _ -> pure (netIf {networkInterfaceHostDevice = tapName})
        Left _ -> pure netIf
    _ -> pure netIf
