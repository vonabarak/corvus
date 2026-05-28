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
import Corvus.Qemu.Config (QemuConfig (..), getEffectiveBasePath)
import Data.Int (Int32, Int64)
import Data.Pool (Pool)
import qualified Data.Text as T
import Data.Word (Word32)
import Database.Persist (Entity (..), get, selectList, (==.))
import qualified Database.Persist
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
  -> IO (Either T.Text VS.VmSpec)
assembleVmSpec pool config mNetAgent vmId waitMs = do
  let vmKey = toSqlKey vmId :: VmId
  mVm <- runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure (Left "VM disappeared from DB before assembleVmSpec")
    Just vm -> do
      basePath <- getEffectiveBasePath config
      let vmNode = vmNodeId vm
      (drives, netIfs, sharedDirs) <-
        runSqlPool
          ( do
              ds <- selectList [M.DriveVmId ==. vmKey] []
              nIfs <- selectList [M.NetworkInterfaceVmId ==. vmKey] []
              sds <- selectList [M.SharedDirVmId ==. vmKey] []
              -- For every drive: load both the DiskImage row and
              -- its per-node placement on this VM's node.
              dimg <- mapM (fetchDriveWithImage vmNode) ds
              pure (dimg, nIfs, sds)
          )
          pool
      -- For each managed/bridge NIC, ask netd to allocate a
      -- persistent TAP and substitute the resolved ifname. A
      -- failure here (netd down, bridge missing because the
      -- network isn't started, …) propagates as @Left@ so the
      -- caller surfaces a clean error instead of launching QEMU
      -- with @ifname=@ and getting EPERM from /dev/net/tun.
      resolvedE <- traverse (resolveNetIf mNetAgent) netIfs
      case sequence resolvedE of
        Left err -> pure (Left err)
        Right resolvedNetIfs -> do
          let driveSpecs =
                [ encodeDriveSpec basePath drive di placement
                | (drive, Just di, Just placement) <- drives
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
                  , VS.vsRebootQuirk = vmRebootQuirk vm
                  , VS.vsSpiceBindAddr = qcSpiceBindAddress config
                  , -- Whether the agent should QEMU with `-incoming` to
                    -- restore a saved RAM image. Flipped by the daemon's
                    -- start handler when the VM row is in `saved` status;
                    -- otherwise a cold boot.
                    VS.vsLoadFromSavedState = vmStatus vm == M.VmSaved
                  }
          pure (Right spec)

-- | Load the 'DiskImage' row and the per-node placement
-- ('DiskImageNode'.filePath) each 'Drive' references. The
-- placement is keyed on the VM's node; missing placements
-- surface as 'Nothing' (the same-node attach check should have
-- caught the inconsistency, so reaching here with 'Nothing'
-- means an operator hand-edited the DB).
fetchDriveWithImage
  :: M.NodeId
  -> Entity Drive
  -> Database.Persist.Sql.SqlPersistT IO (Drive, Maybe M.DiskImage, Maybe T.Text)
fetchDriveWithImage vmNode (Entity _ drive) = do
  mImg <- get (driveDiskImageId drive)
  mPath <-
    Database.Persist.getBy
      (M.UniqueDiskImageOnNode (driveDiskImageId drive) vmNode)
  let placementPath = fmap (M.diskImageNodeFilePath . entityVal) mPath
  pure (drive, mImg, placementPath)

-- | Encode a (Drive, DiskImage, placement-path) triple as
-- 'VS.VmDriveSpec'. The resulting @vdsDiskFilePath@ is an
-- absolute host path (relative DB paths are resolved against
-- @basePath@).
encodeDriveSpec :: FilePath -> Drive -> M.DiskImage -> T.Text -> VS.VmDriveSpec
encodeDriveSpec basePath drive img placement =
  let raw = T.unpack placement
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
--
-- For netd-mediated NICs — 'NetManaged' (attached to a Corvus
-- virtual network) and 'NetBridge' (attached to a user-managed
-- host bridge) — a persistent TAP is allocated via netd and the
-- TAP ifname is written into 'hostDevice' so the agent's QEMU
-- argv builder uses it verbatim. Other types pass through
-- unchanged.
--
-- The only difference between the two netd-mediated cases is the
-- bridge the TAP is slaved to: managed NICs use the Corvus-owned
-- @corvus-br-<base36>@ bridge; bridge NICs use the user-supplied
-- bridge name from 'hostDevice' (validated non-empty at NIC-add
-- time).
resolveNetIf
  :: Maybe NA.NetAgentClient
  -> Entity NetworkInterface
  -> IO (Either T.Text NetworkInterface)
resolveNetIf mAgent (Entity ifaceKey netIf) =
  case (mAgent, bridgeForNetdMediatedNic netIf) of
    (Just nac, Just bridgeName) -> applyOnNetd nac bridgeName
    (Nothing, Just _) ->
      pure $
        Left $
          "NIC "
            <> T.pack (show (fromSqlKey ifaceKey))
            <> " ("
            <> ifTypeText (networkInterfaceInterfaceType netIf)
            <> "): netd is required to allocate a TAP but the daemon "
            <> "has no live netd connection (is corvus-netd running?)"
    _ -> pure (Right netIf)
  where
    applyOnNetd nac bridgeName = do
      uid <- getEffectiveUserID
      gid <- getEffectiveGroupID
      let tapName = NS.corvusTapName (fromSqlKey ifaceKey)
          spec =
            NA.TapSpec
              { NA.tsName = tapName
              , NA.tsBridge = bridgeName
              , NA.tsUid = fromIntegral uid
              , NA.tsGid = fromIntegral gid
              }
      result <- NA.applyTap nac spec
      case result of
        Right _ ->
          pure $ Right (netIf {networkInterfaceHostDevice = tapName})
        Left e ->
          pure $
            Left $
              "NIC "
                <> T.pack (show (fromSqlKey ifaceKey))
                <> " ("
                <> ifTypeText (networkInterfaceInterfaceType netIf)
                <> "): netd applyTap to bridge "
                <> bridgeName
                <> " failed: "
                <> T.pack (show e)
                <> " (is the network started? `crv network list`)"

-- | Decide whether a NIC needs netd to pre-allocate a TAP, and if
-- so which host bridge to slave the TAP to. 'Nothing' means the
-- NIC is not netd-mediated (user/tap/macvtap/vde, or a bridge
-- with empty hostDevice — the latter should have been rejected
-- at add time).
bridgeForNetdMediatedNic :: NetworkInterface -> Maybe T.Text
bridgeForNetdMediatedNic netIf =
  case networkInterfaceInterfaceType netIf of
    NetManaged ->
      fmap (NS.corvusBridgeName . fromSqlKey) (networkInterfaceNetworkId netIf)
    NetBridge ->
      let hd = networkInterfaceHostDevice netIf
       in if T.null hd then Nothing else Just hd
    _ -> Nothing
