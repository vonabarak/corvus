{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | QEMU command line generation.
--
-- Builds the complete QEMU argv the nodeagent feeds to
-- 'createProcess' from a wire-level 'Corvus.Node.VmSpec.VmSpec'
-- record. The daemon pre-resolves per-node disk paths and managed
-- TAP names before sending the spec, so this module touches
-- neither the DB nor netd.
module Corvus.Node.Command
  ( buildQemuCommandFromSpec
  )
where

import Corvus.Node.Runtime (shellQuotePath)
import qualified Corvus.Node.VmSpec as VS
import Corvus.Qemu.Config (QemuConfig (..))
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- VM-abstraction command builder
--------------------------------------------------------------------------------

-- | Build the QEMU argv from a wire-level 'VS.VmSpec'. The
-- daemon resolved per-node disk paths and managed-NIC TAP names
-- before sending the spec, so the builder is pure: no DB lookups,
-- no netd calls.
--
-- The agent feeds the result straight to 'createProcess'.
buildQemuCommandFromSpec
  :: QemuConfig
  -> VS.VmSpec
  -> FilePath
  -- ^ monitor socket
  -> FilePath
  -- ^ QMP socket
  -> FilePath
  -- ^ serial socket
  -> FilePath
  -- ^ guest-agent socket
  -> FilePath
  -- ^ VM runtime dir (for virtiofsd chardev paths)
  -> FilePath
  -- ^ saved-state file path (used as @-incoming \"file:…\"@ when
  -- @vsLoadFromSavedState@ is set; ignored otherwise — pass any
  -- valid string for a cold boot)
  -> (FilePath, [String])
buildQemuCommandFromSpec QemuConfig {..} spec monitorSock qmpSock serialSock guestAgentSock vmRuntimeDir savedStateFile =
  ( qcQemuBinary
  , concatMap
      (filter (not . null))
      [ ["-name", T.unpack (VS.vsName spec) ++ ",process=corvus-vm-" ++ show (VS.vsVmId spec)]
      , ["-machine", "type=q35,accel=kvm"]
      , ["-cpu", T.unpack (VS.vsCpuModel spec)]
      , ["-enable-kvm"]
      , memoryArgs
      , ["-smp", show (VS.vsCpuCount spec)]
      , hotplugPortArgs
      , vsockArgs
      , guestAgentArgs
      , displayArgs
      , monitorArgs
      , concatMap driveArgsSpec (zip [0 ..] (VS.vsDrives spec))
      , concatMap netArgsSpec (zip [0 ..] (VS.vsNetIfs spec))
      , concatMap sharedDirArgsSpec (zip [0 ..] (VS.vsSharedDirs spec))
      , -- Prefer CD-ROM boot when one is attached; see the
        -- matching block in `buildCommandWithSockets`.
        if any (\d -> VS.vdsMedia d == "cdrom") (VS.vsDrives spec)
          then ["-boot", "order=dc"]
          else []
      , -- Reboot-quirk: make QEMU exit on guest-initiated
        -- reboot instead of resetting in place. The agent's
        -- reaper notices the exit and re-spawns QEMU
        -- transparently, so the guest sees a "reboot" but the
        -- firmware sees a cold boot — works around the OVMF
        -- second-boot hang (tianocore/edk2#12441).
        ["-no-reboot" | VS.vsRebootQuirk spec]
      , -- Resume from a previously-saved RAM image. QEMU starts
        -- in @postmigrate@ state once the migration stream is
        -- consumed; the agent's post-spawn coordinator issues
        -- QMP @cont@ once @query-migrate@ reports completion,
        -- then unlinks the state file. The @exec:zstdcat@ URI
        -- matches the @exec:zstd@ used by
        -- 'Corvus.Node.Qmp.qmpMigrate' on the save side — the
        -- state file is a zstd-compressed migration stream.
        if VS.vsLoadFromSavedState spec
          then ["-incoming", "exec:zstdcat " ++ shellQuotePath savedStateFile]
          else []
      ]
  )
  where
    memSize = fromMaybe (show (VS.vsRamMb spec) ++ "M") qcSharedMemSize
    memoryArgs
      | null (VS.vsSharedDirs spec) = ["-m", show (VS.vsRamMb spec)]
      | otherwise =
          [ "-m"
          , show (VS.vsRamMb spec)
          , "-object"
          , "memory-backend-memfd,id=mem,size=" ++ memSize ++ ",share=on"
          , "-numa"
          , "node,memdev=mem"
          ]

    hotplugPortArgs =
      [ "-device"
      , "pcie-root-port,id=hotplug-rp,chassis=0,slot=0"
      , "-device"
      , "pcie-pci-bridge,id=hotplug,bus=hotplug-rp"
      ]

    vsockArgs = case VS.vsVsockCid spec of
      Just cid ->
        [ "-device"
        , "vhost-vsock-pci,guest-cid=" ++ show cid ++ ",id=vsock0"
        ]
      Nothing -> []

    guestAgentArgs =
      [ "-device"
      , "virtio-serial,id=virtio-serial0"
      , "-chardev"
      , "socket,id=qga0,path=" ++ guestAgentSock ++ ",server=on,wait=off"
      , "-device"
      , "virtserialport,chardev=qga0,name=org.qemu.guest_agent.0"
      ]

    displayArgs
      | VS.vsHeadless spec = serialConsoleArgs
      | otherwise = spiceArgs ++ usbRedirArgs

    spicePort = fromMaybe 0 (VS.vsSpicePort spec)

    serialConsoleArgs =
      [ "-chardev"
      , "socket,id=serial0,path=" ++ serialSock ++ ",server=on,wait=off"
      , "-serial"
      , "chardev:serial0"
      , "-display"
      , "none"
      , "-vga"
      , "none"
      ]

    spiceArgs =
      [ "-spice"
      , "addr="
          ++ T.unpack (VS.vsSpiceBindAddr spec)
          ++ ",port="
          ++ show spicePort
          ++ ",disable-ticketing=off"
      , "-chardev"
      , "spicevmc,id=vdagent,name=vdagent"
      , "-device"
      , "virtio-vga"
      , "-device"
      , "virtserialport,chardev=vdagent,name=com.redhat.spice.0"
      ]

    usbRedirArgs =
      [ "-device"
      , "nec-usb-xhci,id=xhci"
      , "-chardev"
      , "spicevmc,id=usbredirchardev1,name=usbredir"
      , "-device"
      , "usb-redir,chardev=usbredirchardev1,id=usbredirdev1"
      , "-chardev"
      , "spicevmc,id=usbredirchardev2,name=usbredir"
      , "-device"
      , "usb-redir,chardev=usbredirchardev2,id=usbredirdev2"
      , "-chardev"
      , "spicevmc,id=usbredirchardev3,name=usbredir"
      , "-device"
      , "usb-redir,chardev=usbredirchardev3,id=usbredirdev3"
      ]

    monitorArgs =
      [ "-monitor"
      , "unix:" ++ monitorSock ++ ",server,nowait"
      , "-qmp"
      , "unix:" ++ qmpSock ++ ",server,nowait"
      ]

    sharedDirArgsSpec :: (Int, VS.VmSharedDirSpec) -> [String]
    sharedDirArgsSpec (idx, d) =
      let charId = "virtiofs" ++ show idx
          tag = T.unpack (VS.vssTag d)
          socketPath = vmRuntimeDir </> "virtiofsd-" ++ tag ++ ".sock"
       in [ "-chardev"
          , "socket,id=" ++ charId ++ ",path=" ++ socketPath
          , "-device"
          , "vhost-user-fs-pci,chardev=" ++ charId ++ ",tag=" ++ tag
          ]

-- | Per-drive argv assembly for 'buildQemuCommandFromSpec'.
-- The wire-level 'VS.VmDriveSpec' already carries an absolute
-- 'vdsDiskFilePath' — the daemon resolved it from the DB.
driveArgsSpec :: (Int, VS.VmDriveSpec) -> [String]
driveArgsSpec (_idx, d) =
  let ifKind = T.unpack (VS.vdsIfKind d)
      readOnlyFlag =
        if VS.vdsReadOnly d then Just "readonly=on" else Nothing
      formatStr = T.unpack (VS.vdsFormat d)
   in case ifKind of
        "pflash" ->
          [ "-drive"
          , intercalate "," $
              catMaybes
                [ Just $ "file=" ++ T.unpack (VS.vdsDiskFilePath d)
                , Just $ "format=" ++ formatStr
                , Just "if=pflash"
                , readOnlyFlag
                ]
          ]
        "floppy" ->
          [ "-drive"
          , intercalate "," $
              catMaybes
                [ Just $ "file=" ++ T.unpack (VS.vdsDiskFilePath d)
                , Just $ "format=" ++ formatStr
                , Just "if=floppy"
                , readOnlyFlag
                ]
          ]
        _ ->
          [ "-drive"
          , intercalate "," $
              catMaybes
                [ Just $ "file=" ++ T.unpack (VS.vdsDiskFilePath d)
                , Just $ "format=" ++ formatStr
                , Just $ "if=" ++ ifForQemu ifKind
                , case T.unpack (VS.vdsMedia d) of
                    "" -> Nothing
                    m -> Just $ "media=" ++ m
                , Just $ "cache=" ++ T.unpack (VS.vdsCache d)
                , if VS.vdsDiscard d
                    then Just "discard=on"
                    else Just "discard=off"
                , readOnlyFlag
                ]
          ]
  where
    ifForQemu "nvme" = "none" -- NVMe uses -device instead
    ifForQemu other = other

-- | Per-NIC argv assembly for 'buildQemuCommandFromSpec'.
-- For managed and bridge NICs the daemon's @assembleVmSpec@
-- pre-resolves the TAP name via netd, so @vnsHostDevice@ is the
-- persistent TAP ifname by the time the agent sees the spec — no
-- agent ↔ netd cross-talk needed. Bridge NICs use the same TAP
-- shape as managed; the difference (which host bridge the TAP is
-- slaved to) is fully handled netd-side.
netArgsSpec :: (Int, VS.VmNetIfSpec) -> [String]
netArgsSpec (idx, n) =
  let netId = "net" ++ show idx
      hostDev = T.unpack (VS.vnsHostDevice n)
      mac = T.unpack (VS.vnsMacAddress n)
      netdev = case T.unpack (VS.vnsIfType n) of
        "user" ->
          let userOpts = if null hostDev then "" else "," ++ hostDev
           in ["-netdev", "user,id=" ++ netId ++ userOpts]
        "tap" ->
          ["-netdev", "tap,id=" ++ netId ++ ",ifname=" ++ hostDev ++ ",script=no,downscript=no"]
        "bridge" ->
          ["-netdev", "tap,id=" ++ netId ++ ",ifname=" ++ hostDev ++ ",script=no,downscript=no"]
        "macvtap" ->
          ["-netdev", "tap,id=" ++ netId ++ ",fd=3"]
        "vde" ->
          ["-netdev", "vde,id=" ++ netId ++ ",sock=" ++ hostDev]
        "managed" ->
          ["-netdev", "tap,id=" ++ netId ++ ",ifname=" ++ hostDev ++ ",script=no,downscript=no"]
        _ -> []
   in netdev ++ ["-device", "virtio-net-pci,netdev=" ++ netId ++ ",mac=" ++ mac]
