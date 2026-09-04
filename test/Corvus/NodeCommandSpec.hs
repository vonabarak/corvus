{-# LANGUAGE OverloadedStrings #-}

module Corvus.NodeCommandSpec (spec) where

import Corvus.Node.Command (buildQemuCommandFromSpec)
import Corvus.Node.VmSpec (VmDriveSpec (..), VmSpec (..))
import Corvus.Qemu.Config (defaultQemuConfig)
import Test.Hspec

baseSpec :: VmSpec
baseSpec =
  VmSpec
    { vsVmId = 7
    , vsName = "tpm-test"
    , vsCpuCount = 2
    , vsRamMb = 2048
    , vsHeadless = True
    , vsGuestAgent = False
    , vsTpm = False
    , vsVsockCid = Nothing
    , vsSpicePort = Nothing
    , vsDrives = []
    , vsNetIfs = []
    , vsSharedDirs = []
    , vsWaitForGuestAgentMs = 0
    , vsRebootQuirk = False
    , vsSpiceBindAddr = "127.0.0.1"
    , vsLoadFromSavedState = False
    , vsStartPaused = False
    , vsCpuModel = "host"
    }

qemuArgs :: VmSpec -> [String]
qemuArgs vm =
  snd $
    buildQemuCommandFromSpec
      defaultQemuConfig
      vm
      "/run/corvus/vms/7/monitor.sock"
      "/run/corvus/vms/7/qmp.sock"
      "/run/corvus/vms/7/serial.sock"
      "/run/corvus/vms/7/qga.sock"
      "/run/corvus/vms/7"
      "/var/lib/corvus/tpm-test/state.qemu.zst"

virtioDrive :: VmDriveSpec
virtioDrive =
  VmDriveSpec
    { vdsDriveId = 42
    , vdsDiskFilePath = "/var/lib/corvus/data.qcow2"
    , vdsFormat = "qcow2"
    , vdsIfKind = "virtio"
    , vdsMedia = "disk"
    , vdsReadOnly = False
    , vdsCache = "writeback"
    , vdsDiscard = True
    }

scsiDrive :: VmDriveSpec
scsiDrive =
  VmDriveSpec
    { vdsDriveId = 43
    , vdsDiskFilePath = "/var/lib/corvus/installer.iso"
    , vdsFormat = "raw"
    , vdsIfKind = "scsi"
    , vdsMedia = "cdrom"
    , vdsReadOnly = True
    , vdsCache = "none"
    , vdsDiscard = False
    }

spec :: Spec
spec = describe "buildQemuCommandFromSpec" $ do
  it "omits every TPM argument when TPM is disabled" $ do
    let args = qemuArgs baseSpec
    args `shouldNotContain` ["-tpmdev"]
    args `shouldNotContain` ["tpm-crb,tpmdev=tpm0"]

  it "attaches a TPM 2.0 CRB device to the swtpm socket when enabled" $ do
    let args = qemuArgs baseSpec {vsTpm = True}
    args
      `shouldContain` [ "-chardev"
                      , "socket,id=chrtpm,path=/run/corvus/vms/7/swtpm.sock"
                      , "-tpmdev"
                      , "emulator,id=tpm0,chardev=chrtpm"
                      , "-device"
                      , "tpm-crb,tpmdev=tpm0"
                      ]

  it "gives boot-time virtio drives a named backend on a hot-unplug-capable PCIe port" $ do
    qemuArgs baseSpec {vsDrives = [virtioDrive]}
      `shouldContain` [ "-blockdev"
                      , "driver=qcow2,node-name=drive-42,read-only=off,cache.direct=off,cache.no-flush=off,discard=unmap,file.driver=file,file.filename=/var/lib/corvus/data.qcow2,file.read-only=off"
                      , "-device"
                      , "virtio-blk-pci,id=device-42,drive=drive-42,bus=virtio-rp-42,write-cache=on,discard=on"
                      ]

  it "places boot-time SCSI drives on the stable hot-pluggable SCSI bus" $ do
    let args = qemuArgs baseSpec {vsDrives = [scsiDrive]}
    args `shouldContain` ["pcie-root-port,id=scsi-rp,chassis=1,slot=1"]
    args `shouldContain` ["virtio-scsi-pci,id=scsi0,bus=scsi-rp"]
    args
      `shouldContain` [ "-blockdev"
                      , "driver=raw,node-name=drive-43,read-only=on,cache.direct=on,cache.no-flush=on,discard=ignore,file.driver=file,file.filename=/var/lib/corvus/installer.iso,file.read-only=on"
                      , "-device"
                      , "scsi-cd,id=device-43,drive=drive-43,bus=scsi0.0,write-cache=on"
                      ]
