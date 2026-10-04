{-# LANGUAGE OverloadedStrings #-}

module Corvus.NodeCommandSpec (spec) where

import Corvus.Model (GraphicsAdapter (..))
import Corvus.Node.Command (buildQemuCommandFromSpec)
import Corvus.Node.VmSpec (VmAudioDeviceSpec (..), VmDriveSpec (..), VmSpec (..))
import Corvus.Qemu.Config (defaultQemuConfig)
import Test.Hspec

baseSpec :: VmSpec
baseSpec =
  VmSpec
    { vsVmId = 7
    , vsLifecycleRevision = 1
    , vsRuntimeGeneration = 1
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
    , vsAudioDevices = []
    , vsWaitForGuestAgentMs = 0
    , vsRebootQuirk = False
    , vsSpiceBindAddr = "127.0.0.1"
    , vsLoadFromSavedState = False
    , vsStartPaused = False
    , vsCpuModel = "host"
    , vsGraphicsAdapter = GraphicsVirtioVga
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
    , vdsDiskFilePath = Just "/var/lib/corvus/data.qcow2"
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
    , vdsDiskFilePath = Just "/var/lib/corvus/installer.iso"
    , vdsFormat = "raw"
    , vdsIfKind = "scsi"
    , vdsMedia = "cdrom"
    , vdsReadOnly = True
    , vdsCache = "none"
    , vdsDiscard = False
    }

spec :: Spec
spec = describe "buildQemuCommandFromSpec" $ do
  it "uses the selected graphics device with SPICE" $ do
    let args = qemuArgs baseSpec {vsHeadless = False, vsSpicePort = Just 5901, vsGraphicsAdapter = GraphicsQxlVga}
    args `shouldContain` ["-vga", "none", "-device", "qxl-vga"]

  it "enables EGL headless for a GL graphics device" $ do
    let args = qemuArgs baseSpec {vsHeadless = False, vsSpicePort = Just 5901, vsGraphicsAdapter = GraphicsVirtioVgaGl}
    args `shouldContain` ["-device", "virtio-vga-gl"]
    args `shouldContain` ["-display", "egl-headless"]

  it "ignores the selected graphics device for a headless VM" $ do
    let args = qemuArgs baseSpec {vsGraphicsAdapter = GraphicsQxlVga}
    args `shouldNotContain` ["-device", "qxl-vga"]

  it "creates independent duplex cards for each audio backend" $ do
    let devices =
          [ VmAudioDeviceSpec 11 "pulse" "virtio-sound" "server=192.0.2.10,out.name=sink,in.name=mic"
          , VmAudioDeviceSpec 12 "pipewire" "ich9-intel-hda" "out.name=desk,in.name=desk-mic"
          , VmAudioDeviceSpec 13 "spice" "AC97" ""
          ]
        args = qemuArgs baseSpec {vsAudioDevices = devices, vsHeadless = False, vsSpicePort = Just 5901}
    args `shouldContain` ["-audiodev", "pa,id=audio11,server=192.0.2.10,out.name=sink,in.name=mic"]
    args `shouldContain` ["-audiodev", "pipewire,id=audio12,out.name=desk,in.name=desk-mic"]
    args `shouldContain` ["-audiodev", "spice,id=audio13"]
    args `shouldContain` ["-device", "virtio-sound-pci,audiodev=audio11"]
    args `shouldContain` ["-device", "hda-micro,bus=hda12.0,audiodev=audio12"]
    args `shouldContain` ["-device", "AC97,audiodev=audio13"]

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
      `shouldContain` [ "-drive"
                      , "id=drive-43,if=none,file=/var/lib/corvus/installer.iso,format=raw,media=cdrom,readonly=on"
                      , "-device"
                      , "scsi-cd,id=device-43,drive=drive-43,bus=scsi0.0"
                      ]

  it "emits IDE CD-ROM drives as legacy -drive lines carrying an ejectable id" $ do
    let ideCdrom =
          scsiDrive
            { vdsDriveId = 44
            , vdsIfKind = "ide"
            }
        args = qemuArgs baseSpec {vsDrives = [ideCdrom]}
    args
      `shouldContain` [ "-drive"
                      , "id=drive-44,if=ide,file=/var/lib/corvus/installer.iso,format=raw,media=cdrom,readonly=on"
                      ]

  it "omits file and format for a CD-ROM drive whose tray is empty" $ do
    let emptyTray = scsiDrive {vdsDiskFilePath = Nothing}
        args = qemuArgs baseSpec {vsDrives = [emptyTray]}
    args
      `shouldContain` [ "-drive"
                      , "id=drive-43,if=none,media=cdrom,readonly=on"
                      , "-device"
                      , "scsi-cd,id=device-43,drive=drive-43,bus=scsi0.0"
                      ]
