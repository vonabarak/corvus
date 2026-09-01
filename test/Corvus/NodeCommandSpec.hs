{-# LANGUAGE OverloadedStrings #-}

module Corvus.NodeCommandSpec (spec) where

import Corvus.Node.Command (buildQemuCommandFromSpec)
import Corvus.Node.VmSpec (VmSpec (..))
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

spec :: Spec
spec = describe "buildQemuCommandFromSpec TPM arguments" $ do
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
