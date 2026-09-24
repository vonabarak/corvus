-- | Public compatibility façade for VM handlers.
module Corvus.Handlers.Vm
  ( VmCreate (..)
  , VmDelete (..)
  , VmEdit (..)
  , VmPause (..)
  , VmReset (..)
  , VmSave (..)
  , VmStart (..)
  , VmStop (..)
  , handleVmList
  , handleVmShow
  , getVmDetails
  , attachVmMonitor
  , reattachVmMonitors
  , handleVmCloudInit
  , handleSerialConsole
  , handleSerialConsoleFlush
  , handleHmpMonitor
  , handleHmpMonitorFlush
  , handleVmSendCtrlAltDel
  , handleVmViewGrant
  , generateSpicePassword
  , getVmWithStatus
  , getVmStatusOnly
  , setVmStatus
  , setVmError
  , setVmStarted
  , setVmStopped
  , claimVmReset
  , claimVmStart
  , completeVmReset
  , setVmErrorIfCurrent
  , setVmSpicePortIfCurrent
  , setVmStartedIfCurrent
  , setVmVsockCidIfCurrent
  , hasNetdMediatedNetIf
  )
where

import Corvus.Handlers.Vm.CloudInit (handleVmCloudInit)
import Corvus.Handlers.Vm.Configure (VmCreate (..), VmEdit (..))
import Corvus.Handlers.Vm.Console
  ( generateSpicePassword
  , handleHmpMonitor
  , handleHmpMonitorFlush
  , handleSerialConsole
  , handleSerialConsoleFlush
  , handleVmSendCtrlAltDel
  , handleVmViewGrant
  )
import Corvus.Handlers.Vm.Db
  ( claimVmReset
  , claimVmStart
  , completeVmReset
  , getVmStatusOnly
  , getVmWithStatus
  , hasNetdMediatedNetIf
  , setVmError
  , setVmErrorIfCurrent
  , setVmSpicePortIfCurrent
  , setVmStarted
  , setVmStartedIfCurrent
  , setVmStatus
  , setVmStopped
  , setVmVsockCidIfCurrent
  )
import Corvus.Handlers.Vm.Delete (VmDelete (..))
import Corvus.Handlers.Vm.Lifecycle (VmPause (..), VmReset (..), VmSave (..), VmStop (..))
import Corvus.Handlers.Vm.Monitor (attachVmMonitor, reattachVmMonitors)
import Corvus.Handlers.Vm.Query (getVmDetails, handleVmList, handleVmShow)
import Corvus.Handlers.Vm.Start (VmStart (..), autostartVmsOnNode)
