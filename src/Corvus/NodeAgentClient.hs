-- | Daemon-side Cap'n Proto client for @corvus-nodeagent@.
--
-- This umbrella preserves the established client API while the implementation
-- is organized by resource domain.
module Corvus.NodeAgentClient
  ( module Corvus.NodeAgentClient.Core
  , module Corvus.NodeAgentClient.Disk
  , module Corvus.NodeAgentClient.Snapshot
  , module Corvus.NodeAgentClient.CloudInit
  , VmSpec (..)
  , VmDriveSpec (..)
  , VmNetIfSpec (..)
  , VmSharedDirSpec (..)
  , VmRuntimeInfo (..)
  , VmStartOutcome (..)
  , VmStopResult (..)
  , VmStopKind (..)
  , VmAgentStatus (..)
  , VmAgentState (..)
  , VmGuestExecReq (..)
  , VmGuestExecInfo (..)
  , module Corvus.NodeAgentClient.Vm
  )
where

import Corvus.Node.VmSpec
  ( VmAgentState (..)
  , VmAgentStatus (..)
  , VmDriveSpec (..)
  , VmGuestExecInfo (..)
  , VmGuestExecReq (..)
  , VmNetIfSpec (..)
  , VmRuntimeInfo (..)
  , VmSharedDirSpec (..)
  , VmSpec (..)
  , VmStopKind (..)
  , VmStopResult (..)
  )
import Corvus.NodeAgentClient.CloudInit
import Corvus.NodeAgentClient.Core hiding (callOn, remote, remoteWithin)
import Corvus.NodeAgentClient.Disk hiding (decodeDiskOpResult)
import Corvus.NodeAgentClient.Snapshot
import Corvus.NodeAgentClient.Vm
