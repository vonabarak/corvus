-- | Shared VM-spec records used on both sides of the
-- @corvus-nodeagent@ Cap'n Proto wire.
--
-- The daemon (in "Corvus.NodeAgentClient.Spec") assembles a
-- 'VmSpec' from DB rows, encodes it to the wire, and ships it
-- across in 'vmStart'. The agent (in "Corvus.Node.Caps.Session")
-- decodes it back and hands it to
-- 'Corvus.Node.Command.buildQemuCommandFromSpec' to assemble the
-- QEMU argv.
--
-- Keeping the types here (rather than in 'Corvus.NodeAgentClient'
-- alongside the client functions) lets the agent depend on the
-- spec without pulling in client-only modules like
-- 'Network.Socket' or 'Capnp.Rpc'.
module Corvus.Node.VmSpec
  ( VmSpec (..)
  , VmDriveSpec (..)
  , VmNetIfSpec (..)
  , VmSharedDirSpec (..)
  , VmRuntimeInfo (..)
  , VmStopResult (..)
  , VmStopKind (..)
  , VmAgentStatus (..)
  , VmAgentState (..)
  , VmGuestExecReq (..)
  , VmGuestExecInfo (..)
  )
where

import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import Data.Word (Word32)

data VmSpec = VmSpec
  { vsVmId :: !Int64
  , vsName :: !T.Text
  , vsCpuCount :: !Int32
  , vsRamMb :: !Int32
  , vsHeadless :: !Bool
  , vsGuestAgent :: !Bool
  , vsVsockCid :: !(Maybe Word32)
  , vsSpicePort :: !(Maybe Int32)
  , vsDrives :: ![VmDriveSpec]
  , vsNetIfs :: ![VmNetIfSpec]
  , vsSharedDirs :: ![VmSharedDirSpec]
  , vsWaitForGuestAgentMs :: !Word32
  }
  deriving (Eq, Show)

data VmDriveSpec = VmDriveSpec
  { vdsDiskFilePath :: !T.Text
  , vdsFormat :: !T.Text
  , vdsIfKind :: !T.Text
  , vdsMedia :: !T.Text
  , vdsReadOnly :: !Bool
  , vdsCache :: !T.Text
  , vdsDiscard :: !Bool
  }
  deriving (Eq, Show)

data VmNetIfSpec = VmNetIfSpec
  { vnsIfType :: !T.Text
  , vnsHostDevice :: !T.Text
  , vnsMacAddress :: !T.Text
  }
  deriving (Eq, Show)

data VmSharedDirSpec = VmSharedDirSpec
  { vssHostPath :: !T.Text
  , vssTag :: !T.Text
  , vssCache :: !T.Text
  , vssReadOnly :: !Bool
  }
  deriving (Eq, Show)

data VmRuntimeInfo = VmRuntimeInfo
  { vriQemuPid :: !Int32
  , vriVirtiofsdPids :: ![Int32]
  , vriSpicePort :: !Int32
  }
  deriving (Eq, Show)

data VmStopKind
  = VmStopStopped
  | VmStopAlreadyStopped
  | VmStopTimeout
  | VmStopFailed
  deriving (Eq, Show)

data VmStopResult = VmStopResult
  { vsrKind :: !VmStopKind
  , vsrMessage :: !T.Text
  }
  deriving (Eq, Show)

data VmAgentState
  = VmAgentRunning
  | VmAgentStopped
  | VmAgentErrored
  | VmAgentUnknown
  deriving (Eq, Show)

data VmAgentStatus = VmAgentStatus
  { vasState :: !VmAgentState
  , vasQemuPid :: !Int32
  , vasLastExitCode :: !Int32
  }
  deriving (Eq, Show)

data VmGuestExecReq = VmGuestExecReq
  { vgeVmId :: !Int64
  , vgePath :: !T.Text
  , vgeArgs :: ![T.Text]
  , vgeCaptureOutput :: !Bool
  , vgeInputData :: !BS.ByteString
  , vgeTimeoutSec :: !Word32
  }
  deriving (Eq, Show)

data VmGuestExecInfo = VmGuestExecInfo
  { vgiExitCode :: !Int32
  , vgiHasExit :: !Bool
  , vgiSignal :: !Int32
  , vgiStdout :: !BS.ByteString
  , vgiStderr :: !BS.ByteString
  }
  deriving (Eq, Show)
