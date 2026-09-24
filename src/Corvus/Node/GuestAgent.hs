-- | Public QEMU Guest Agent API.
--
-- Command implementations are grouped by responsibility under
-- 'Corvus.Node.GuestAgent'. This module preserves the established API for
-- nodeagent callers and tests.
module Corvus.Node.GuestAgent
  ( GuestExecResult (..)
  , GuestIpAddress (..)
  , GuestNetIf (..)
  , GuestAgentConns
  , releaseConn
  , guestExec
  , guestExecWithTimeout
  , guestExecWithStdin
  , guestExecStream
  , ChunkSink
  , guestPing
  , guestShutdown
  , guestNetworkGetInterfaces
  , guestFsFreeze
  , guestFsThaw
  , guestSetTime
  , parseGuestInterfaces
  , splitLines
  , pollStatus
  , pollRecvTimeoutMicros
  )
where

import Corvus.Node.GuestAgent.Connection (GuestAgentConns, releaseConn)
import Corvus.Node.GuestAgent.Control (guestFsFreeze, guestFsThaw, guestPing, guestSetTime, guestShutdown)
import Corvus.Node.GuestAgent.Exec (guestExec, guestExecWithStdin, guestExecWithTimeout, pollRecvTimeoutMicros, pollStatus, splitLines)
import Corvus.Node.GuestAgent.Exec.Stream (guestExecStream)
import Corvus.Node.GuestAgent.Network (guestNetworkGetInterfaces, parseGuestInterfaces)
import Corvus.Node.GuestAgent.Types (ChunkSink, GuestExecResult (..), GuestIpAddress (..), GuestNetIf (..))
