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
  , guestExecWithTimeout
  , guestReadFile
  , guestWriteFile
  , guestExecWithStdin
  , guestExecStream
  , ChunkSink
  , guestHealthProbe
  , guestPing
  , guestShutdown
  , guestFsFreeze
  , guestFsThaw
  , guestSetTime
  , guestSshAddAuthorizedKeys
  , parseGuestInterfaces
  , splitLines
  , pollStatus
  , pollRecvTimeoutMicros
  )
where

import Corvus.Node.GuestAgent.Connection (GuestAgentConns, releaseConn)
import Corvus.Node.GuestAgent.Control (guestFsFreeze, guestFsThaw, guestHealthProbe, guestPing, guestSetTime, guestShutdown, guestSshAddAuthorizedKeys)
import Corvus.Node.GuestAgent.Exec (guestExecWithStdin, guestExecWithTimeout, pollRecvTimeoutMicros, pollStatus, splitLines)
import Corvus.Node.GuestAgent.Exec.Stream (guestExecStream)
import Corvus.Node.GuestAgent.File (guestReadFile, guestWriteFile)
import Corvus.Node.GuestAgent.Network (parseGuestInterfaces)
import Corvus.Node.GuestAgent.Types (ChunkSink, GuestExecResult (..), GuestIpAddress (..), GuestNetIf (..))
